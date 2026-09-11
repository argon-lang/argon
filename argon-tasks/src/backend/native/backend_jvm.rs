use super::super::backend_jvm::{
    BackendJVM, JvmCodegenOptions, JvmPlatformMetadataOptions, display_error, report_error,
};
use alloc::{borrow::ToOwned, format, string::String, vec::Vec};
use argon_io::{InputFile, InputStream, OutputFile, OutputStream, Read, Write as AsyncWrite};
use embedded_io::Write;
use jni::{
    InitArgsBuilder, JNIVersion, JavaVM, jni_sig, jni_str,
    objects::{JByteArray, JObject, JValue},
};
use std::{path::PathBuf, sync::OnceLock};

static JVM: OnceLock<Result<JavaVM, String>> = OnceLock::new();
// Java 25 exposes JNI_VERSION_24 as its newest JNI invocation interface.
const JNI_VERSION_24: i32 = 0x0018_0000;

pub struct NativeBackendJVM {
    path: PathBuf,
}

impl NativeBackendJVM {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }

    fn jvm(&self) -> Result<&JavaVM, String> {
        JVM.get_or_init(|| {
            let path = self.path.canonicalize().map_err(display_error)?;
            let module_path = format!("--module-path={}", path.to_string_lossy());
            let args = InitArgsBuilder::new()
                .version(JNIVersion::new(JNI_VERSION_24))
                // The JVM and Rust compiler share this thread; the compiler needs more than 1 MiB.
                .option("-Xss8m")
                .option(&module_path)
                .option("--add-modules=dev.argon.backend")
                .build()
                .map_err(display_error)?;
            JavaVM::new(args).map_err(display_error)
        })
        .as_ref()
        .map_err(Clone::clone)
    }

    fn invoke_metadata(&self, files: &[(String, Vec<u8>)]) -> Result<Vec<u8>, String> {
        let file_count =
            i32::try_from(files.len()).map_err(|_| "too many JVM extern files".to_owned())?;
        self.jvm()?
            .attach_current_thread(|env| {
                let string_class = env.find_class(jni_str!("java/lang/String"))?;
                let byte_array_class = env.find_class(jni_str!("[B"))?;
                let names = env.new_object_array(file_count, string_class, JObject::null())?;
                let contents =
                    env.new_object_array(file_count, byte_array_class, JObject::null())?;
                for (index, (name, bytes)) in files.iter().enumerate() {
                    let name = env.new_string(name)?;
                    names.set_element(env, index, name)?;
                    let bytes = env.byte_array_from_slice(bytes)?;
                    contents.set_element(env, index, bytes)?;
                }
                let value = env
                    .call_static_method(
                        jni_str!("dev/argon/backend/bridge/BackendBridge"),
                        jni_str!("platformMetadata"),
                        jni_sig!("([Ljava/lang/String;[[B)[B"),
                        &[
                            JValue::Object(names.as_ref()),
                            JValue::Object(contents.as_ref()),
                        ],
                    )?
                    .l()?;
                let value = JByteArray::cast_local(env, value)?;
                env.convert_byte_array(value)
            })
            .map_err(display_error)
    }

    fn invoke_codegen(
        &self,
        name: &str,
        bytes: &[u8],
        executable: bool,
    ) -> Result<Vec<u8>, String> {
        self.jvm()?
            .attach_current_thread(|env| {
                let name = env.new_string(name)?;
                let bytes = env.byte_array_from_slice(bytes)?;
                let value = env
                    .call_static_method(
                        jni_str!("dev/argon/backend/bridge/BackendBridge"),
                        jni_str!("codegen"),
                        jni_sig!("(Ljava/lang/String;[BZ)[B"),
                        &[
                            JValue::Object(name.as_ref()),
                            JValue::Object(bytes.as_ref()),
                            JValue::Bool(executable.into()),
                        ],
                    )?
                    .l()?;
                let value = JByteArray::cast_local(env, value)?;
                env.convert_byte_array(value)
            })
            .map_err(display_error)
    }
}

impl BackendJVM for NativeBackendJVM {
    async fn platform_metadata<I, O, W>(
        &self,
        options: JvmPlatformMetadataOptions<I, O>,
        output: &mut W,
    ) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write,
    {
        let result = async {
            let mut files = Vec::with_capacity(options.extern_files.len());
            for file in &options.extern_files {
                let name = file
                    .path()
                    .file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or("input.class")
                    .to_owned();
                files.push((name, read_input(file).await?));
            }
            let bytes = self.invoke_metadata(&files)?;
            write_output(&options.output_file, &bytes).await
        }
        .await;
        finish(result, &options.output_file, output).await
    }

    async fn codegen_jvm<I, O, W>(&self, options: JvmCodegenOptions<I, O>, output: &mut W) -> bool
    where
        I: InputFile + 'static,
        I::Reader: 'static,
        O: OutputFile + 'static,
        O::Writer: 'static,
        W: Write,
    {
        let result = async {
            let name = options
                .input_file
                .path()
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("input.avm")
                .to_owned();
            let input = read_input(&options.input_file).await?;
            let bytes = self.invoke_codegen(&name, &input, options.executable)?;
            write_output(&options.output_file, &bytes).await
        }
        .await;
        finish(result, &options.output_file, output).await
    }
}

async fn read_input<I: InputFile>(file: &I) -> Result<Vec<u8>, String> {
    let mut reader = file.open().await.map_err(display_error)?;
    let mut bytes = Vec::new();
    let mut buffer = [0; 8192];
    loop {
        let count = reader.read(&mut buffer).await.map_err(display_error)?;
        if count == 0 {
            break;
        }
        bytes.extend_from_slice(&buffer[..count]);
    }
    reader.close().await.map_err(display_error)?;
    Ok(bytes)
}

async fn write_output<O: OutputFile>(file: &O, bytes: &[u8]) -> Result<(), String> {
    let mut writer = file.open().await.map_err(display_error)?;
    writer.write_all(bytes).await.map_err(display_error)?;
    writer.flush().await.map_err(display_error)?;
    writer.close().await.map_err(display_error)
}

async fn finish<O: OutputFile, W: Write>(
    result: Result<(), String>,
    file: &O,
    output: &mut W,
) -> bool {
    match result {
        Ok(()) => true,
        Err(error) => {
            let _ = file.delete().await;
            report_error(output, error)
        }
    }
}
