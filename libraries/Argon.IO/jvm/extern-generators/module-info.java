module dev.argon.libraryexterns.io {
	requires dev.argon.externcompiler;

	provides dev.argon.externcompiler.ExternGenerator
		with dev.argon.libraryexterns.IoExternGenerator;
}
