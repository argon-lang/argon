module dev.argon.libraryexterns.core {
	requires dev.argon.externcompiler;

	provides dev.argon.externcompiler.ExternGenerator
		with dev.argon.libraryexterns.CoreExternGenerator;
}
