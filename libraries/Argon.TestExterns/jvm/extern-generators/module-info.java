module dev.argon.libraryexterns.testexterns {
	requires dev.argon.externcompiler;

	provides dev.argon.externcompiler.ExternGenerator
		with dev.argon.libraryexterns.TestExternGenerator;
}
