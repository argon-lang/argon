package dev.argon.backend.bridge;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

final class BackendBridgeTest {
	@Test
	void createsPlatformMetadataInMemory() throws Exception {
		assertNotEquals(0, BackendBridge.platformMetadata(new String[0], new byte[0][]).length);
	}

	@Test
	void propagatesMetadataExceptions() {
		assertThrows(IllegalArgumentException.class,
			() -> BackendBridge.platformMetadata(new String[]{"bad.class"}, new byte[][]{{0, 1, 2}}));
	}

	@Test
	void propagatesCodegenExceptions() {
		assertThrows(Exception.class, () -> BackendBridge.codegen("bad.avm", new byte[]{0, 1, 2}, false));
	}
}
