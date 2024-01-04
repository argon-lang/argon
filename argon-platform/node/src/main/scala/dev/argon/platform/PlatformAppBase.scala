package dev.argon.platform

import zio.*

trait PlatformAppBase[R, E, AppLayerIn >: ZIOAppArgs] extends ZIOApp {
}
