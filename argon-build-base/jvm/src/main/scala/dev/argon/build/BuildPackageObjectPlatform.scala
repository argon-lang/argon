package dev.argon.build

import zio.blocking.Blocking

trait BuildPackageObjectPlatform {

  type BuildEnvironment = Blocking

}
