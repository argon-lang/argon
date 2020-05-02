package dev.argon

import zio.Has

package object build extends BuildPackageObjectPlatform {

  type BackendProvider = Has[BackendProvider.Service]

}
