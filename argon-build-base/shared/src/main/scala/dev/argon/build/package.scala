package dev.argon

import zio.Has

package object build {

  type BackendProvider = Has[BackendProvider.Service]

}
