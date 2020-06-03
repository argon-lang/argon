package dev.argon.backend

import zio.Has

package object js {
  type IdProvider = Has[IdProvider.Service]

}
