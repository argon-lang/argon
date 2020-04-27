package dev.argon.build.testrunner

import zio.Has

package object js {
  type JSModuleLoad = Has[JSModuleLoad.Service]
}
