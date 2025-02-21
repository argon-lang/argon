package dev.argon.compiler_tests

import dev.argon.backend.options.OptionValue
import dev.argon.backend.Backend

sealed trait BackendOptionsProvider {
  def getOptionsForBackend(backend: Backend[TestError]): Option[Map[String, OptionValue[TestError]]]
}

object BackendOptionsProvider {
  def apply(mapping: (String, Map[String, OptionValue[TestError]])*): BackendOptionsProvider =
    val backendMap = mapping.toMap
    new BackendOptionsProvider {
      override def getOptionsForBackend(backend: Backend[TestError]): Option[Map[String, OptionValue[TestError]]] =
        backendMap.get(backend.name)
    }
  end apply
}

