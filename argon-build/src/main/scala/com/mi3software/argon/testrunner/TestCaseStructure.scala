package com.mi3software.argon.testrunner

import scala.collection.immutable._

final case class TestCaseStructure(nestedStructures: Seq[(String, TestCaseStructure)], tests: Seq[TestCase])
