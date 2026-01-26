#!/bin/bash

TEST_COMMAND="test_runner/runMain dev.argon.testrunner.TestRunner --libraries libraries --dist dist --backends backend testcases"

if sbt --client -Dsbt.server.autostart=false about < /dev/null > /dev/null 2>&1; then
  sbt --client "$TEST_COMMAND"
else
  sbt "$TEST_COMMAND"
fi

