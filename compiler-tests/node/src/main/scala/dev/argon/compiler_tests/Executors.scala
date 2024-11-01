package dev.argon.compiler_tests

import dev.argon.plugin.PluginError

def executors[E >: PluginError]: Seq[TestExecutor[E]] = Seq(
)


