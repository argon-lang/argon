package dev.argon.plugin

import dev.argon.util.{*, given}
import zio.*

object PluginLoader {
  def load[E >: PluginError](pluginFactories: Seq[PluginFactory]): UIO[PluginSet[E]] =
    val (platformFactories, formatFactories) = dividePluginFactories(pluginFactories)

    for
      platformPlugins <- ZIO.foreach(platformFactories)(_.create[E])
      platforms = platformPlugins
        .map(platPlugin => PlatformPluginSet.Singleton(platPlugin) : PlatformPluginSet[E])
        .reduceOption(PlatformPluginSet.union)
        .getOrElse(PlatformPluginSet.Empty())

      formatPlugins <- ZIO.foreach(formatFactories)(_.create(platforms))
      formats = formatPlugins
        .map(fmtPlugin => FormatPluginSet.Singleton[E, platforms.type](fmtPlugin) : FormatPluginSet[E, platforms.type])
        .reduceOption(FormatPluginSet.union(platforms))
        .getOrElse(FormatPluginSet.Empty[E, platforms.type](platforms))

    yield PluginSet(platforms, formats)
  end load


  private def dividePluginFactories(pluginFactories: Seq[PluginFactory]): (Seq[PluginFactory.OfPlatform], Seq[PluginFactory.OfFormat]) =
    def impl(pluginFactories: List[PluginFactory], platforms: Seq[PluginFactory.OfPlatform], formats: Seq[PluginFactory.OfFormat]): (Seq[PluginFactory.OfPlatform], Seq[PluginFactory.OfFormat]) =
      pluginFactories match {
        case (head: PluginFactory.OfPlatform) :: tail => impl(tail, platforms :+ head, formats)
        case (head: PluginFactory.OfFormat) :: tail => impl(tail, platforms, formats :+ head)
        case Nil => (platforms, formats)
      }

    impl(pluginFactories.toList, Seq(), Seq())
  end dividePluginFactories

}
