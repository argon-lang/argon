package dev.argon.plugin

import dev.argon.util.{*, given}
import zio.*

object PluginLoader {
  def load(pluginFactories: Seq[PluginFactory]): UIO[PluginSet] =
    val (platformFactories, formatFactories) = dividePluginFactories(pluginFactories)

    for
      platformPlugins <- ZIO.foreach(platformFactories)(_.create)
      platforms = platformPlugins
        .map(PlatformPluginSet.Singleton(_))
        .reduceOption(PlatformPluginSet.union)
        .getOrElse(PlatformPluginSet.Empty)

      formatPlugins <- ZIO.foreach(formatFactories)(_.create(platforms))
      formats = formatPlugins
        .map(FormatPluginSet.Singleton[platforms.type](_))
        .reduceOption(FormatPluginSet.union(platforms))
        .getOrElse(FormatPluginSet.Empty[platforms.type](platforms))

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
