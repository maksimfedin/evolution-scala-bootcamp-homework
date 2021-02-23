package com.evolutiongaming.bootcamp.homework.plugin

import sbt.Keys._
import sbt._


/**
 * @author Maxim Fedin
 */
object BulkySourcesPlugin extends AutoPlugin {
    import autoImport._

    object autoImport {
        lazy val bulkySources = taskKey[Seq[(Int, File)]]("Sorted list of bullki sources with largest number of lines")

        lazy val bulkyThresholdInLines = settingKey[Int]("Threshold of the bulky lines")
    }

    override def trigger = allRequirements

    override val projectSettings: Seq[Setting[_]] = inConfig(Compile)(baseBulkySources) ++ inConfig(Test)(baseBulkySources)

    lazy val baseBulkySources: Seq[Setting[_]] = Seq(
        bulkySources := {
            bulkyThresholdInLines := 100
            getBulkyLines(sources.value, bulkyThresholdInLines.value)
        }
    )

    private def getBulkyLines(files: Seq[File], threshold: Int): Seq[(Int, sbt.File)] = {
        files.map { file => (sbt.IO.readLines(file).length, file) }
          .filter { case (count, _) => count > threshold }
          .sortBy { case (count, _) => count }
          .reverse
    }
}
