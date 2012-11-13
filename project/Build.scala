package parsnip

import sbt._
import sbt.Defaults._
import Keys._
import Dependencies._
import Resolvers._

object ParsnipBuild extends Build {
  import Dependencies._





  val parsnipSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.vireo",
    name         := "parsnip",
    version      := "0.1",
    scalaVersion := "2.9.2"
  )
 
  lazy val project = Project("parsnip", file("."), settings = parsnipSettings    
                             ++ Seq(resolvers ++= repos,
		             libraryDependencies ++= parsnip_dependencies
                                    )
                           )
}

object Resolvers {
  val sonatype_snapshots = "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  val repos = Seq()//sonatype_snapshots )
}

object Dependencies {
  val scalazVersion = "7.0-SNAPSHOT"
  val specs2Version = "1.9"
  val parsnip_dependencies = Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.specs2" %% "specs2" % specs2Version
    )
}
