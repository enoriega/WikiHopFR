import Dependencies._
import sbtassembly.AssemblyPlugin.autoImport.MergeStrategy

lazy val scala212 = "2.12.8"
lazy val scala211 = "2.11.12"
lazy val supportedScalaVersions = List(scala212, scala211)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.ml4ai",
      scalaVersion := scala212,
      //crossScalaVersions := supportedScalaVersions,
      version      := "0.1.0-SNAPSHOT"
    )),
    scalacOptions ++= Seq(
      "-feature",
    ),
    name := "WikiHopFR",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-jackson" % "3.6.3",
      "org.clulab" %% "processors-main" % "7.4.5-SNAPSHOT",
      "org.clulab" %% "processors-modelsmain" % "7.4.5-SNAPSHOT",
      "org.clulab" %% "processors-corenlp" % "7.4.5-SNAPSHOT",
      "org.clulab" %% "processors-modelscorenlp" % "7.4.5-SNAPSHOT",
      "org.clulab" %% "processors-odin" % "7.4.5-SNAPSHOT",
      "com.typesafe" % "config" % "1.3.3",
      "org.apache.lucene" % "lucene-core" % "7.6.0",
      "org.apache.lucene" % "lucene-analyzers-common" % "7.6.0",
      "org.apache.lucene" % "lucene-queryparser" % "7.6.0",
      "org.scala-graph" %% "graph-core" % "1.12.5",
      "org.scala-graph" %% "graph-dot" % "1.12.1",
      "org.clulab" %% "sarsamora" % "0.2.0-SNAPSHOT",
      "org.apache.httpcomponents" % "httpclient" % "4.5.9",
      scalaTest % Test
    ),
  )
