ThisBuild / version          := "0.1.0"
ThisBuild / scalaVersion     := "2.13.10"

val chiselVersion = "3.5.5"

lazy val hardfloat = RootProject(uri("https://github.com/ucb-bar/berkeley-hardfloat.git"))
lazy val root = (project in file("."))
  .settings(
    name := "psygs-gen-dac",

    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.5"
    ),

    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-P:chiselplugin:genBundleElements"
    ),

    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
  ).dependsOn(hardfloat)
