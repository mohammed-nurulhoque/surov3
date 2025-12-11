ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.13.14"
ThisBuild / organization := "org.example"

val spinalVersion = "1.12.3"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
val ujsonLib = "com.lihaoyi" %% "ujson" % "3.3.1"
val upickleLib = "com.lihaoyi" %% "upickle" % "3.3.1"

lazy val surov3 = (project in file("."))
  .settings(
    name := "myproject", 
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin, ujsonLib, upickleLib)
  )

fork := true
