

name := "scalajs-react"

version := "0.1"

scalaVersion := "2.12.6"


enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)
//scalaJSOutputMode := ECMAScript6

scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.2"
)


npmDependencies in Compile ++= Seq(
  "create-react-class" -> "^15.5.1",
  "react" -> "16.5.1",
  "react-dom" -> "16.5.1")
