import org.scalajs.core.tools.linker.backend.OutputMode
import scalajsbundler.sbtplugin.ScalaJSBundlerPlugin.autoImport.npmDependencies

val commonSettign = Seq(
  organization := "csw",
  version := "0.1",
  scalaVersion := "2.12.6"
)



lazy val core = project.in(file("core"))
  .settings(commonSettign: _*)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "scalajs-react",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.2",
      "com.lihaoyi" %%% "utest" % "0.6.6" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )


lazy val example = project.in(file("example"))
  .dependsOn(core)
  .settings(commonSettign: _*)
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
  .settings(
    name := "scalajs-react-example",
    scalaJSUseMainModuleInitializer := true,
    useYarn := true,
    scalaJSOutputMode := OutputMode.ECMAScript51,

    npmDependencies in Compile ++= Seq(
      "create-react-class" -> "^15.5.1",
      "react" -> "16.5.1",
      "react-dom" -> "16.5.1")
  )
