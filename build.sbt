enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.2"
jsDependencies += RuntimeDOM

name := "trie-autocomplete"

scalaVersion := "2.12.0"