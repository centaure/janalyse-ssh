import AssemblyKeys._

seq(assemblySettings: _*)

name := "Janalyse-SSH"

version := "0.5.1"

scalaVersion := "2.9.1"

mainClass in assembly := Some("scala.tools.nsc.MainGenericRunner")

jarName in assembly := "jassh.jar"

libraryDependencies <++=  scalaVersion { sv =>
       ("org.scala-lang" % "jline"           % sv  % "compile") ::
       ("org.scala-lang" % "scala-compiler"  % sv  % "compile") ::
       ("org.scala-lang" % "scala-dbc"       % sv  % "compile") ::
       ("org.scala-lang" % "scalap"          % sv  % "compile") ::
       ("org.scala-lang" % "scala-swing"     % sv  % "compile") ::Nil
}

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "com.jcraft" % "jsch" % "0.1.45" % "compile"

libraryDependencies += "junit" % "junit" % "4.10" % "test"
