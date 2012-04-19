import AssemblyKeys._

seq(assemblySettings: _*)

name := "janalyse-ssh-onejar"

scalaVersion := "2.9.2"

mainClass in assembly := Some("scala.tools.nsc.MainGenericRunner")

jarName in assembly := "jassh.jar"

libraryDependencies <++=  scalaVersion { sv =>
       ("org.scala-lang" % "jline"           % sv  % "compile")  ::
       ("org.scala-lang" % "scala-compiler"  % sv  % "compile")  ::
       ("org.scala-lang" % "scala-dbc"       % sv  % "compile")  ::
       ("org.scala-lang" % "scalap"          % sv  % "compile")  ::
       ("org.scala-lang" % "scala-swing"     % sv  % "compile")  ::Nil
}

libraryDependencies += "fr.janalyse"   %% "janalyse-ssh" % "0.7.4" % "compile"

resolvers += "JAnalyse Repository" at "http://www.janalyse.fr/repository/"
