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
       ("org.scala-lang" % "scalap"          % sv  % "compile")  ::Nil
//       ("org.scala-lang" % "scala-swing"     % sv  % "compile")  ::Nil
}

libraryDependencies += "fr.janalyse"   %% "janalyse-ssh" % "0.8.0" % "compile"

resolvers += "JAnalyse Repository" at "http://www.janalyse.fr/repository/"


// jansi is embedded inside jline !
excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
  cp filter {c=> List("jansi") exists {c.data.getName contains _} }
}