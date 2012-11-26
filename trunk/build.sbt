name := "janalyse-ssh"

version := "0.9.5-b3"

organization :="fr.janalyse"

organizationHomepage := Some(new URL("http://www.janalyse.fr"))

scalaVersion := "2.10.0-RC2"

crossScalaVersions := Seq("2.9.1", "2.9.1-1", "2.9.2", "2.10.0-RC2")

//libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"
libraryDependencies += "org.scalatest" % "scalatest_2.10.0-RC2" % "2.0.M5" % "test"

//libraryDependencies += "org.scala-lang" % "scala-actors-migration" % "2.10.0-RC2"


libraryDependencies += "com.jcraft" % "jsch" % "0.1.49" % "compile"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

publishTo := Some(
     Resolver.sftp(
         "JAnalyse Repository",
         "www.janalyse.fr",
         "/home/tomcat/webapps-janalyse/repository"
     ) as("tomcat", new File(util.Properties.userHome+"/.ssh/id_rsa"))
)
