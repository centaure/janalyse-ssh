name := "janalyse-ssh"

version := "0.9.8"

organization :="fr.janalyse"

organizationHomepage := Some(new URL("http://www.janalyse.fr"))

scalaVersion := "2.10.0"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature")

crossScalaVersions := Seq("2.10.0")

libraryDependencies ++= Seq(
    "com.typesafe" %% "scalalogging-slf4j" % "1.0.0"
   ,"org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
   ,"com.jcraft" % "jsch" % "0.1.49" % "compile"
   ,"junit" % "junit" % "4.10" % "test"
)

publishTo := Some(
     Resolver.sftp(
         "JAnalyse Repository",
         "www.janalyse.fr",
         "/home/tomcat/webapps-janalyse/repository"
     ) as("tomcat", new File(util.Properties.userHome+"/.ssh/id_rsa"))
)
