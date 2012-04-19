name := "janalyse-ssh"

version := "0.7.4"

organization :="fr.janalyse"

organizationHomepage := Some(new URL("http://www.janalyse.fr"))

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.1", "2.9.2")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.2" % "test"

libraryDependencies += "com.jcraft" % "jsch" % "0.1.47" % "compile"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

publishTo := Some(
     Resolver.sftp(
         "JAnalyse Repository",
         "www.janalyse.fr",
         "/home/tomcat/webapps-janalyse/repository"
     ) as("tomcat", new File(util.Properties.userHome+"/.ssh/id_rsa"))
)
