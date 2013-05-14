name := "janalyse-ssh"

version := "0.9.9-1"

organization :="fr.janalyse"

organizationHomepage := Some(new URL("http://www.janalyse.fr"))

scalaVersion := "2.10.1"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature")

crossScalaVersions := Seq("2.10.1")

libraryDependencies ++= Seq(
    "com.typesafe"      %% "scalalogging-slf4j" % "1.0.1"
   ,"com.jcraft"         % "jsch"               % "0.1.49"
   ,"org.apache.commons" % "commons-compress"   % "1.5"
   ,"org.scalatest"     %% "scalatest"          % "1.9.1"  % "test"
   ,"junit"              % "junit"              % "4.10"   % "test"
)

publishTo := Some(
     Resolver.sftp(
         "JAnalyse Repository",
         "www.janalyse.fr",
         "/home/tomcat/webapps-janalyse/repository"
     ) as("tomcat", new File(util.Properties.userHome+"/.ssh/id_rsa"))
)
