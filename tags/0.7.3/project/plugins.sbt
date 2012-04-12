// Eclipse IDE plugin for SBT - Allowing sbt project import/editing within eclipse
// but keeping SBT as the main build system

resolvers += Classpaths.typesafeResolver

resolvers += Resolver.url("sbt-plugin-releases",
  new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0")


addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.3")
