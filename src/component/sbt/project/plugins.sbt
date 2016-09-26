resolvers +=  Resolver.url(
  "meetup-sbt-plugins",
  new java.net.URL("https://dl.bintray.com/meetup/sbt-plugins/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.meetup" % "sbt-plugins" % "0.2.19")

addSbtPlugin("com.eed3si9n" % "sbt-dirty-money" % "0.1.0")

