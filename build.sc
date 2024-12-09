import mill._
import mill.scalalib._

object myModule extends ScalaModule {
  def scalaVersion = "3.3.3"
  libraryDependencies ++= Seq(
    "org.quickfixj" % "quickfixj-all" % "2.3.1",
    "com.lmax" % "disruptor" % "3.4.4",
    "com.typesafe.akka" %% "akka-actor" % "2.6.20"
  )
}
