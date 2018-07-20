organization := "de.uni_koblenz"
version      := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.4"
name := "dltypes-lib"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "net.sourceforge.owlapi" % "owlapi-apibinding" % "3.4"

resolvers += "Stardog Public" at "http://maven.stardog.com"
libraryDependencies += "com.complexible.stardog" % "server" % "5.2.1"
libraryDependencies += "com.complexible.stardog" % "client-http" % "5.2.1"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.21"
