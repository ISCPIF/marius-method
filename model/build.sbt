organization := "fr.geocites"

name := "marius"

version := "1.0.0"

scalaVersion := "2.11.1"

libraryDependencies += "org.geotools" % "gt-referencing" % "9.3"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1"

osgiSettings

OsgiKeys.exportPackage := Seq("fr.geocites.marius.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("*")

scalariformSettings

