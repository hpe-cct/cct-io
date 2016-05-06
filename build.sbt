name := "cogx-io"

description := "CogX I/O libraries."

organizationName := "Hewlett Packard Labs"

organizationHomepage := Some(url("http://www.labs.hpe.com"))

version := "0.8.6"

organization := "com.hpe.cct"

scalaVersion := "2.11.7"

parallelExecution in Test := false

libraryDependencies += "com.hpe.cct" %% "cogx" % "4.4.9"

libraryDependencies ++= {
  val javacv = "1.1"
  val baseLibs = Seq("org.bytedeco" % "javacv" % s"$javacv", "org.bytedeco" % "javacpp" % s"$javacv" classifier "")
  val classifiers = Seq("", "linux-x86_64", "windows-x86_64", "macosx-x86_64")
  val upstream = Seq(
    ("artoolkitplus", "2.3.1"),
    ("ffmpeg", "2.8.1"),
    ("flandmark", "1.07"),
    ("flycapture", "2.8.3.1"),
    ("libdc1394", "2.2.3"),
    ("libfreenect", "0.5.3"),
    ("opencv", "3.0.0"),
    ("videoinput", "0.200")
  )
  baseLibs ++ upstream.flatMap(l => {
    classifiers.map(c => {
      "org.bytedeco.javacpp-presets" % l._1 % (l._2 + s"-$javacv") classifier c
    })
  })
}

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.7" % "test"

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

bintrayRepository := "maven"

bintrayOrganization := Some("cogexmachina")