scalaVersion := "2.11.8"

enablePlugins(AndroidApp)
android.useSupportVectors

versionCode := Some(1)
version := "0.1-SNAPSHOT"
name := "family-money-tracker"
organization := "net.arturaz.family_money_tracker"

instrumentTestRunner :=
  "android.support.test.runner.AndroidJUnitRunner"

platformTarget := "android-25"

javacOptions in Compile ++= "-source" :: "1.7" :: "-target" :: "1.7" :: Nil

libraryDependencies ++= Vector(
  "com.android.support" % "appcompat-v7" % "24.0.0",
  "com.android.support.test" % "runner" % "0.5" % "androidTest",
  "com.android.support.test.espresso" % "espresso-core" % "2.2.2" % "androidTest",
  "com.nrinaudo" %% "kantan.csv" % "0.1.18",
  "org.typelevel" %% "cats-core" % "0.9.0",
  "com.softwaremill.quicklens" % "quicklens_2.11" % "1.4.8"
)