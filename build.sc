import mill._
import scalalib._
import scalafmt._
import os.Path
import publish._
import $file.common
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.common

val defaultScalaVersion = "2.13.15"

def defaultVersions = Map(
  "chisel"        -> ivy"org.chipsalliance::chisel:7.0.0",
  "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:7.0.0"
)

val pwd = os.Path(sys.env("MILL_WORKSPACE_ROOT"))

trait HasChisel extends SbtModule {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(defaultVersions("chisel"))

  def chiselPluginIvy: Option[Dep] = Some(defaultVersions("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object rocketchip extends `rocket-chip`.common.RocketChipModule with HasChisel {

  val rcPath = pwd / "rocket-chip"
  override def millSourcePath = rcPath

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.7"

  object macros extends `rocket-chip`.common.MacrosModule with SbtModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${scalaVersion}"
  }

  object cde extends `rocket-chip`.cde.common.CDEModule with ScalaModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = rcPath / "cde" / "cde"
  }

  object hardfloat extends `rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {
    override def millSourcePath = rcPath / "hardfloat" / "hardfloat"
  }

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

}

object utility extends HasChisel {
  override def millSourcePath = pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::sourcecode:0.4.4",
  )
}

object huancun extends HasChisel {
  override def millSourcePath = pwd / "HuanCun"
  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip, utility
  )
}

object openNCB extends HasChisel {
  override def millSourcePath = pwd / "openNCB"
  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)
}

object CoupledL2 extends HasChisel with $file.common.CoupledL2Module {

  override def millSourcePath = pwd / "coupledL2"

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def huancunModule: ScalaModule = huancun

  object test extends SbtTests with TestModule.ScalaTest

  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")

}

object OpenLLC extends HasChisel with millbuild.common.OpenLLCModule {

  override def millSourcePath = pwd / "openLLC"

  def coupledL2Module: ScalaModule = CoupledL2

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def openNCBModule: ScalaModule = openNCB

  object test extends SbtTests with TestModule.ScalaTest

  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")

}