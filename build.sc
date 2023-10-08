import mill._
import scalalib._
import scalafmt._
import os.Path
import publish._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.build

val defaultVersions = Map(
  "chisel3" -> "3.5.4",
  "chisel3-plugin" -> "3.5.4",
  "chiseltest" -> "0.3.2",
  "scala" -> "2.12.13",
  "scalatest" -> "3.2.7"
)

def getVersion(dep: String, org: String = "edu.berkeley.cs", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = defaultVersions("scala")

  override def scalacOptions = Seq("-Xsource:2.11")

  val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.4"

  override def compileIvyDeps = Agg(macroParadise)
  override def scalacPluginIvyDeps = Agg(macroParadise, chisel3Plugin)

}

object rocketchip extends `rocket-chip`.common.CommonRocketChip {

  val rcPath = os.pwd / "rocket-chip"

  override def scalaVersion = defaultVersions("scala")

  override def scalacOptions = Seq("-Xsource:2.11")

  override def millSourcePath = rcPath

  object cdeRocket extends `rocket-chip`.cde.common.CDEModule with PublishModule {
    override def millSourcePath = rcPath / "cde" / "cde"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    override def pomSettings = T {
      rocketchip.pomSettings()
    }

    override def publishVersion = T {
      rocketchip.publishVersion()
    }
  }

  object hardfloatRocket extends `rocket-chip`.hardfloat.build.hardfloat {
    override def millSourcePath = rcPath / "hardfloat"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
      `rocket-chip`.common.getVersion("chisel3")
    ) else Agg.empty[Dep]

    def chisel3PluginIvyDeps = Agg(`rocket-chip`.common.getVersion("chisel3-plugin", cross=true))
  }

  def hardfloatModule = hardfloatRocket

  def cdeModule = cdeRocket

}

object utility extends SbtModule with ScalafmtModule with CommonModule {

   override def ivyDeps = Agg(`rocket-chip`.common.getVersion("chisel3"))

   override def millSourcePath = os.pwd / "utility"

   override def moduleDeps = super.moduleDeps ++ Seq(rocketchip)
 }

object huancun extends SbtModule with ScalafmtModule with CommonModule {
  override def millSourcePath = os.pwd / "HuanCun"
  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip, utility
  )
}

object CoupledL2 extends SbtModule with ScalafmtModule with CommonModule {

  override def millSourcePath = millOuterCtx.millSourcePath


  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel3"),
    getVersion("chiseltest"),
  )

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip, utility, huancun)

  object test extends Tests {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      getVersion("scalatest","org.scalatest")
    )

    def testFramework = "org.scalatest.tools.Framework"
  }
}
