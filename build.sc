import mill._
import scalalib._
import $file.{common => commonModule}
import $file.`rocket-chip`.{common => rocketChipCommon}
import $file.`rocket-chip`.cde.{common => cdeCommon}
import $file.`rocket-chip`.hardfloat.{common => hardfloatCommon}

val defaultScalaVersion = "2.13.15"

def defaultVersions = Map(
  "chisel"        -> ivy"org.chipsalliance::chisel:6.6.0",
  "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.6.0",
  "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:6.0.0",
  "sourcecode"    -> ivy"com.lihaoyi::sourcecode:0.4.2",
)

trait HasChisel extends ScalaModule {
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

object `rocket-chip` extends rocketChipCommon.RocketChipModule with HasChisel {

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.7"

  object macros extends rocketChipCommon.MacrosModule with HasChisel {
    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${scalaVersion}"
  }

  object cde extends cdeCommon.CDEModule with HasChisel {
    override def millSourcePath = super.millSourcePath / "cde"
  }

  object hardfloat extends hardfloatCommon.HardfloatModule with HasChisel {
    override def millSourcePath = super.millSourcePath / "hardfloat"
  }

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

  override def ivyDeps = super.ivyDeps() ++ Agg(
    mainargsIvy,
    json4sJacksonIvy,
  )
  override def moduleDeps = super.moduleDeps ++ Seq(macrosModule, hardfloatModule, cdeModule)
}

object utility extends SbtModule with HasChisel {
  override def moduleDeps = super.moduleDeps ++ Seq(`rocket-chip`)

  override def ivyDeps = super.ivyDeps() ++ Agg(
    defaultVersions("sourcecode"),
  )
 }

object HuanCun extends SbtModule with HasChisel {
  override def moduleDeps = super.moduleDeps ++ Seq(`rocket-chip`, utility)
}

object CoupledL2 extends SbtModule with HasChisel with commonModule.CoupledL2Module {

  override def millSourcePath = super.millSourcePath / os.up

  def rocketModule: ScalaModule = `rocket-chip`

  def utilityModule: ScalaModule = utility

  def huancunModule: ScalaModule = HuanCun

  object test extends SbtTests with TestModule.ScalaTest {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest"),
    )
  }
  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")

  override def moduleDeps = super.moduleDeps ++ Seq(rocketModule, utilityModule, huancunModule)
}
