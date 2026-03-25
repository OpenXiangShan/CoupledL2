import mill._
import scalalib._

trait CoupledL2Module extends ScalaModule {

  def rocketModule: ScalaModule

  def utilityModule: ScalaModule

  def huancunModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(rocketModule, utilityModule, huancunModule)
}

trait OpenLLCModule extends ScalaModule {

  def coupledL2Module: ScalaModule

  def rocketModule: ScalaModule

  def utilityModule: ScalaModule

  def openNCBModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(coupledL2Module, rocketModule, utilityModule, openNCBModule)
}
