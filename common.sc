import mill._
import scalalib._

trait XSCacheModule extends ScalaModule {

  def rocketModule: ScalaModule

  def utilityModule: ScalaModule

  def huancunModule: ScalaModule

  def openNCBModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(rocketModule, utilityModule, huancunModule, openNCBModule)
}