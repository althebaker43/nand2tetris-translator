import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

object translator extends RootModule with ScalaModule {
  def scalaVersion = "3.4.2"
  object test extends ScalaTests with TestModule.ScalaTest {
    override def ivyDeps: T[Agg[Dep]] = Agg(ivy"org.scalatest::scalatest:3.2.19")
  }
}
