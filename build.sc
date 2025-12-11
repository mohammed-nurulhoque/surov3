import mill._, scalalib._

val spinalVersion = "1.12.3"

object surov3 extends SbtModule {
  def scalaVersion = "2.13.14"
  override def millSourcePath = os.pwd
  def sources = T.sources(
    millSourcePath / "hw" / "spinal"
  )
  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:$spinalVersion",
    ivy"com.github.spinalhdl::spinalhdl-lib:$spinalVersion",
    ivy"com.lihaoyi::ujson:3.3.1",
    ivy"com.lihaoyi::upickle:3.3.1"
  )
  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:$spinalVersion")
}
