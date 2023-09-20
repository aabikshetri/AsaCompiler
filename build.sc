import mill._, scalalib._

object Asa extends ScalaModule {
  def scalaVersion = "2.13.11"
  def scalacOptions = Seq("-deprecation")
  
  def unmanagedClasspath = T {
        if (!os.exists(millSourcePath / "lib")) Agg()
        else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_)))
  }
  
}
