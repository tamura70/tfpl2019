/**
  * 課題 bchain3_mul の課題回答用プログラム (Level 4)
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/WorkMul.scala WorkMul.scala]]
  */
object WorkMul {
  def bchain2_add(z: Seq[Lit], x: Seq[Lit], y: Seq[Lit]): Unit =
    WorkBchain.bchain2_add(z, x, y)

  def bchain3_mul(z: Seq[Lit], x: Seq[Lit], y: Seq[Lit]): Unit = {
    val m = x.size
    val n = y.size
    require(z.size == m + n)
    ??? //TODO
  }
}
