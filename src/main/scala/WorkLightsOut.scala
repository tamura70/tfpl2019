/**
  * 課題 bchain3_lightsout の課題回答用プログラム (Level 4)
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/WorkLightsOut.scala WorkLightsOut.scala]]
  */
object WorkLightsOut {
  /**
    * tステップ目にi行j列のセルが点いていることを表すリテラル．t, i, jは0から始める．
    */
  def p(t: Int, i: Int, j: Int) = Lit("p" + t + "_" + i + "_" + j)

  /**
    * tステップ目からt+1ステップ目にi行j列のセルを押すことを表すリテラル
    */
  def q(t: Int, i: Int, j: Int) = Lit("q" + t + "_" + i + "_" + j)

  /**
    * ライツアウトを解くためのブール連鎖を出力する関数．
    * サイズは5*5とし，0ステップ目はすべてのセルが点いているとする．
    * ステップ数 steps で全部消せる方法を探す．
    */
  def bchain3_lightsout(steps: Int): Unit = {
    // サイズ
    val n = 5
    // 0ステップ目の状態
    for (i <- 0 until n; j <- 0 until n)
      println(p(0,i,j) + " = 1")
    // 最後のステップの状態
    for (i <- 0 until n; j <- 0 until n)
      println(p(steps,i,j) + " = 0")
    // 各ステップ t で押すのはちょうど1つのセル q(t,i,j)
    for (t <- 0 until steps) {
      val qs = for (i <- 0 until n; j <- 0 until n) yield q(t,i,j)
      println("EX1 " + qs.mkString(" "))
    }
    // 同じセルを押すのは高々1回
    for (i <- 0 until n; j <- 0 until n) {
      val qs = for (t <- 0 until steps) yield q(t,i,j)
      println("AM1 " + qs.mkString(" "))
    }
    ??? //TODO
  }
}
