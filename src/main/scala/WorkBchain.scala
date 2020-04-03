/**
  * 「ブール連鎖の処理」の課題回答用プログラム．"???" の部分を回答すること．
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/WorkBchain.scala WorkBchain.scala]]
  */
object WorkBchain {
  /** 課題 bchain2_halfadder : ((z1z0) = x0 + y0) を表すブール連鎖を出力する関数 bchain2_halfadder を WorkBchain.scala 中に記述せよ (Level 1)
    */
  def bchain2_halfadder(z1: Lit, z0: Lit, x0: Lit, y0: Lit): Unit = {
    println(z0 + " = XOR " + x0 + " " + y0)
    ??? //TODO
  }

  /** 課題 bchain2_fulladder : ((z1z0) = x0 + y0 + w0) を表すブール連鎖を出力する関数 bchain2_fulladder を WorkBchain.scala 中に記述せよ (Level 1)
    */
  def bchain2_fulladder(z1: Lit, z0: Lit, x0: Lit, y0: Lit, w0: Lit): Unit = {
    ??? //TODO
  }

  /** 課題 bchain2_4p4 : (z@5 = x@4 + y@4) を表すブール連鎖を出力する関数 bchain2_4p4 を WorkBchain.scala 中に記述せよ (Level 1)
    */
  def bchain2_4p4(z: Seq[Lit], x: Seq[Lit], y: Seq[Lit]): Unit = {
    val n = 4
    require(x.size == n && y.size == n && z.size == n+1)
    val c = Lit.newLits(n)
    bchain2_halfadder(c(0), z(0), x(0), y(0))
    bchain2_fulladder(c(1), z(1), x(1), y(1), c(0))
    ??? //TODO
  }

  /** 課題 bchain2_4gt4 : (x@4 > y@4) を表すブール連鎖を出力する関数 bchain2_4gt4 を WorkBchain.scala 中に記述せよ (Level 2)
    */
  def bchain2_4gt4(x: Seq[Lit], y: Seq[Lit]): Unit = {
    val n = 4
    require(x.size == n && y.size == n)
    // val z = Lit.newLits(n+1)
    // val ny = y.map(lit => ~lit)
    ??? //TODO
  }

  /** 課題 bchain2_add : (z@(n+1) = x@n + y@n) を表すブール連鎖を出力する関数 bchain2_add を WorkBchain.scala 中に記述せよ (Level 2)
    */
  def bchain2_add(z: Seq[Lit], x: Seq[Lit], y: Seq[Lit]): Unit = {
    val n = x.size
    require(y.size == n && z.size == n+1)
    val c = Lit.newLits(n)
    bchain2_halfadder(c(0), z(0), x(0), y(0))
    ??? //TODO
  }

  /** 課題 bchain2_3p3p3 : (z@5 = x@3 + y@3 + w@3) を表すブール連鎖を出力する関数 bchain2_3p3p3 を WorkBchain.scala 中に記述せよ (Level 2)
    */
  def bchain2_3p3p3(z: Seq[Lit], x: Seq[Lit], y: Seq[Lit], w: Seq[Lit]): Unit = {
    val n = 3
    require(x.size == n && y.size == n && w.size == n && z.size == n+2)
    ??? //TODO
  }

}
