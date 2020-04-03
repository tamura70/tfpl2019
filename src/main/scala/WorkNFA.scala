/**
  * 課題 cnf3_nfa の課題回答用プログラム (Level 3)
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/WorkNFA.scala WorkNFA.scala]]
  */
object WorkNFA {
  /**
    * i文字目を読む前の状態 s_i が q_j であることを表すリテラル p_{ij}
    */
  def p(i: Int, j: Int) = Lit("p" + i + "_" + j)

  /**
    * NFAに対応するCNFを出力する関数．
    */
  def cnf3_nfa(x: Seq[Lit]): Unit = {
    val n = x.size
    // 初期状態 s_0 = q_0
    println(p(0, 0))
    // 終了状態 s_n = q_2
    println(p(n, 2))
    // 状態
    for (i <- 0 to n) {
      WorkCNF.cnf2_exact1(Seq(p(i,0), p(i,1), p(i,2)))
    }
    // 遷移
    for (i <- 0 until n) {
      // s_i=q_0 and x(i)=0 ==> s_{i+1}=q_2
      println(Seq(~p(i,0), x(i), p(i+1,2)).mkString(" "))
      // s_i=q_0 and x(i)=1 ==> s_{i+1}=q_1 or s_{i+1}=q_2
      println(Seq(~p(i,0), ~x(i), p(i+1,1), p(i+1,2)).mkString(" "))
      ??? //TODO
    }
  }
}
