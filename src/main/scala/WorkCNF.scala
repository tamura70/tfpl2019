/**
  * 「CNF式の処理」の課題回答用プログラム．"???" の部分を回答すること．
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/WorkCNF.scala WorkCNF.scala]]
  */
object WorkCNF {
  /** 課題 cnf2_exact1_4 : (p(0)+p(1)+p(2)+p(3) = 1) を表すCNFを出力する関数 cnf2_exact1_4 を WorkCNF.scala 中に記述せよ (Level 1)
    */
  def cnf2_exact1_4(p: Seq[Lit]): Unit = {
    require(p.size == 4)
    println(p.mkString(" "))
    println(~p(0) + " " + ~p(1))
    ??? //TODO
  }

  /** 課題 cnf2_atleast1 : (p(0)+p(1)+...+p(n-1) >= 1) を表すCNFを出力する関数 cnf2_atleast1 を WorkCNF.scala 中に記述せよ (Level 2)
    */
  def cnf2_atleast1(p: Seq[Lit]): Unit = {
    ??? //TODO
  }
  
  /** 課題 cnf2_atmost1 : (p(0)+p(1)+...+p(n-1) <= 1) を表すCNFを出力する関数 cnf2_atmost1 を WorkCNF.scala 中に記述せよ (Level 2)
    */
  def cnf2_atmost1(p: Seq[Lit]): Unit = {
    for (Seq(p1,p2) <- p.combinations(2)) {
      ??? //TODO
    }
  }
  
  /** 課題 cnf2_exact1 : (p(0)+p(1)+...+p(n-1) = 1) を表すCNFを出力する関数 cnf2_exact1 を WorkCNF.scala 中に記述せよ (Level 2)
    */
  def cnf2_exact1(p: Seq[Lit]): Unit = {
    ??? //TODO
  }
}
