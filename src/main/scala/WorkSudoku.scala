/**
  * 課題 cnf3_sudoku の課題回答用プログラム (Level 3)
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/WorkSudoku.scala WorkSudoku.scala]]
  */
object WorkSudoku {
  /**
    * 文献中の図2(b)の数独の問題 (藤原博文氏作)
    */
  val puzzle = Seq(
    Seq(0, 0, 0, 0, 0, 0, 0, 0, 0),
    Seq(0, 4, 3, 0, 0, 0, 6, 7, 0),
    Seq(5, 0, 0, 4, 0, 2, 0, 0, 8),
    Seq(8, 0, 0, 0, 6, 0, 0, 0, 1),
    Seq(2, 0, 0, 0, 0, 0, 0, 0, 5),
    Seq(0, 5, 0, 0, 0, 0, 0, 4, 0),
    Seq(0, 0, 6, 0, 0, 0, 7, 0, 0),
    Seq(0, 0, 0, 5, 0, 1, 0, 0, 0),
    Seq(0, 0, 0, 0, 8, 0, 0, 0, 0),
  )

  /**
    * i行j列のマスの数字がaであることを表すリテラル．i, jは1から始める．
    */
  def p(i: Int, j: Int, a: Int) = Lit("p" + i + "_" + j + "_" + a)

  /**
    * puzzleの数独を解くためのCNFを出力する関数．
    */
  def cnf3_sudoku(): Unit = {
    // 式(7)の節の出力
    for (i <- 1 to 9; j <- 1 to 9; a = puzzle(i-1)(j-1); if a > 0)
      println(p(i,j,a))
    // 以下に式(4)(5)(6)の節を出力するプログラムを記述する
    ??? //TODO
  }
}
