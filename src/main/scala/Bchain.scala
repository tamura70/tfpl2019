/**
  * ブール連鎖を表すケースクラス．
  * 
  * @param chain ブール連鎖式を表す文字列の列 ([[scala.collection.Seq Seq]])
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/Bchain.scala Bchain.scala]]
  */
case class Bchain(chain: Seq[String]) {
  /**
    * 2つのBchainを結合したBchainを返す．
    *
    * @param that 結合するBchain
    * @return このBchainとthatを結合したBchain
    */
  def ++ (that: Bchain): Bchain =
    Bchain(chain ++ that.chain)

  /**
    * ブール連鎖を表示する
    */
  def show(): Unit = {
    var c = 0
    for (exp <- chain) {
      c += 1
      println(exp)
    }
  }

  private def equiv(z: Lit, x: Lit): Seq[Seq[Lit]] =
    Seq(
      Seq(~z, x),
      Seq(z, ~x),
    )
  private def eqAnd(z: Lit, x1: Lit, x2: Lit): Seq[Seq[Lit]] =
    Seq(
      Seq(~z, x1),
      Seq(~z, x2),
      Seq(z, ~x1, ~x2),
    )
  private def eqOr(z: Lit, x1: Lit, x2: Lit): Seq[Seq[Lit]] =
    Seq(
      Seq(~z, x1, x2),
      Seq(z, ~x1),
      Seq(z, ~x2),
    )
  private def eqXor(z: Lit, x1: Lit, x2: Lit): Seq[Seq[Lit]] =
    Seq(
      Seq(~z, ~x1, ~x2),
      Seq(~z, x1, x2),
      Seq(z, ~x1, x2),
      Seq(z, x1, ~x2),
    )
  private def eqAnd3(z: Lit, x1: Lit, x2: Lit, x3: Lit): Seq[Seq[Lit]] =
    Seq(
      Seq(~z, x1),
      Seq(~z, x2),
      Seq(~z, x3),
      Seq(~z, ~x1, ~x2, ~x3),
    )
  private def eqOr3(z: Lit, x1: Lit, x2: Lit, x3: Lit): Seq[Seq[Lit]] =
    Seq(
      Seq(~z, x1, x2, x3),
      Seq(z, ~x1),
      Seq(z, ~x2),
      Seq(z, ~x3),
    )
  private def eqXor3(z: Lit, x1: Lit, x2: Lit, x3: Lit): Seq[Seq[Lit]] =
    Seq(
      Seq(~z, ~x1, ~x2, x3),
      Seq(~z, ~x1, x2, ~x3),
      Seq(~z, x1, ~x2, ~x3),
      Seq(~z, x1, x2, x3),
      Seq(z, ~x1, ~x2, ~x3),
      Seq(z, ~x1, x2, x3),
      Seq(z, x1, ~x2, x3),
      Seq(z, x1, x2, ~x3),
    )
  private def eqMedian(z: Lit, x1: Lit, x2: Lit, x3: Lit): Seq[Seq[Lit]] =
    Seq(
      Seq(~z, x1, x2),
      Seq(~z, x1, x3),
      Seq(~z, x2, x3),
      Seq(z, ~x1, ~x2),
      Seq(z, ~x1, ~x3),
      Seq(z, ~x2, ~x3),
    )
  private def atleast1(x: Seq[Lit]): Seq[Seq[Lit]] =
    Seq(x)
  private def atmost1(x: Seq[Lit]): Seq[Seq[Lit]] =
    for (Seq(x1,x2) <- x.combinations(2).toSeq)
    yield Seq(~x1, ~x2)
  private def exact1(x: Seq[Lit]): Seq[Seq[Lit]] =
    atleast1(x) ++ atmost1(x)

  /**
    * ブール連鎖と同値なCNFを返す．
    * 
    * @return 同値なCNF
    */
  def toCNF: CNF = {
    def lit(s: String): Lit = Lit.mkLit(s)
    def lits(ss: Seq[String]): Seq[Lit] = Lit.mkLits(ss)
    var clauses: Seq[Seq[Lit]] = Vector.empty
    for {
      line0 <- chain
      line = line0.trim
      if line != "" && line != "~" && ! line.startsWith("~ ")
      ss = line.split("""\s+""").toSeq
    } ss match {
      case Seq(s1, "=", s2) =>
        clauses ++= equiv(lit(s1), lit(s2))
      case Seq(s1, "=", "AND", s2, s3) =>
        clauses ++= eqAnd(lit(s1), lit(s2), lit(s3))
      case Seq(s1, "=", "OR", s2, s3) =>
        clauses ++= eqOr(lit(s1), lit(s2), lit(s3))
      case Seq(s1, "=", "XOR", s2, s3) =>
        clauses ++= eqXor(lit(s1), lit(s2), lit(s3))
      case Seq(s1, "=", "AND", s2, s3, s4) =>
        clauses ++= eqAnd3(lit(s1), lit(s2), lit(s3), lit(s4))
      case Seq(s1, "=", "OR", s2, s3, s4) =>
        clauses ++= eqOr3(lit(s1), lit(s2), lit(s3), lit(s4))
      case Seq(s1, "=", "XOR", s2, s3, s4) =>
        clauses ++= eqXor3(lit(s1), lit(s2), lit(s3), lit(s4))
      case Seq(s1, "=", "MEDIAN", s2, s3, s4) =>
        clauses ++= eqMedian(lit(s1), lit(s2), lit(s3), lit(s4))
      case Seq("AL1", ss @ _*) =>
        clauses ++= atleast1(lits(ss))
      case Seq("AM1", ss @ _*) =>
        clauses ++= atmost1(lits(ss))
      case Seq("EX1", ss @ _*) =>
        clauses ++= exact1(lits(ss))
      case _ => {
        throw new IllegalArgumentException("間違ったブール連鎖の記述 : " + line)
      }
    }
    CNF(clauses)
  }
}

/**
  * ブール連鎖用のメソッドを提供するコンパニオンオブジェクト．
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/Bchain.scala Bchain.scala]]
  */
object Bchain {
  import scala.io.Source

  /**
    * ブール連鎖ファイルを読み込み，ブール連鎖を返す．
    *
    * @param fileName ブール連鎖ファイル名
    * @return ブール連鎖
    */
  def load(fileName: String): Bchain = {
    val chain = Source.fromFile(fileName).getLines.toSeq
    Bchain(chain)
  }

  /**
    * 2つのブール連鎖ファイルを読み込み，ブール連鎖を返す．
    * 
    * @param fileName1 1つ目のブール連鎖ファイル名
    * @param fileName2 2つ目のブール連鎖ファイル名
    * @return ブール連鎖
    */
  def load(fileName1: String, fileName2: String): Bchain = {
    val chain1 = Source.fromFile(fileName1).getLines
    val chain2 = Source.fromFile(fileName2).getLines
    Bchain((chain1 ++ chain2).toSeq)
  }
}
