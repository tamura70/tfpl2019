/**
  * リテラルを表すケースクラス．
  * 
  * Lit("p", true) および Lit("p") は変数名が p の正リテラルを生成する．
  * Lit("p", false) は変数名が p の負リテラルを生成する．
  * 
  * 変数名 `name` は，"1" あるいは英字で始まる英数字の文字列でなければならない (英字には _ を含む)．
  * 異なれば，例外が発生する．
  *
  * @param name 変数名の文字列
  * @param positive 正負を表すブール値 (省略可)．trueなら正リテラル，falseなら負リテラル．
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/CNF.scala CNF.scala]]
  */
case class Lit(name: String, positive: Boolean = true) {
  /* 変数名 name のチェック */
  require(
    name.matches("""1|[a-zA-Z_][a-zA-Z_0-9]*"""),
    "間違った変数名 : " + name
  )

  /** `~x` は `x` の正負を反転させたリテラルを返す．
    * @return 正負を反転させたリテラル
    */
  def unary_~ = Lit(name, ! positive)

  /** リテラルの文字列表現を返す．
    * Lit("p",true) は "p"，Lit("p",false) は "~p" という文字列になる．
    * ただし Lit("1",true)は "1", Lit("1",false)は "0" になる．
    * @return リテラルの文字列表現
    */
  override def toString = name match {
    case "1" => if (positive) name else "0"
    case _ => if (positive) name else "~" + name
  }
}

/**
  * リテラル用のメソッドを提供するコンパニオンオブジェクト．
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/CNF.scala CNF.scala]]
  */
object Lit {
  /** 真を表すリテラル */
  val True = Lit("1")

  /** 偽を表すリテラル */
  val False = ~True

  /** Falseの列を返す．
    * @param n Falseの個数
    * @return Falseの列
    */
  def False(n: Int): Seq[Lit] =
    Seq.fill(n)(False)

  /**
    * 変数名が s から始まるリテラルの列 ([[scala.collection.Seq Seq]])を返す．
    * s が "p" で n が 3 なら，Seq(Lit("p0"),Lit("p1"),Lit("p2")) が返される．
    * @param name 変数名の最初の部分
    * @param n リテラルの個数
    * @return リテラルの列
    */
  def lits(s: String, n: Int): Seq[Lit] =
    (0 until n).map(i => Lit(s + i))

  /** 
    * CNFファイル中でリテラルを表す文字列をLitオブジェクトに変換する．
    * リテラルを表す文字列は，英数字列またはその前に "~" を付けたものである．
    * 
    * たとえば `str` が "p" なら正リテラル Lit("p", true) が返される．
    * `str` が "~p" なら負リテラル Lit("p", false) を返される．
    * @param str CNFファイル中でリテラルを表す文字列
    * @return リテラル (Litオブジェクト)
    */
  def mkLit(str: String): Lit = str match {
    case "1" | "~0" => True
    case "~1" | "0" => False
    case _ if str.startsWith("~") => ~Lit(str.drop(1))
    case _ => Lit(str)
  }

  /** 
    * 文字列の列 ([[scala.collection.Seq Seq]])をLitオブジェクトの列 ([[scala.collection.Seq Seq]])に変換する．
    * 
    * たとえば `ss` が Seq("p", "~q") なら Seq(Lit("p",true), Lit("q",false)) が返される．
    * @param ss 文字列の列
    * @return リテラル (Litオブジェクト)の列
    */
  def mkLits(ss: Seq[String]): Seq[Lit] =
    ss.map(s => mkLit(s))

  /* 新しい変数を生成するためのカウンター */
  private var count = 0

  /** 
    * 新しい正リテラルを返す．
    * 
    * 変数名は順に "_1", "_2", ... などとなる．
    * @return 新しい正リテラル
    */
  def newLit: Lit = {
    count += 1
    Lit("_" + count)
  }

  /** 
    * 新しい正リテラルの列 ([[scala.collection.Seq Seq]])を返す．
    * 
    * 変数名は順に "_1_0", "_1_1", "_1_2", ..., "_2_0", "_2_1", ... などとなる．
    * @param n 新しい正リテラルの個数
    * @return 新しい正リテラルの列
    */
  def newLits(n: Int): Seq[Lit] = {
    count += 1
    (0 until n).map(i => Lit("_" + count + "_" + i))
  }
}

/**
  * CNFを表すケースクラス．
  * 内容を理解する必要はない．
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/CNF.scala CNF.scala]]
  */
case class CNF(clauses: Seq[Seq[Lit]]) {
  private val satSolver = new SatSolver()

  /**
    * 2つのCNFを結合したCNFを返す．
    *
    * @param that 結合するCNF
    * @return このCNFとthatを結合したCNF
    */
  def ++ (that: CNF): CNF =
    CNF(clauses ++ that.clauses)

  /**
    * 最初のモデルを探す．
    * モデルがあればtrueを返し，なければfalseを返す．
    *
    * @return モデルがあればtrue
    */
  def find: Boolean = {
    satSolver.reset
    satSolver.addClauses(clauses)
    satSolver.isSatisfiable
  }

  /**
    * モデルがなければtrueを返す．
    *
    * @return モデルがなければtrue
    */
  def unsat: Boolean =
    satSolver.unsat

  /**
    * 次ののモデルを探す．
    * 次のモデルがあればtrueを返し，なければfalseを返す．
    *
    * @return 次のモデルがあればtrue
    */
  def findNext: Boolean = {
    ! unsat && {
      satSolver.addClause(model.map(lit => ~lit))
      satSolver.isSatisfiable
    }
  }

  /**
    * モデルを返す．
    *
    * @return モデル
    */
  def model: Seq[Lit] =
    satSolver.model

  /**
    * 最大 limit 個のモデルを返す．
    *
    * @param limit 最大個数
    * @return モデルの列
    */
  def modelsAtMost(limit: Int): Seq[Seq[Lit]] = {
    var ms: Seq[Seq[Lit]] = Vector.empty
    if (find) {
      do {
        ms = ms :+ model
      } while ((limit <= 0 || ms.size < limit) && findNext)
    }
    ms
  }

  /**
    * すべてのモデルを返す．
    *
    * @return モデルの列
    */
  def models: Seq[Seq[Lit]] =
    modelsAtMost(0)

  /**
    * すべてのモデルの集合を返す．各モデルもリテラルの集合である．
    *
    * @return モデルの集合
    */
  def modelSets: Set[Set[Lit]] =
    models.map(model => model.toSet).toSet

  /**
    * CNFを表示する．
    */
  def show(): Unit = {
    var c = 0
    for (clause <- clauses) {
      c += 1
      println(clause.mkString(s"\tClause$c : ", " ", ""))
    }
  }

  /**
    * 最大 limit 個のモデルを表示する．0ならすべてを表示する．
    * 
    * @param limit 最大個数
    */
  def showModels(limit: Int): Unit = {
    var c = 0
    for (model <- modelsAtMost(limit)) {
      c += 1
      println(model.mkString(s"\tModel$c : ", " ", ""))
    }
  }

  /**
    * すべてのモデルを表示する．
    */
  def showModels(): Unit =
    showModels(0)
}

/**
  * CNF用のメソッドを提供するオブジェクト．
  * 内容を理解する必要はない．
  * @see [[http://bach.istc.kobe-u.ac.jp/lect/tfpl/prog/tfpl2019/src/main/scala/CNF.scala CNF.scala]]
  */
object CNF {
  import scala.io.Source

  /**
    * CNFを表す入力文字列の列をパースし，CNFを返す．
    *
    * @param lines 入力文字列の列
    * @return CNF
    */
  def load(lines: Seq[String]): CNF = {
    val cnf = for {
      line0 <- lines
      line = line0.trim
      if line != "" && line != "~" && ! line.startsWith("~ ")
      ss = line.split("""\s+""").toSeq
    } yield Lit.mkLits(ss)
    CNF(cnf)
  }

  /**
    * CNFファイルをパースし，CNFを返す．
    *
    * @param fileName CNFファイル名
    * @return CNF
    */
  def load(fileName: String): CNF =
    load(Source.fromFile(fileName).getLines.toSeq)

  /**
    * 2つのCNFファイルをパースし，CNFを返す．
    *
    * @param fileName1 1つ目のCNFファイル名
    * @param fileName2 2つ目のCNFファイル名
    * @return CNF
    */
  def load(fileName1: String, fileName2: String): CNF = {
    val lines1 = Source.fromFile(fileName1).getLines
    val lines2 = Source.fromFile(fileName2).getLines
    load((lines1 ++ lines2).toSeq)
  }
}
