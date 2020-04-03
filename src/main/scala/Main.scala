/**
  * 課題の実行およびテスト用のプログラム．内容を理解する必要はない．
  */
protected object Main {
  var dataDir = "data" + java.io.File.separator
  var dataDir2 = "data_tmp" + java.io.File.separator
  var verbosity = 0
  val allWorks = Seq(
    cnf1_or, cnf1_and, cnf1_xor, cnf1_equiv, cnf1_atleast1_3, cnf1_atmost1_3, cnf1_exact1_3,
    cnf2_exact1_4, cnf2_atleast1, cnf2_atmost1, cnf2_exact1,
    cnf3_sudoku, cnf3_nfa,
    bchain1_halfadder, bchain1_fulladder, bchain1_3p3, bchain1_3x2, bchain1_3x3, bchain1_queens4,
    bchain2_halfadder, bchain2_fulladder, bchain2_4p4, bchain2_4gt4, bchain2_add, bchain2_3p3p3,
    bchain3_mul, bchain3_queens, bchain3_lightsout
  )
  val allWorkNames = allWorks.map(work => work.toString)
  val allWorkMaps = allWorks.map(work => work.toString -> work).toMap
  def totalScore(): Unit = {
    val ss = allWorks.map(_.level).groupBy(identity).map(kv => kv._1 -> kv._2.size)
    var score = 0
    for (l <- ss.keys.toSeq.sorted) {
      val s = (1 << l) * ss(l)
      println(s"Level $l : ${ss(l)} *  ${1 << l} = $s")
      score += s
    }
    println(s"Total = $score")
  }
  def run(work: String): Unit = {
    if (allWorkMaps.contains(work)) {
      allWorkMaps(work).run
    } else {
      val works = allWorkNames.filter(_.startsWith(work))
      if (works.isEmpty)
        println(s"不明な課題名 $work")
      else
        run(works)
    }
  }
  def run(works: Seq[String]): Unit = {
    if (works.isEmpty)
      run(allWorkNames)
    else
      works.foreach(work => run(work))
  }
  def main(args: Array[String]): Unit = {
    var i = 0
    if (args.size > i && args(0).matches("-v\\d*")) {
      verbosity = if (args(0) == "-v") 1 else args(0).drop(2).toInt
      i += 1
    }
    if (args.size > i && args(i) == "-l") {
      for (work <- allWorks)
        println(work.title)
      if (verbosity > 0)
        totalScore
    } else {
      run(args.drop(i))
    }
  }
}

protected abstract class RunWork {
  val work: String
  val level: Int
  val title: String
  def dataFile = s"${Main.dataDir}$work.txt"
  def dataFile2 = s"${Main.dataDir2}$work.txt"
  def testFile = s"${Main.dataDir}${work}_test.txt"
  def cnf1Title(formula: String) =
    s"課題 $work : $formula を表すCNFをファイル $dataFile に記述せよ (Level $level)"
  def cnf2Title(formula: String) =
    s"課題 $work : $formula を表すCNFを出力する関数 $work を WorkCNF.scala 中に記述せよ (Level $level)"
  def bchain1Title(formula: String) =
    s"課題 $work : $formula を表すブール連鎖をファイル $dataFile に記述せよ (Level $level)"
  def bchain2Title(formula: String) =
    s"課題 $work : $formula を表すブール連鎖を出力する関数 $work を WorkBchain.scala 中に記述せよ (Level $level)"
  val numModels: Int
  val checkNumModels = true
  def checkModel(model: Set[Lit]): Boolean
  var err = 0
  def error(msg: String): Unit = {
    println("  ERROR : " + msg)
    err += 1
  }
  def info(msg: String): Unit = {
    if (Main.verbosity >= 1)
      println(msg)
  }
  def p(i: Int) = Lit("p" + i)
  def q(i: Int) = Lit("q" + i)
  def r(i: Int) = Lit("r" + i)
  def w(i: Int) = Lit("w" + i)
  def x(i: Int) = Lit("x" + i)
  def y(i: Int) = Lit("y" + i)
  def z(i: Int) = Lit("z" + i)
  def ps(n: Int) = (0 until n).map(p(_))
  def qs(n: Int) = (0 until n).map(q(_))
  def rs(n: Int) = (0 until n).map(r(_))
  def ws(n: Int) = (0 until n).map(w(_))
  def xs(n: Int) = (0 until n).map(x(_))
  def ys(n: Int) = (0 until n).map(y(_))
  def zs(n: Int) = (0 until n).map(z(_))

  def containsAll(lits: Seq[Lit], model: Set[Lit]): Boolean =
    lits.forall(lit => model.contains(lit) || model.contains(~lit))
  def value(lit: Lit, model: Set[Lit]): Boolean =
    model.contains(lit)
  def ivalue(lit: Lit, model: Set[Lit]): Int =
    if (value(lit, model)) 1 else 0
  def binvalue(lits: Seq[Lit], model: Set[Lit]): Int =
    (0 until lits.size).map(i => ivalue(lits(i), model) << i).sum
  def bc(lits: Seq[Lit], model: Set[Lit]): Int =
    lits.map(lit => ivalue(lit, model)).sum

  def output(fileName: String)(block: => Unit): Unit = {
    val out = new java.io.PrintStream(fileName)
    Console.withOut(out) {
      println(s"~ 課題プログラム $work により作成されたファイル")
      block
    }
    out.close
  }
  def getCNF: CNF
  def run(): Unit = {
    try {
      println(title)
      val cnf = getCNF
      if (Main.verbosity >= 1) {
        info(s"  $work のCNF")
        cnf.show
      }
      if (cnf.clauses.isEmpty)
        throw new RuntimeException("空のCNF")
      if (Main.verbosity >= 1) {
        info(s"  $work のモデル")
        cnf.showModels(numModels)
      }
      val models = cnf.modelsAtMost(numModels)
      var c = 0
      for (model <- models) {
        c += 1
        if (! checkModel(model.toSet)) {
          error(s"間違ったModel$c : " + model.mkString(" "))
        }
      }
      if (checkNumModels) {
        if (models.size < numModels) {
          error(s"モデルの個数が $numModels 個より少ない")
        } else if (! cnf.unsat && cnf.findNext) {
          error(s"モデルの個数が $numModels 個より多い")
        } else {
          info("  " + models.size + " 個のモデル")
        }
      } else {
        if (models.size < numModels) {
          error(s"モデルの個数が $numModels 個より少ない")
        } else {
          info("  " + models.size + " 個以上のモデル")
        }
      }
    } catch {
      case e: Throwable => {
        error(e.toString)
        for {
          t <- e.getStackTrace; s = t.toString
          if s.startsWith("Work")
        } println("\t" +  s)
        if (Main.verbosity >= 1)
          e.printStackTrace
      }
    }
    if (err == 0)
      println(s"課題 $work : SUCCESS")
    else
      println(s"課題 $work : ERROR")
    println
  }
}

/*
 * 課題 cnf1
 */

protected case object cnf1_or extends RunWork {
  val work = toString
  val level = 0
  val title = cnf1Title("(p0 or p1)")
  def getCNF = CNF.load(dataFile)
  val numModels = 3
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(2), model) &&
    (value(p(0), model) || value(p(1), model))
}

protected case object cnf1_and extends RunWork {
  val work = toString
  val level = 0
  val title = cnf1Title("(p0 and p1)")
  def getCNF = CNF.load(dataFile)
  val numModels = 1
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(2), model) &&
    (value(p(0), model) && value(p(1), model))
}

protected case object cnf1_xor extends RunWork {
  val work = toString
  val level = 0
  val title = cnf1Title("(p0 xor p1)")
  def getCNF = CNF.load(dataFile)
  val numModels = 2
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(2), model) &&
    (value(p(0), model) ^ value(p(1), model))
}

protected case object cnf1_equiv extends RunWork {
  val work = toString
  val level = 1
  val title = cnf1Title("(p0 <=> p1)")
  def getCNF = CNF.load(dataFile)
  val numModels = 2
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(2), model) &&
    (value(p(0), model) == value(p(1), model))
}

protected case object cnf1_atleast1_3 extends RunWork {
  val work = toString
  val level = 1
  val title = cnf1Title("(p0+p1+p2 >= 1)")
  val n = 3
  def getCNF = CNF.load(dataFile)
  val numModels = (1 << n) - 1
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(n), model) &&
    bc(ps(n), model) >= 1
}

protected case object cnf1_atmost1_3 extends RunWork {
  val work = toString
  val level = 1
  val title = cnf1Title("(p0+p1+p2 <= 1)")
  val n = 3
  def getCNF = CNF.load(dataFile)
  val numModels = n + 1
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(n), model) &&
    bc(ps(n), model) <= 1
}

protected case object cnf1_exact1_3 extends RunWork {
  val work = toString
  val level = 1
  val title = cnf1Title("(p0+p1+p2 = 1)")
  val n = 3
  def getCNF = CNF.load(dataFile)
  val numModels = n
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(n), model) &&
    bc(ps(n), model) == 1
}

/*
 * 課題 cnf2
 */

protected case object cnf2_exact1_4 extends RunWork {
  val work = toString
  val level = 1
  val title = cnf2Title("(p(0)+p(1)+p(2)+p(3) = 1)")
  val n = 4
  def getCNF = {
    output(dataFile2) { WorkCNF.cnf2_exact1_4(ps(n)) }
    CNF.load(dataFile2)
  }
  val numModels = n
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(n), model) &&
    bc(ps(n), model) == 1
}

protected case object cnf2_atleast1 extends RunWork {
  val work = toString
  val level = 2
  val title = cnf2Title("(p(0)+p(1)+...+p(n-1) >= 1)")
  val n = 8
  def getCNF(n: Int) = {
    output(dataFile2) { WorkCNF.cnf2_atleast1(ps(n)) }
    CNF.load(dataFile2)
  }
  def getCNF = getCNF(n)
  val numModels = (1 << n) - 1
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(n), model) &&
    bc(ps(n), model) >= 1
}

protected case object cnf2_atmost1 extends RunWork {
  val work = toString
  val level = 2
  val title = cnf2Title("(p(0)+p(1)+...+p(n-1) <= 1)")
  val n = 8
  def getCNF(n: Int) = {
    output(dataFile2) { WorkCNF.cnf2_atmost1(ps(n)) }
    CNF.load(dataFile2)
  }
  def getCNF = getCNF(n)
  val numModels = n + 1
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(n), model) &&
    bc(ps(n), model) <= 1
}

protected case object cnf2_exact1 extends RunWork {
  val work = toString
  val level = 2
  val title = cnf2Title("(p(0)+p(1)+...+p(n-1) = 1)")
  val n = 8
  def getCNF(n: Int) = {
    output(dataFile2) { WorkCNF.cnf2_exact1(ps(n)) }
    CNF.load(dataFile2)
  }
  def getCNF = getCNF(n)
  val numModels = n
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(ps(n), model) &&
    bc(ps(n), model) == 1
}

/*
 * 課題 cnf3
 */

protected case object cnf3_sudoku extends RunWork {
  val work = toString
  val level = 3
  val title = s"課題 $work : 数独を解くためのCNFを出力する関数 $work を WorkSudoku.scala 中に記述せよ (Level $level)"
  def getCNF = {
    output(dataFile2) { WorkSudoku.cnf3_sudoku }
    CNF.load(dataFile2)
  }
  val numModels = 1
  // override val checkNumModels = false
  val m = 3; val n = 9
  def p(i: Int, j: Int, a: Int) = Lit("p" + i + "_" + j + "_" + a)
  def getAnswer(model: Seq[Lit]): Seq[Seq[Int]] =
    for (i <- 1 to n) yield {
      for (j <- 1 to n) yield {
        (1 to n).find(a => model.contains(p(i,j,a))).getOrElse(0)
      }
    }
  def checkModel(model: Set[Lit]): Boolean = {
    def eq1(lits: Seq[Lit]): Boolean = bc(lits, model) == 1
    val check0 =
      for (i <- 1 to n; j <- 1 to n; a = WorkSudoku.puzzle(i-1)(j-1); if a > 0)
      yield ivalue(p(i,j,a), model) == 1
    val check1 =
      for (i <- 1 to n; j <- 1 to n)
      yield eq1((1 to n).map(a => p(i,j,a)))
    val check2 =
      for (i <- 1 to n; a <- 1 to n)
      yield eq1((1 to n).map(j => p(i,j,a)))
    val check3 =
      for (j <- 1 to n; a <- 1 to n)
      yield eq1((1 to n).map(i => p(i,j,a)))
    val check4 =
      for (i <- 1 to n by m; j <- 1 to n by m; a <- 1 to n)
      yield {
        val lits = for (di <- 0 until m; dj <- 0 until m)
                   yield p(i+di,j+dj,a)
        eq1(lits)
      }
    check0.forall(identity) &&
    check1.forall(identity) &&
    check2.forall(identity) &&
    check3.forall(identity) &&
    check4.forall(identity)
  }
}

protected case object cnf3_nfa extends RunWork {
  val work = toString
  val level = 3
  val title = s"課題 $work : NFAに対応するCNFを出力する関数 $work を WorkNFA.scala 中に記述せよ (Level $level)"
  val n = 8
  def getCNF = {
    output(dataFile2) { WorkNFA.cnf3_nfa(xs(n)) }
    CNF.load(dataFile2)
  }
  val numModels = 128
  def checkModel(model: Set[Lit]): Boolean = {
    containsAll(xs(n), model) &&
    ivalue(x(0), model) == 1
  }
}

/*
 * 課題 bchain1
 */

protected case object bchain1_halfadder extends RunWork {
  val work = toString
  val level = 0
  val n = 1
  val title = bchain1Title(s"(z@${n+1} = x@$n + y@$n)")
  def getCNF = Bchain.load(dataFile).toCNF
  val numModels = 1 << (2*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(zs(n+1), model) &&
    (binvalue(xs(n), model) + binvalue(ys(n), model) == binvalue(zs(n+1), model))
}

protected case object bchain1_fulladder extends RunWork {
  val work = toString
  val level = 0
  val n = 1
  val title = bchain1Title(s"(z@${n+1} = x@$n + y@$n + w@$n)")
  def getCNF = Bchain.load(dataFile).toCNF
  val numModels = 1 << (3*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(ws(n), model) &&
    containsAll(zs(n+1), model) &&
    (binvalue(xs(n), model) + binvalue(ys(n), model) + binvalue(ws(n), model) == binvalue(zs(n+1), model))
}

protected case object bchain1_3p3 extends RunWork {
  val work = toString
  val level = 1
  val n = 3
  val title = bchain1Title(s"(z@${n+1} = x@$n + y@$n)")
  def getCNF = Bchain.load(dataFile).toCNF
  val numModels = 1 << (2*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(zs(n+1), model) &&
    (binvalue(xs(n), model) + binvalue(ys(n), model) == binvalue(zs(n+1), model))
}

protected case object bchain1_3x2 extends RunWork {
  val work = toString
  val level = 2
  val n1 = 3
  val n2 = 2
  val title = bchain1Title(s"(z@${n1+n2} = x@$n1 * y@$n2)")
  def getCNF = Bchain.load(dataFile).toCNF
  val numModels = 1 << (n1 + n2)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n1), model) &&
    containsAll(ys(n2), model) &&
    containsAll(zs(n1+n2), model) &&
  (binvalue(xs(n1), model) * binvalue(ys(n2), model) == binvalue(zs(n1+n2), model))
}

protected case object bchain1_3x3 extends RunWork {
  val work = toString
  val level = 3
  val n = 3
  val title = bchain1Title(s"(z@${2*n} = x@$n * y@$n)")
  def getCNF = Bchain.load(dataFile).toCNF
  val numModels = 1 << (2*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(zs(2*n), model) &&
    (binvalue(xs(n), model) * binvalue(ys(n), model) == binvalue(zs(2*n), model))
}

protected case object bchain1_queens4 extends RunWork {
  val work = toString
  val level = 2
  val n = 4
  val title = bchain1Title(s"$n-クイーン問題")
  def getCNF = Bchain.load(dataFile).toCNF
  val numModels = 2
  val p = (0 until n).map(i => (0 until n).map(j => Lit(s"p${i}_$j")))
  def getAnswer(model: Seq[Lit]): Seq[Seq[Int]] =
    for (i <- 0 until n) yield {
      for (j <- 0 until n) yield {
        if (model.contains(p(i)(j))) 1 else 0
      }
    }
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(p.flatten, model) &&
    (0 until n).forall(i => {
      val ps = for (j <- 0 until n) yield p(i)(j)
      bc(ps, model) == 1
    }) &&
    (0 until n).forall(j => {
      val ps = for (i <- 0 until n) yield p(i)(j)
      bc(ps, model) == 1
    }) &&
    (0+0 to (n-1)+(n-1)).forall(u => {
      val ps = for (i <- 0 until n; j = u-i; if 0 <= j && j < n) yield p(i)(j)
      bc(ps, model) <= 1
    }) &&
    (0-(n-1) to (n-1)-0).forall(d => {
      val ps = for (i <- 0 until n; j = i-d; if 0 <= j && j < n) yield p(i)(j)
      bc(ps, model) <= 1
    })
}

/*
 * 課題 bchain2
 */

protected case object bchain2_halfadder extends RunWork {
  val work = toString
  val level = 1
  val n = 1
  val title = bchain2Title(s"((z1z0) = x0 + y0)")
  def getCNF = {
    output(dataFile2) { WorkBchain.bchain2_halfadder(z(1), z(0), x(0), y(0)) }
    Bchain.load(dataFile2).toCNF
  }
  val numModels = 1 << (2*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(zs(n+1), model) &&
    (binvalue(xs(n), model) + binvalue(ys(n), model) == binvalue(zs(n+1), model))
}

protected case object bchain2_fulladder extends RunWork {
  val work = toString
  val level = 1
  val n = 1
  val title = bchain2Title(s"((z1z0) = x0 + y0 + w0)")
  def getCNF = {
    output(dataFile2) { WorkBchain.bchain2_fulladder(z(1), z(0), x(0), y(0), w(0)) }
    Bchain.load(dataFile2).toCNF
  }
  val numModels = 1 << (3*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(ws(n), model) &&
    containsAll(zs(n+1), model) &&
    (binvalue(xs(n), model) + binvalue(ys(n), model) + binvalue(ws(n), model) == binvalue(zs(n+1), model))
}

protected case object bchain2_4p4 extends RunWork {
  val work = toString
  val level = 1
  val n = 4
  val title = bchain2Title(s"(z@${n+1} = x@$n + y@$n)")
  def getCNF = {
    output(dataFile2) { WorkBchain.bchain2_4p4(zs(n+1), xs(n), ys(n)) }
    Bchain.load(dataFile2).toCNF
  }
  val numModels = 1 << (2*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(zs(n+1), model) &&
    (binvalue(xs(n), model) + binvalue(ys(n), model) == binvalue(zs(n+1), model))
}

protected case object bchain2_4gt4 extends RunWork {
  val work = toString
  val level = 2
  val n = 4
  val title = bchain2Title(s"(x@$n > y@$n)")
  def getCNF = {
    output(dataFile2) { WorkBchain.bchain2_4gt4(xs(n), ys(n)) }
    Bchain.load(dataFile2).toCNF
  }
  val numModels = 120
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    (binvalue(xs(n), model) > binvalue(ys(n), model))
}

protected case object bchain2_add extends RunWork {
  val work = toString
  val level = 2
  val n = 6
  val title = bchain2Title("(z@(n+1) = x@n + y@n)")
  def getCNF(n: Int) = {
    output(dataFile2) { WorkBchain.bchain2_add(zs(n+1), xs(n), ys(n)) }
    Bchain.load(dataFile2).toCNF
  }
  def getCNF = getCNF(n)
  val numModels = 1 << (2*n)
  // override val checkNumModels = false
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(zs(n+1), model) &&
    (binvalue(xs(n), model) + binvalue(ys(n), model) == binvalue(zs(n+1), model))
}

protected case object bchain2_3p3p3 extends RunWork {
  val work = toString
  val level = 2
  val n = 3
  val title = bchain2Title(s"(z@${n+2} = x@$n + y@$n + w@$n)")
  def getCNF = {
    output(dataFile2) { WorkBchain.bchain2_3p3p3(zs(n+2), xs(n), ys(n), ws(n)) }
    Bchain.load(dataFile2).toCNF
  }
  val numModels = 1 << (3*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(ws(n), model) &&
    containsAll(zs(n+2), model) &&
    (binvalue(xs(n), model) + binvalue(ys(n), model) + binvalue(ws(n), model) == binvalue(zs(n+2), model))
}

/*
 * 課題 bchain3
 */

protected case object bchain3_mul extends RunWork {
  val work = toString
  val level = 4
  val n = 4
  val title = s"課題 $work : (z@(m+n) = x@m * y@n) を表すブール連鎖を出力する関数 $work を WorkMul.scala 中に記述せよ (Level $level)"
  def getCNF(n: Int) = {
    output(dataFile2) { WorkMul.bchain3_mul(zs(2*n), xs(n), ys(n)) }
    Bchain.load(dataFile2).toCNF
  }
  def getCNF = getCNF(n)
  val numModels = 1 << (2*n)
  def checkModel(model: Set[Lit]): Boolean =
    containsAll(xs(n), model) &&
    containsAll(ys(n), model) &&
    containsAll(zs(2*n), model) &&
    (binvalue(xs(n), model) * binvalue(ys(n), model) == binvalue(zs(2*n), model))
}

protected case object bchain3_queens extends RunWork {
  val work = toString
  val level = 3
  val n = 8
  val title = s"課題 $work : n-クイーン問題 を解くためのブール連鎖を出力する関数 $work を WorkQueens.scala 中に記述せよ (Level $level)"
  def getCNF(n: Int) = {
    output(dataFile2) { WorkQueens.bchain3_queens(n) }
    Bchain.load(dataFile2).toCNF
  }
  def getCNF = getCNF(n)
  val numModels = 92
  def p(i: Int, j: Int) = Lit(s"p${i}_$j")
  def getAnswer(n: Int, model: Seq[Lit]): Seq[Seq[Int]] =
    for (i <- 0 until n) yield {
      for (j <- 0 until n) yield {
        if (model.contains(p(i,j))) 1 else 0
      }
    }
  def checkModel(model: Set[Lit]): Boolean = {
    val ps = for (i <- 0 until n; j <- 0 until n) yield p(i,j)
    containsAll(ps, model) &&
    (0 until n).forall(i => {
      val ps = for (j <- 0 until n) yield p(i,j)
      bc(ps, model) == 1
    }) &&
    (0 until n).forall(j => {
      val ps = for (i <- 0 until n) yield p(i,j)
      bc(ps, model) == 1
    }) &&
    (0+0 to (n-1)+(n-1)).forall(u => {
      val ps = for (i <- 0 until n; j = u-i; if 0 <= j && j < n) yield p(i,j)
      bc(ps, model) <= 1
    }) &&
    (0-(n-1) to (n-1)-0).forall(d => {
      val ps = for (i <- 0 until n; j = i-d; if 0 <= j && j < n) yield p(i,j)
      bc(ps, model) <= 1
    })
  }
}

protected case object bchain3_lightsout extends RunWork {
  val work = toString
  val level = 4
  val title = s"課題 $work : ライツアウト を解くためのブール連鎖を出力する関数 $work を WorkLightsOut.scala 中に記述せよ (Level $level)"
  val steps = 15
  val n = 5
  def getCNF = {
    output(dataFile2) { WorkLightsOut.bchain3_lightsout(steps) }
    Bchain.load(dataFile2).toCNF
  }
  val numModels = 1
  override val checkNumModels = false
  def checkModel(model: Set[Lit]): Boolean = {
    val qs = for (t <- 0 until steps; i <- 0 until n; j <- 0 until n)
             yield WorkLightsOut.q(t,i,j)
    containsAll(qs, model) && {
      for (t <- 0 until steps) {
        val q =  model.filter(_.positive).filter(_.name.startsWith(s"q${t}_"))
        println(s"ステップ$t : " + q.mkString(" "))
      }
      val p = Array.fill(n)(Array.fill(n)(1))
      for {
        t <- 0 until steps; i0 <- 0 until n; j0 <- 0 until n
        if value(WorkLightsOut.q(t,i0,j0), model)
      } {
        for {
          (i,j) <- Seq((i0,j0), (i0-1,j0), (i0+1,j0), (i0,j0-1), (i0,j0+1))
          if 0 <= i && i < n && 0 <= j && j < n
        } p(i)(j) = 1 - p(i)(j)
      }
      p.forall(_.forall(_ == 0))
    }
  }
}

