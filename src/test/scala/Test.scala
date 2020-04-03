import org.scalatest.FunSuite

class Test extends FunSuite {
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

  def check1(work: RunWork): Unit = {
    test (work.title) {
      work.run
      assert(work.err == 0)
    }
  }
  def check2(work: RunWork)(runBlock: => Unit): Unit = {
    test (work.title) {
      println(s"課題 $work の出力")
      runBlock
      work.run
      assert(work.err == 0)
    }
  }
}

class CNF1_Test extends Test {
  check1(cnf1_or)
  check1(cnf1_and)
  check1(cnf1_xor)
  check1(cnf1_equiv)
  check1(cnf1_atleast1_3)
  check1(cnf1_atmost1_3)
  check1(cnf1_exact1_3)
}

class CNF2_Test extends Test {
  check2(cnf2_exact1_4) {
    val p = Lit.lits("p", 4)
    WorkCNF.cnf2_exact1_4(p)
  }
  check2(cnf2_atleast1) {
    val p = Lit.lits("p", 8)
    WorkCNF.cnf2_atleast1(p)
  }
  check2(cnf2_atmost1) {
    val p = Lit.lits("p", 8)
    WorkCNF.cnf2_atmost1(p)
  }
  check2(cnf2_exact1) {
    val p = Lit.lits("p", 8)
    WorkCNF.cnf2_exact1(p)
  }
}

class CNF3_Test extends Test {
  check1(cnf3_sudoku)
  check2(cnf3_nfa) {
    val n = 8
    WorkNFA.cnf3_nfa(xs(n))
  }
}

class Bchain1_Test extends Test {
  check1(bchain1_halfadder)
  check1(bchain1_fulladder)
  check1(bchain1_3p3)
  check1(bchain1_3x2)
  check1(bchain1_3x3)
  check1(bchain1_queens4)
}

class Bchain2_Test extends Test {
  check2(bchain2_halfadder) {
    WorkBchain.bchain2_halfadder(z(1), z(0), x(0), y(0))
  }
  check2(bchain2_fulladder) {
    WorkBchain.bchain2_fulladder(z(1), z(0), x(0), y(0), w(0))
  }
  check2(bchain2_4p4) {
    val n = 4
    WorkBchain.bchain2_4p4(zs(n+1), xs(n), ys(n))
  }
  check2(bchain2_4gt4) {
    val n = 4
    WorkBchain.bchain2_4gt4(xs(n), ys(n))
  }
  check2(bchain2_add) {
    val n = 6
    WorkBchain.bchain2_add(zs(n+1), xs(n), ys(n))
  }
  check2(bchain2_3p3p3) {
    val n = 3
    WorkBchain.bchain2_3p3p3(zs(n+2), xs(n), ys(n), ws(n))
  }
}

class Bchain3_Test extends Test {
  check2(bchain3_mul) {
    val n = 4
    WorkMul.bchain3_mul(zs(2*n), xs(n), ys(n))
  }
  check2(bchain3_queens) {
    val n = 8
    WorkQueens.bchain3_queens(n)
  }
  check2(bchain3_lightsout) {
    val steps = 15
    WorkLightsOut.bchain3_lightsout(steps)
  }
}

