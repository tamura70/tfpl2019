/**
  * SAT solver の Sat4j を利用するクラス．内容を理解する必要はない．
  */
protected class SatSolver {
  import scala.collection.mutable.Map
  import org.sat4j.specs.ISolver
  import org.sat4j.specs.ContradictionException
  import org.sat4j.core.VecInt
  import org.sat4j.minisat.SolverFactory

  val sat4j: ISolver = SolverFactory.newDefault
  val True = Lit.True
  val False = Lit.False
  val var2num: Map[String,Int] = Map.empty
  val num2var: Map[Int,String] = Map.empty
  var nVars = 0
  var unsat = false

  def reset(): Unit = {
    var2num.clear; num2var.clear; nVars = 0; unsat = false
    sat4j.reset
  }
  def toNum(lit: Lit): Int = {
    val v = lit.name
    val num = var2num.getOrElse(v, {
      nVars += 1
      var2num += v -> nVars; num2var += nVars -> v
      nVars
    })
    if (lit.positive) num else -num
  }
  def toNumClause(clause: Seq[Lit]): Seq[Int] = {
    for {
      lit <- clause
      if lit != False
    } yield toNum(lit)
  }
  def addClause(clause: Seq[Lit]): Unit = {
    if (! clause.contains(True)) {
      val nums = toNumClause(clause)
      if (nums.isEmpty) {
        unsat = true
      } else {
        try {
          sat4j.addClause(new VecInt(nums.toArray))
        } catch {
          case e: ContradictionException =>
            unsat = true
        }
      }
    } 
  }
  def addClauses(clauses: Seq[Seq[Lit]]): Unit = {
    for (clause <- clauses)
      addClause(clause)
  }
  def isSatisfiable: Boolean =
    ! unsat && sat4j.isSatisfiable
  def model: Seq[Lit] = {
    val m = for (num <- sat4j.model.toSeq) yield {
      if (num < 0) ~Lit(num2var(-num))
      else Lit(num2var(num))
    }
    m.sortBy(_.name)
  }
}
