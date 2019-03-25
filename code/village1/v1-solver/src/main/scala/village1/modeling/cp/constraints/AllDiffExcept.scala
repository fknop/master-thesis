package village1.modeling.cp.constraints

import oscar.cp.constraints.{AllDiffExceptFWC, GCCFWC, GCCUpperBC}
import oscar.cp.core._
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
  * Alldifferent constraint
  * @author Pierre Schaus pschaus@gmail.com
  * @author Guillaume Derval guillaume.derval@uclouvain.be
  * @author Florian Knop (fixed some issues)
  */
class AllDiffExcept(x: Array[CPIntVar], exclude: Set[Int]) extends Constraint(x(0).store, "AllDiffExcept") {

  override def associatedVars(): Iterable[CPVar] = x

  /**
    * Post the constraint that for every pair of variables in x[i], x[j], we have x[i] != x[j] <br>
    * Available propagation strength are Weak (default) and Strong
    * @see CPPropagStrength
    */
  override def setup(l: CPPropagStrength): Unit = {
    val allValues = x.map(_.toSet).foldLeft(Set[Int]())((u,v) => u.union(v))
    val permutation = allValues.size == x.length

    s.post {
      if (permutation) {
        val minVal = allValues.min
        val maxVal = allValues.max
        val minCards = Array.tabulate(maxVal - minVal + 1)(v => {
          if (exclude.contains(v + minVal))
            0
          else if (allValues.contains(v + minVal))
            1
          else
            0
        })

        val maxCards = Array.tabulate(maxVal - minVal + 1)(v => {
          if (exclude.contains(v + minVal))
            Int.MaxValue
          else if (allValues.contains(v + minVal))
            1
          else
            0
        })

        new GCCFWC(x, minVal, minCards, maxCards)
      }
      else new AllDiffExceptFWC(x, exclude)
    }

    if(l != CPPropagStrength.Weak) {
      val minVal = x.map(_.min).min
      val maxVal = x.map(_.max).max
      val cards = Array.fill(maxVal - minVal + 1)(1)
      for(i <- exclude)
        cards(i - minVal) = x.length
      s.post(new GCCUpperBC(x, minVal, cards))
    }
  }
}
