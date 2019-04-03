package village1.search.cp.heuristic

import oscar.cp.core.variables.CPIntVar

class LastAssignedHeuristic(x: Array[CPIntVar]) {
  private val values: Array[Int] = Array.tabulate(x.length)(i => x(i).min - 1)

  def valueHeuristic(fallback: Int => Int): Int => Int = {
    def heuristic(i: Int): Int = {
      if (x(i).hasValue(values(i))) {
        values(i)
      }
      else {
        val v = fallback(i)
        values(i) = v
        v
      }
    }

    heuristic
  }
}
