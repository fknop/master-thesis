package village1.search.cp.heuristic

import oscar.algo.search.Branching

trait Heuristic {
  def branching: Branching

  def onSolution(): Unit = {}
  def onRepeat(): Unit = {}
}
