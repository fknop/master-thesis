package village1.search.cp

import oscar.algo.search.SearchStatistics
import village1.modeling.Solution

trait Search {
  private var listeners: List[Solution => Unit] = List()
  private[this] var solution: Solution = _

  def onSolutionFound(block: Solution => Unit): Unit = listeners = block :: listeners
  protected def emitSolution(solution: Solution): Unit = {
    this.solution = solution
    listeners.foreach(_(solution))
  }

  def lastSolution: Solution = solution
}
