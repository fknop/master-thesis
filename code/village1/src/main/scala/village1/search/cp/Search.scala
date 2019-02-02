package village1.search.cp

import village1.modeling.Solution

trait Search {
  private var listeners: List[Solution => Unit] = List()

  def onSolutionFound(block: Solution => Unit): Unit = listeners = block :: listeners
  protected def emitSolution(solution: Solution): Unit = listeners.foreach(_(solution))

  def solve(): Unit
}
