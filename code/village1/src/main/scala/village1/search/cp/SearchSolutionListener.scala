package village1.search.cp

import village1.modeling.Solution

trait SearchSolutionListener {

  private var listeners: List[Solution => Unit] = List()

  def onSolutionFound(block: Solution => Unit): Unit = listeners = block :: listeners
  def emitSolution(solution: Solution): Unit = listeners.foreach(_(solution))
}
