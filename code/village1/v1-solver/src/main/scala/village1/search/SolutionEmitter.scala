package village1.search

import village1.modeling.Solution

trait SolutionEmitter {
  private var listeners: List[Solution => Unit] = List()
  private[this] var solution: Option[Solution] = None

  def onSolutionFound(block: Solution => Unit): Unit = listeners = block :: listeners
  protected def emitSolution(solution: Solution): Unit = {
    this.solution = Some(solution)
    listeners.foreach(_(solution))
  }

  def lastSolution: Option[Solution] = solution
}
