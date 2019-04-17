package village1.search.cp.relaxations

import oscar.cp.searches.lns.operators.RelaxationFunctions
import village1.search.cp.VillageOneLNS

class RandomRelaxation(search: VillageOneLNS, percentage: Double) extends Relaxation {
  private val size: Int = (search.flatWorkers.length * percentage).round.toInt
  private val solver = search.solver
  override def relax(): Unit = RelaxationFunctions.randomRelax(solver, search.flatWorkers, search.currentSolution, size)
}
