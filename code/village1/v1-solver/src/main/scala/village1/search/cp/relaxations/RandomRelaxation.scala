package village1.search.cp.relaxations

import oscar.cp.searches.lns.operators.RelaxationFunctions
import village1.search.cp.VillageOneLNS

class RandomRelaxation(search: VillageOneLNS) extends Relaxation {
  private val solver = search.solver
  private val flatWorkers = search.flatWorkers
  override def relax(): Unit = RelaxationFunctions.randomRelax(solver, flatWorkers, search.currentSolution, flatWorkers.length / 2)
}
