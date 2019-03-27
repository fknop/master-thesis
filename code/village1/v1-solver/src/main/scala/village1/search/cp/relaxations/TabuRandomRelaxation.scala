package village1.search.cp.relaxations

import oscar.algo.Inconsistency
import village1.search.cp.VillageOneLNS

import scala.collection.mutable
import scala.util.Random

class TabuRandomRelaxation(search: VillageOneLNS, t: Int, k: Double) extends Relaxation {
  private val solver = search.solver
  private val flatWorkers = search.flatWorkers

  private val tabuList: Array[Int] = Array.fill(flatWorkers.length)(0)

  val tabuVars: mutable.Set[Int] = mutable.Set()

  override def relax(): Unit = {
    val vars = flatWorkers
    val varSeq = vars.toSeq
    val varArray = varSeq.indices.toArray //map to real indice of variable
    var boundStart = varArray.length //Elements of varArray from this index are bound

    var i = 0
    while (i < vars.length) {
      if (tabuList(i) >= 0 && tabuList(i) <= t && Random.nextDouble() <= k) {
        tabuVars += i

        val x = varArray(i)
        solver.add(varSeq(x) === search.currentSolution.values(x))
        if (!varSeq(x).isBound)
          throw Inconsistency

        //marking var as bound:
        boundStart -= 1
        varArray(i) = varArray(boundStart)
        varArray(boundStart) = x
      }

      i += 1
    }
    for (v <- tabuVars) {
      tabuList(v) += 1
      if (tabuList(v) > t) {
        tabuList(v) = 0
        tabuVars.remove(v)
      }
    }

    println(vars.count(_.isBound))
  }
}
