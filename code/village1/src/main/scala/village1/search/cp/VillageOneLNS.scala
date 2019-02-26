package village1.search.cp

import oscar.algo.search.SearchStatistics
import oscar.cp._
import oscar.cp.core.CPSol
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.RelaxationFunctions
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, VillageOneModel}
import village1.modeling.cp.VillageOneCPModel
import village1.util.Benchmark.time


class VillageOneLNS(problem: Problem, baseModel: Option[VillageOneModel] = None) extends VillageOneCPModel(problem, baseModel) with Search {

  def this(baseModel: VillageOneModel) = this(baseModel.problem, Some(baseModel))

  private def relaxShifts(percentage: Int, solution: Array[Array[Array[Int]]]) {
    val rand = new scala.util.Random(0)
    for (d <- Demands) {
      for (s <- 0 until demands(d).requiredWorkers if rand.nextInt(100) < percentage) {
        for (t <- demands(d).periods) {
          add(workerVariables(t)(d)(s) === solution(t)(d)(s))
        }
      }
    }
  }

  def solve(nSols: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, repeat: Int = 10): Long = {

    val flatWorkers: Array[CPIntVar] = workerVariables.flatten.flatten
    val flatMachines: Array[CPIntVar] = machineVariables.flatten
    val flatLocations: Array[CPIntVar] = locationVariables.filter(_ != null)
    val shifts: Array[CPIntVar] = shiftDifferences.flatten

    solver.addDecisionVariables(flatWorkers)
    solver.addDecisionVariables(flatMachines)
    solver.addDecisionVariables(flatLocations)
    
    search {

      var branching = binaryFirstFail(flatWorkers)

      if (flatMachines.nonEmpty) {
        branching = branching ++ binaryFirstFail(flatMachines)
      }

      if (flatLocations.nonEmpty) {
        branching = branching ++ binaryFirstFail(flatLocations)
      }

      branching = branching ++ binaryFirstFail(shifts)

      /*binarySplit(sameWorkerViolations) ++ */

      branching
    }

    var currentSolution: CPIntSol = null
    var workerSolution: Array[Array[Array[Int]]] = null
    onSolution {
      currentSolution = new CPIntSol(flatWorkers.map(_.value), objective.value, 0L)

      workerSolution = workerVariables.map {
        _.map(
          _.map(_.value)
        )
      }

      println(objective.value)
      // TODO avoid creating solutions if not necessary
      // - Save the values of variable and create solution from those when required
      emitSolution(createSolution())
    }

    time {
      var limit = 10
      var totalTime = 0L
      var totalSol = 0
      val rand = new scala.util.Random(0)

      val stat = start(nSols = 1)

      totalSol = stat.nSols
      totalTime += stat.time

      var r = 0

      while (r < repeat && totalTime < timeLimit && totalSol < nSols) {
        val stat = startSubjectTo(failureLimit = limit) {
          val percentage = 50

          relaxShifts(percentage, workerSolution)

         //RelaxationFunctions.randomRelax(solver, flatWorkers, currentSolution, flatWorkers.length / (100 / percentage))
        }

        totalSol += stat.nSols
        totalTime += stat.time
        limit = if (stat.completed) limit / 2 else limit * 2

        r += 1
      }

    }
  }
}

object MainLNS extends App {

  val folder = "data/instances"
  val instance = s"$folder/problem2.json"
  val generatedFolder = s"$folder/generated/"
  val generatedInstances: Array[String] = Array(
    "t5d5w20-491.json",
    "t10d50w300-638.json"
  ).map(f => s"$generatedFolder/$f")


  val problem = JsonParser.parse(generatedInstances(1))

  val search = new VillageOneLNS(problem)
  var nSolution = 0
  search.onSolutionFound( _ => nSolution += 1)
  val stats = search.solve(repeat = 50, timeLimit = 60 * 1000)


  println("nsolution " + nSolution)
  if (search.lastSolution != null) {
    JsonSerializer.serialize(search.lastSolution)("results/results3.json")
    println(search.lastSolution.valid)
  }

}