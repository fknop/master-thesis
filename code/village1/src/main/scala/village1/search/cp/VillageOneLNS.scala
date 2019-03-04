package village1.search.cp


import oscar.cp._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.RelaxationFunctions
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.cp.{CPModelOptions, VillageOneCPModel}
import village1.modeling.{Problem, VillageOneModel}
import village1.util.Benchmark.time
import village1.util.Utilities

object SearchHeuristic extends Enumeration {
  val MostAvailable, Default = Value
}

trait Relaxation {
  def apply(): Unit
}

class VillageOneLNS(problem: Problem, options: CPModelOptions = CPModelOptions(), base: Option[VillageOneModel] = None) extends VillageOneCPModel(problem, options, base) with Search {

  def this(base: VillageOneModel) = this(problem = base.problem, base = Some(base))
  def this(base: VillageOneModel, options: CPModelOptions) = this(base.problem, options, Some(base))
//
//  private def relaxShifts(variables: Array[CPIntVar], sol: CPIntSol, percentage: Int/* solution: Array[Array[Array[Int]]])*/) {
//    val rand = new scala.util.Random(0)
//    for (d <- Demands) {
//      for (s <- 0 until demands(d).requiredWorkers if rand.nextInt(100) < percentage) {
//        for (t <- demands(d).periods) {
//
//          //t * d + s
//          add(variables((t * d) + s) === sol.values((t * d) + s))
////          add(workerVariables(t)(d)(s) === solution(t)(d)(s))
//        }
//      }
//    }
//  }

  val flatWorkers: Array[CPIntVar] = workerVariables.flatten.flatten
  val flatMachines: Array[CPIntVar] = machineVariables.flatten
  val flatLocations: Array[CPIntVar] = locationVariables.filter(_ != null)

  solver.addDecisionVariables(flatWorkers)
  solver.addDecisionVariables(flatMachines)
  solver.addDecisionVariables(flatLocations)

  var currentSolution: CPIntSol = _
  var bestObjective: Int = Int.MaxValue

  private var relaxation: Relaxation = () => RelaxationFunctions.randomRelax(solver, flatWorkers, currentSolution, flatWorkers.length / 2)

  def relax(relaxation: Relaxation): Unit = {
    this.relaxation = relaxation
  }

  def solve(
     nSols: Int = Int.MaxValue,
     timeLimit: Int = Int.MaxValue,
     repeat: Int = Int.MaxValue,
     silent: Boolean = false,
     heuristic: SearchHeuristic.Value = SearchHeuristic.MostAvailable
   ): Long = {

    solver.silent = silent

    minimize(objective)
    search {
      var branching = heuristic match {
        case SearchHeuristic.Default => binaryFirstFail(flatWorkers)
        case SearchHeuristic.MostAvailable => new MostAvailableHeuristic(this, flatWorkers).branching
      }

      if (flatMachines.nonEmpty) {
        branching ++= binaryFirstFail(flatMachines)
      }

      if (flatLocations.nonEmpty) {
        branching ++= binaryFirstFail(flatLocations)
      }

      branching
    }

    onSolution {
      currentSolution = new CPIntSol(flatWorkers.map(_.value), objective.value, 0L)
      bestObjective = objective.value
      emitSolution(createSolution())
    }

    time {
      var limit = 1000
      var totalTime = 0L
      var totalSol = 0

      val stat = start(nSols = 1, timeLimit / 1000)

      totalSol = stat.nSols
      totalTime += stat.time

      var r = 0

      var found = true

      while (bestObjective > objective.min && r < repeat && totalTime < timeLimit && totalSol < nSols) {
        val remainingTime = timeLimit - totalTime
        val stat = startSubjectTo(failureLimit = limit, timeLimit = (remainingTime / 1000).toInt) {
          val percentage = 50
          relaxation()
//          RelaxationFunctions.randomRelax(solver, flatWorkers, currentSolution, flatWorkers.length / 2)
        }

        totalSol += stat.nSols
        totalTime += stat.time

        if (timeLimit - totalTime < 1000) {
          totalTime = timeLimit
        }

        found = stat.nSols > 0
        limit =
          if (stat.completed || found)
            limit / 2
          else
            if (limit * 2 < 0)
              Int.MaxValue
            else
              limit * 2

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
  )

  val generatedInstancesPath = generatedInstances.map(f => s"$generatedFolder/$f")

  val id = 1
  val path = generatedInstancesPath(id)
  val name = generatedInstances(id)


  val problem = JsonParser.parse(path)

  val search = new VillageOneLNS(problem, CPModelOptions(symmetryBreaking = true))

  search.relax {
    () => RelaxationFunctions.propagationGuidedRelax(search.solver, search.flatWorkers, search.currentSolution, search.flatWorkers.length / 3)
  }

//  var nSolution = 0
//  search.onSolutionFound( _ => nSolution += 1)
  val stats = search.solve(timeLimit = 60 * 1000)


//  println("nsolution " + nSolution)
  val solution = search.lastSolution
  if (solution != null) {
    JsonSerializer.serialize(solution)(s"data/results/$name-o=${solution.objective}.json")
    println(solution.valid)
  }

}

