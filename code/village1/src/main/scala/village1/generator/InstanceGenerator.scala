package village1.generator

import village1.data.{Client, Demand, Skill, Worker}
import village1.format.json.JsonSerializer
import village1.modeling.Problem
import village1.util.Utilities.{rand}

object InstanceGenerator {

  private def isTrue(probability: Double = 0.5): Boolean = math.random() < probability

  private def generateWorkforce(n: Int): Array[Worker] = {
    Array.tabulate(n)(i => Worker(id = i, name = s"Worker $i", availabilities = Set()))
  }

  private def takeSkill(skills: Array[Skill]): Skill = {
    skills(rand(0, skills.length - 1))
  }

  private def generateDemands(
      n: Int,
      maxWorkers: Int,
      clients: Int,
      skills: Array[Skill] = Array(),
      skillProbability: Double = 0
  ): Array[Demand] = {

    val demands = Array.fill[Demand](n)(null)

    for (i <- 0 until n) {
      val requiredWorkers = rand(1, maxWorkers)
      var requiredSkills = Array[Array[Skill]]()

      for (_ <- 0 until requiredWorkers) {
        if (isTrue(skillProbability)) {
          val skill = takeSkill(skills)
          requiredSkills :+= Array(skill)
        }
      }

      demands(i) = Demand(id = i, client = rand(0, 9), periods = Set(), requiredWorkers = requiredWorkers, requiredSkills = requiredSkills)
    }

    demands
  }

  private def updatePeriods(demand: Demand, periods: Set[Int]): Demand = {
    demand.copy(periods = periods)
  }

  private def generatePeriodForDemand(demand: Demand, t: Int, prob: Double): Demand = {
    val periods = (0 until t).foldLeft(Set[Int]()) {
      // Second condition avoid empty periods
      (acc, i) => if (isTrue(prob) || (acc.isEmpty && i == t - 1)) acc + i else acc
    }

    updatePeriods(demand, periods)
  }


  // TODO: make this more efficient and cleaner
  private def generateWorkersAvailabilities(workers: Array[Worker], demands: IndexedSeq[Demand], T: Int, skills: Array[Skill]): Array[Worker] = {
    for (d <- demands.indices) {
      for (t <- demands(d).periods) {
        var done = 0
        var i = rand(0, workers.length - 1)
        while (done < demands(d).requiredWorkers) {

          // Trick to avoid infinite loop for now
          val allAvailable = workers.forall(_.available(t))
          if (allAvailable) {
            done += 1
          }

          // This assure to be solvable
          if (!workers(i).available(t)) {
            workers(i) = workers(i).copy(availabilities = workers(i).availabilities + t)

            val requirements = demands(d).worker(done).skills
            val skills = requirements.foldLeft(workers(i).skills) {
              (current, skill) => current.updated(skill.name, skill)
            }

            workers(i) = workers(i).copy(skills = skills)

            done += 1
          }
          i = rand(0, workers.length - 1)
        }
      }

      // Add all availabilities to random workers (might be workers who already are available)
      var done = 0
      var i = rand(0, workers.length - 1)
      while (done < demands(d).requiredWorkers) {
        workers(i) = workers(i).copy(availabilities = workers(i).availabilities ++ demands(d).periods)
        done += 1
        i = rand(0, workers.length - 1)
      }

      // Add random skills to particular workers with random probability
      for (i <- workers.indices) {
        if (isTrue(0.2)) {
          val skill = takeSkill(skills)
          workers(i) = workers(i).copy(skills = workers(i).skills.updated(skill.name, skill))
        }
      }

    }

    workers
  }

  // Only generate simple skills for now (does not change much in the solver)
  private def generateSkills(s: Int) = Array.tabulate(s)(i => Skill(name = s"Skill $i"))

  private def generateClients(c: Int) = Array.tabulate(c)(i => Client(name = s"Client $i"))

  def generate(t: Int, c: Int, d: Int, w: Int, s: Int, prob: Map[String, Double] = Map()): Problem = {

    val clients = generateClients(c)
    val skills = generateSkills(s)
    val demands = generateDemands(
        n = d,
        maxWorkers = Math.min(d, Math.max(w / d, 1)),
        clients = c,
        skills = skills,
        skillProbability = prob.getOrElse("skill", 0.2)
      )
      .map(d => generatePeriodForDemand(d, t, prob = prob.getOrElse("period", 0.5)))


    val workforce = generateWorkforce(w)
    val workers = generateWorkersAvailabilities(workforce, demands, t, skills)


    Problem(
      T = t,
      workers = workers,
      demands = demands,
      clients = clients
    )
  }
}

object InstanceGeneratorApp extends App {

  val t = 10
  val c = 30
  val d = 50
  val w = 300
  val s = 20
  val prob = Map("skill" -> 0.2, "period" -> 0.6)

  val problem = InstanceGenerator.generate(
    t = t,
    c = c,
    d = d,
    w = w,
    s = s,
    prob = prob
  )

  val fileName = s"t${t}d${d}w${w}-${rand(0, 1000)}"
  val folder = "data/instances/generated/"
  JsonSerializer.serialize(problem)(s"$folder/$fileName.json")
}
