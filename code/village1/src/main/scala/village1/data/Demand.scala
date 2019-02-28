package village1.data


// TODO: change periods with an array for better iterations
case class Demand(
   id: Int,
   client: Int,
   periods: Set[Int],
   requiredWorkers: Int,
   machineNeeds: Array[Machine] = Array(),
   possibleLocations: Set[Int] = Set(),
   requiredSkills: Array[Array[Skill]] = Array(),
   additionalSkills: Array[Skill] = Array()
) {

  val positions: Range = 0 until requiredWorkers

  val nWorkers: Int = requiredWorkers

  val requirements: Array[WorkerRequirement] = Array.tabulate(nWorkers)(i => {
    if (i < requiredSkills.length) {
      WorkerRequirement(skills = requiredSkills(i))
    }
    else {
      WorkerRequirement(skills = Array[Skill]())
    }
  })

  def hasPeriod (t: Int): Boolean = periods.contains(t)


  def overlapWith (demand: Demand): Boolean = {
    val (smaller, larger) =
      if (demand.periods.size < periods.size) (demand.periods, periods)
      else (periods, demand.periods)

    var overlap = false
    val iterator = smaller.iterator
    while (iterator.hasNext && !overlap) {
      overlap = larger.contains(iterator.next())
    }

    overlap
  }

  def worker (w: Int): WorkerRequirement = requirements(w)
}




