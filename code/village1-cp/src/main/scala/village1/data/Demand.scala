package village1.data


case class Demand(
  id: Int,
  client: Client,
  periods: Set[Int],
  machines: Set[Int] = Set(),
  locations: Set[Int] = Set(),
  workersRequirements: IndexedSeq[WorkerRequirement],
) {

  val workers: Int = workersRequirements.length

  def hasPeriod (t: Int): Boolean = periods.contains(t)

  def worker (w: Int): WorkerRequirement = workersRequirements(w)
}
