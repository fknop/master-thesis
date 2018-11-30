package village1.data

case class Demand(
  id: Int,
  periods: Set[Int],
  workers: Int,
  vehicles: Int = 0,
  zones: Set[Int] = Set()
) {

  def hasPeriod (t: Int) = periods.contains(t)
}
