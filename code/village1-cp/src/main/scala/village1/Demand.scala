package village1

case class Demand(
  id: Int,
  periods: Set[Int],
  workers: Int,
  vehicles: Int = 0,
  zones: Set[Int] = Set()
)