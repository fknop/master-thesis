package village1

case class Demand(
  id: Int,
  availabilities: Set[Int],
  workers: Int,
  vehicles: Int = 0,
  zones: Set[Int] = null
)