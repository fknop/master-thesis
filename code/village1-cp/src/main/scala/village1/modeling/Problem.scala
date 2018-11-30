package village1.modeling

import village1.data.{Demand, Worker}

case class Problem(
  T: Int,
  vehicles: Int,
  zones: Int,
  demands: Array[Demand],
  workers: Array[Worker],
  workersIncompatibilities: Array[Array[Int]] = Array()
)
