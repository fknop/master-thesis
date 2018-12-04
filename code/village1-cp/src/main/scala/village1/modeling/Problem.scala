package village1.modeling

import village1.data.{Demand, Worker}

case class Problem(
  T: Int,
//  vehicles: Int,
  locations: Int,
  demands: Array[Demand],
  workers: Array[Worker],
  workerWorkerIncompatibilities: Array[Array[Int]] = Array(),
  workerClientIncompatibilities: Array[Array[Int]] = Array()
)
