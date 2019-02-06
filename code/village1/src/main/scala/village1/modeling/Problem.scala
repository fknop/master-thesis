package village1.modeling

import village1.data._

case class Problem(
  T: Int,
  demands: Array[Demand],
  workers: Array[Worker],
  clients: Array[Client],
  locations: Array[Location] = Array(),
  machines: Array[Machine] = Array(),
  workerWorkerIncompatibilities: Array[Array[Int]] = Array(),
  workerClientIncompatibilities: Array[Array[Int]] = Array()
)
