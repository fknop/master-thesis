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
) {

  for (i <- demands.indices) {
    demands(i).periods.find(_ >= T) match {
      case Some(_) =>
        System.err.println(s"Warning: demand $i has periods above parameter T ($T), these periods will be ignored.")
        demands(i) = demands(i).copy(periods = demands(i).periods.filter(_ < T))
      case None =>
    }
  }

  for (i <- workers.indices) {
    workers(i).availabilities.find(_ >= T) match {
      case Some(_) =>
        System.err.println(s"Warning: worker $i has availabilities above parameter T ($T), these availabilities will be ignored.")
        workers(i) = workers(i).copy(availabilities = workers(i).availabilities.filter(_ < T))
      case None =>
    }
  }
}
