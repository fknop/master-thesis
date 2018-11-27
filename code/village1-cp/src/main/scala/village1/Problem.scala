package village1

import play.api.libs.json._

import scala.io.Source

case class Problem(
  T: Int,
  vehicles: Int,
  zones: Int,
  demands: Array[Demand],
  workers: Array[Worker],
  workersIncompatibilities: Array[Array[Int]] = Array()
)


object Problem {

  def parse (path: String): Problem = {

    val file = Source.fromFile(path)
    val content = file.getLines.mkString("\n")
    file.close

    val json = Json.parse(content)


    val T = (json \ "T").as[Int]
    val vehicles = (json \ "vehicles").as[Int]
    val zones = (json \ "zones").as[Int]
    val jsonDemands = (json \ "demands").as[Array[JsValue]]
    val jsonWorkers = (json \ "workers").as[Array[JsValue]]

    val demands = jsonDemands.map(d => {
      Demand(
        id = (d \ "id").as[Int],
        periods = Set((d \ "periods").as[Array[Int]]: _*),
        workers = (d \ "workers").as[Int],
        vehicles = (d \ "vehicles").as[Int]
      )
    })

    val workers = jsonWorkers.map(w => {
      Worker(
        id = (w \ "id").as[Int],
        availabilities = Set((w \ "availabilities").as[Array[Int]]: _*)
      )
    })

    val workersIncompatibilities = (json \ "workersIncompatibilities").as[Array[Array[Int]]]

    Problem(
      T = T,
      vehicles = vehicles,
      zones = zones,
      workers = workers,
      demands = demands,
      workersIncompatibilities = workersIncompatibilities
    )
  }
}