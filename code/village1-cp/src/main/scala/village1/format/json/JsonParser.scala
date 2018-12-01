package village1.format.json

import play.api.libs.json.{JsString, JsValue, Json}
import village1.data.{Demand, Worker, WorkerRequirement}
import village1.modeling.Problem

import scala.io.Source

object JsonParser {

  private def parseDemand(value: JsValue): Demand = {
    Demand(
      id = value("id").as[Int],
      periods = Set(value("periods").as[Array[Int]]: _*),
      workersRequirements = (0 until value("workers").as[Int]).map(_ => WorkerRequirement()),
      vehicles = value("vehicles").as[Int]
    )
  }

  private def parseDemands(json: JsValue): Array[Demand] = {
    val demands = json("demands").as[Array[JsValue]]
    demands.map(parseDemand)
  }


  private def parseWorker(value: JsValue): Worker = {
    Worker(
      id = value("id").as[Int],
      name = (value \ "name").getOrElse(JsString("Anonymous")).as[String],
      availabilities = Set(value("availabilities").as[Array[Int]]: _*)
    )
  }

  private def parseWorkers(json: JsValue): Array[Worker] = {
    val workers = json("workers").as[Array[JsValue]]
    workers.map(parseWorker)
  }

  private def parseT(json: JsValue)= json("T").as[Int]

  private def parseVehicleNumber(json: JsValue) = json("vehicles").as[Int]

  private def parseZoneNumber(json: JsValue) = json("zones").as[Int]

  private def parseWorkerIncompatibilities(json: JsValue) = json("workersIncompatibilities").as[Array[Array[Int]]]

  def parse (path: String): Problem = {

    val file = Source.fromFile(path)
    val content = file.getLines.mkString("\n")
    file.close()

    val json = Json.parse(content)

    Problem(
      T = parseT(json),
      vehicles = parseVehicleNumber(json),
      zones = parseZoneNumber(json),
      workers = parseWorkers(json),
      demands = parseDemands(json),
      workersIncompatibilities = parseWorkerIncompatibilities(json)
    )
  }
}