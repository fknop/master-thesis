package village1.format.json

import play.api.libs.json.{JsArray, JsString, JsValue, Json}
import village1.data._
import village1.modeling.Problem

import scala.io.Source

object JsonParser {

  private def parseClient(value: JsValue): Client = {
    val client = (value \ "client").as[JsValue]

    val name = (client \ "name").as[String]
    Client(name)
  }

  private def parseSkill(value: JsValue): Skill = {

    val parameterType = (value \ "type").toOption match {
      case Some(t) => ParameterType.from(t.as[String])
      case None => ParameterType.None
    }

    val name = value("name").as[String]

    if (parameterType == ParameterType.None) {
      Skill(
        name,
        parameterType
      )
    }
    else {
      Skill(
        name,
        parameterType,
        value = value("value").as[Double]
      )
    }
  }

  private def parseSkills(array: JsArray): IndexedSeq[Skill] = {
    array.value.map(parseSkill)
  }

  private def parseDemand(value: JsValue): Demand = {

    val skillsJson = (value \ "requiredSkills").toOption

    val skills = skillsJson match {
      case Some(jsValue) => jsValue.as[IndexedSeq[JsArray]].map(parseSkills)
      case None => IndexedSeq[IndexedSeq[Skill]]()
    }

    Demand(
      id = value("id").as[Int],
      client = parseClient(value),
      periods = Set(value("periods").as[Array[Int]]: _*),
      requiredWorkers = value("requiredWorkers").as[Int],
      requiredSkills = skills
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

  private def parseLocationsNumber(json: JsValue) = json("locations").as[Int]

  private def parseWorkerWorkerIncompatibilities(json: JsValue) = (json \ "workerWorkerIncompatibilities").toOption match {
    case Some(value) => value.as[Array[Array[Int]]]
    case None => Array[Array[Int]]()
  }

  private def parseWorkerClientIncompatibilities(json: JsValue) = (json \ "workerClientIncompatibilities").toOption match {
    case Some(value) => value.as[Array[Array[Int]]]
    case None => Array[Array[Int]]()
  }

  def parse (path: String): Problem = {

    val file = Source.fromFile(path)
    val content = file.getLines.mkString("\n")
    file.close()

    val json = Json.parse(content)

    Problem(
      T = parseT(json),
      locations = parseLocationsNumber(json),
      workers = parseWorkers(json),
      demands = parseDemands(json),
      workerWorkerIncompatibilities = parseWorkerWorkerIncompatibilities(json),
      workerClientIncompatibilities = parseWorkerClientIncompatibilities(json)
    )
  }
}