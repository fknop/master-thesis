package village1.format.json

import com.eclipsesource.schema.{SchemaType, SchemaValidator}
import com.eclipsesource.schema.drafts._

import play.api.libs.json._
import village1.data._
import village1.modeling.Problem
import JsonUtils.parseJsonFile

object JsonParser {

  final val PROBLEM_SCHEMA_PATH = "schema/problem.schema.json"

  private def parseClient(value: JsValue): Client = {
   // val client = (value \ "client").as[JsValue]

    val name = (value \ "name").as[String]
    Client(name)
  }

  private def parseClients (value: JsValue): Array[Client] = {
    val clients = (value \ "clients").as[JsArray]
    clients.value.map(parseClient).toArray[Client]
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
      client = value("client").as[Int],
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

  private def parseLocations (json: JsValue): Array[Location] = {
    (json \ "machines").toOption match {
      case Some(value) => value.as[Array[JsValue]].map(m => Location(m("name").as[String]))
      case None => Array[Location]()
    }
  }

  private def parseMachines (json: JsValue): Array[Machine] = {
    (json \ "machines").toOption match {
      case Some(value) => value.as[Array[JsValue]].map(m => Machine(m("name").as[String]))
      case None => Array[Machine]()
    }
  }

  private def parseWorkerWorkerIncompatibilities(json: JsValue) = (json \ "workerWorkerIncompatibilities").toOption match {
    case Some(value) => value.as[Array[Array[Int]]]
    case None => Array[Array[Int]]()
  }

  private def parseWorkerClientIncompatibilities(json: JsValue) = (json \ "workerClientIncompatibilities").toOption match {
    case Some(value) => value.as[Array[Array[Int]]]
    case None => Array[Array[Int]]()
  }

  def validate (jsonSchema: JsValue, json: JsValue): JsResult[JsValue] = {
    import Version7._
    val schema = Json.fromJson[SchemaType](jsonSchema).get
    val validator = SchemaValidator(Some(Version7))
    validator.validate(schema, json)
  }

  def parse (path: String): Problem = {

    val json = parseJsonFile(path)
    val schema = parseJsonFile(PROBLEM_SCHEMA_PATH)
    val result = validate(schema, json)

    if (result.isError) {
      throw new JsonSchemaValidationError(result)
    }

    Problem(
      T = parseT(json),
      locations = parseLocations(json),
      machines = parseMachines(json),
      workers = parseWorkers(json),
      demands = parseDemands(json),
      clients = parseClients(json),
      workerWorkerIncompatibilities = parseWorkerWorkerIncompatibilities(json),
      workerClientIncompatibilities = parseWorkerClientIncompatibilities(json)
    )
  }
}