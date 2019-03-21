package village1.json

import com.eclipsesource.schema.drafts.Version7
import com.eclipsesource.schema.{SchemaType, SchemaValidator}
import play.api.libs.json._
import village1.benchmark._
import village1.data._
import village1.json.JsonUtils.parseJsonFile
import village1.modeling.Problem

object JsonParser {

  final val PROBLEM_SCHEMA_PATH = "schema/problem.schema.json"


  /**
    * Parse client value
    * @param value { "name": "value" }
    * @return the client
    */
  private def parseClient(value: JsValue): Client = {
    val name = (value \ "name").as[String]
    Client(name)
  }

  /**
    * Parse clients
    * @param value { "clients": [client1, client2] }
    * @return list of clients
    */
  private def parseClients (value: JsValue): Array[Client] = {
    val clients = (value \ "clients").as[JsArray]
    clients.value.map(parseClient).toArray[Client]
  }

  private def parseSkill(value: JsValue): Skill = {
    value match {
      case s: JsString => Skill(s.as[String])
      case o: JsObject =>
        val parameterType = (o \ "type").toOption match {
          case Some(t) => ParameterType.from(t.as[String])
          case None => ParameterType.None
        }

        val name = o("name").as[String]

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
            value = o("value").as[Double]
          )
        }
    }
  }

  private def parseDemandSkills(array: JsArray): Array[Skill] = {
    array.value.map(parseSkill).toArray
  }

  private def parseAdditionalSkills(value: JsValue): Array[Skill] = {
    val skills = (value \ "additionalSkills").toOption
    skills match {
      case Some(s) => s.as[Array[JsValue]].map(parseSkill)
      case None => Array()
    }
  }

  private def parsePossibleLocations(value: JsValue): Set[Int] = {

    (value \ "possibleLocations").toOption match {
      case Some(locations) => Set(locations.as[Array[Int]]: _*)
      case None => Set()
    }
  }

  private def parseDemand(value: JsValue, index: Int): Demand = {

    val skillsJson = (value \ "requiredSkills").toOption

    val skills = skillsJson match {
      case Some(jsValue) => jsValue.as[Array[JsArray]].map(parseDemandSkills)
      case None => Array[Array[Skill]]()
    }

    Demand(
      id = index,
      client = value("client").as[Int],
      periods = Set(value("periods").as[Array[Int]]: _*),
      requiredWorkers = value("requiredWorkers").as[Int],
      requiredSkills = skills,
      additionalSkills = parseAdditionalSkills(value),
      machineNeeds = parseMachineNeeds(value),
      possibleLocations = parsePossibleLocations(value)
    )
  }

  private def parseDemands(json: JsValue): Array[Demand] = {
    val demands = json("demands").as[Array[JsValue]]
    demands.zipWithIndex.map { case(v, i) => parseDemand(v, i) }
  }


  private def parseWorkersSkills(values: Array[JsValue]): Map[String, Skill] = {

    var map = Map[String, Skill]()

    for (v <- values) {
      val skill = parseSkill(v)
      map = map.updated(skill.name, skill)
    }

    map
  }

  // value: a value that possibly contains an array "skills"
  private def parseWorkersSkills(value: JsValue): Map[String, Skill] = {
    val json = (value \ "skills").toOption

    json match {
      case Some(s) => parseWorkersSkills(s.as[Array[JsValue]])
      case None => Map[String, Skill]()
    }
  }

  private def parseWorker(value: JsValue, index: Int): Worker = {
    Worker(
      id = index,
      name = (value \ "name").getOrElse(JsString("Anonymous")).as[String],
      availabilities = Set(value("availabilities").as[Array[Int]]: _*),
      skills = parseWorkersSkills(value)
    )
  }

  private def parseWorkers(json: JsValue): Array[Worker] = {
    val workers = json("workers").as[Array[JsValue]]
    workers.zipWithIndex.map { case(v, i) => parseWorker(v, i) }
  }

  private def parseT(json: JsValue)= json("T").as[Int]

  private def parseLocations (json: JsValue): Array[Location] = {
    (json \ "locations").toOption match {
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

  private def parseMachineNeeds (json: JsValue): Array[Machine] = {
    (json \ "machineNeeds").toOption match {
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

  private def parseWorkingRequirement(json: JsValue): WorkingRequirement = {
    val worker = (json \ "worker").as[Int]
    val minWorkingPeriods = (json \ "minWorkingPeriods").toOption match {
      case Some(min) => Some(min.as[Int])
      case None => None
    }

    val maxWorkingPeriods = (json \ "maxWorkingPeriods").toOption match {
      case Some(min) => Some(min.as[Int])
      case None => None
    }

    WorkingRequirement(worker, minWorkingPeriods, maxWorkingPeriods)
  }

  private def parseWorkingRequirements(json: JsValue): Array[WorkingRequirement] = (json \ "workingRequirements").toOption match {
    case Some(value) => value.as[Array[JsValue]].map(parseWorkingRequirement)
    case None => Array()
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
      workerClientIncompatibilities = parseWorkerClientIncompatibilities(json),
      workingRequirements = parseWorkingRequirements(json)
    )
  }


  implicit private val problemSizeReads = Json.reads[ProblemSize]
  implicit private val bennchmarkMeasurementReads = Json.reads[BenchmarkMeasurement]
  implicit private val benchmarkResultReads = Json.reads[BenchmarkResult]
  implicit private val benchmarkSeriReads= Json.reads[BenchmarkSerie]
  implicit private val benchmarkReads = Json.reads[BenchmarkInstance]
  def parseBenchmark (path: String): BenchmarkInstance = {
    val json = JsonUtils.parseJsonFile(path)
    val result = Json.fromJson[BenchmarkInstance](json)
    result match {
      case JsSuccess(b: BenchmarkInstance, _) => b
      case e: JsError => throw JsResultException(e.errors)
    }
  }
}
