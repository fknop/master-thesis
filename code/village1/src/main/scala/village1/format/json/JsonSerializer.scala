package village1.format.json

import play.api.libs.json.{JsNumber, Json}
import village1.modeling.Solution

object JsonSerializer {

  def serialize (solution: Solution): (String => Unit) = {

    val plannings = solution.plannings.reverse
    val json = Json.toJson(plannings.map { p =>
      val timeslot = p.timeslot

      val demands = p.demandAssignments.reverse.map { assignment =>
        Json.obj(
          "demand" -> assignment.demand.id,
          "workers" -> assignment.workerAssignments.map(_.worker.id)
        )
      }

      Json.obj(
        "timeslot" -> p.timeslot,
        "demands" -> demands
      )
    })

    path: String => {
      JsonUtils.writeJsonFile(path, json)
    }
  }
}
