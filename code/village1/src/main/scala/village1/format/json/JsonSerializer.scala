package village1.format.json

import play.api.libs.json.{JsNumber, JsObject, Json}
import village1.data.Skill
import village1.modeling.{Problem, Solution}

object JsonSerializer {

  def serialize (solution: Solution): String => Unit = {

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

  def serialize (problem: Problem): String => Unit = {

    val json = Json.obj(
      "T" -> problem.T,
      "machines" -> problem.machines.size,
      "locations" -> problem.locations,
      "workers" -> problem.workers.map(w =>
        Json.obj(
          "id" -> w.id,
          "name" -> w.name,
          "availabilities" -> w.availabilities,
          "skills" -> w.skills.foldLeft(List[JsObject]()) {
            (acc, skill) => {
              Json.obj(
                "name" -> skill._1,
                "type" -> skill._2.parameterType.toString
              ) :: acc
            }
          }
        )
      ),
      "workerWorkerIncompatibilities" -> Json.arr(), // TODO
      "workerClientIncompatibilities" -> Json.arr(), // TODO
      "clients" -> problem.clients.map { c =>
        Json.obj(
          "name" -> c.name
        )
      },
      "demands" -> problem.demands.map { d =>
        Json.obj(
          "id" -> d.id,
          "client" -> d.client,
          "periods" -> d.periods.toSeq,
          "requiredWorkers" -> d.requiredWorkers
        )
      }

    )

    path: String => {
      JsonUtils.writeJsonFile(path, json)
    }
  }
}
