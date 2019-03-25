package village1.json

import play.api.libs.json._
import village1.modeling.violations.{AdditionalSkillViolation, WorkerViolation, WorkingRequirementViolation}
import village1.modeling.{Problem, Solution}

object JsonSerializer {

  def serialize (solution: Solution): String => Unit = {

    val plannings = solution.plannings
    val violations = Json.toJson(solution.violations.map { v =>
      Json.obj(
        "type" -> v.toString,
        "values" -> (v match {
          case WorkerViolation(d, p, t) => Json.obj("demand" -> d, "position" -> p, "time" -> t)
          case AdditionalSkillViolation(d, t, s) => Json.obj("demand" -> d, "time" -> t, "skill" -> s.name)
          case WorkingRequirementViolation(w, _, _, v) => Json.obj("worker" -> w, "occurrences" -> v)
        }),
        "description" -> v.description

      )
    })

    val planning = Json.toJson(plannings.map { p =>
      var demand = Json.obj(
        "demand" -> p.demand,
        "workerAssignments" -> p.workerAssignments.sortBy(_.timeslot).map(w => {

          Json.obj(
            "t" -> w.timeslot,
            "workers" -> w.workers
          )
        })
      )

      p.machineAssignments match {
        case Some(machines) =>
          demand += ("machineAssignments", Json.toJson(machines))
        case None =>
      }

      p.locationAssignment match {
        case Some(location) =>
          demand += ("locationAssignment", JsNumber(location))
        case None =>
      }

      demand
    })

    val json = Json.obj(
      "plannings" -> planning,
      "violations" -> violations
    )

    path: String => {
      JsonUtils.writeJsonFile(path, json)
    }
  }

  def serialize (problem: Problem): String => Unit = {

    val json = Json.obj(
      "T" -> problem.T,
      "machines" -> problem.machines.map(m =>
        Json.obj(
          "name" -> m.name
        )
      ),
      "locations" -> problem.locations.map(l =>
        Json.obj(
          "name" -> l.name
        )
      ),
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
          "requiredWorkers" -> d.requiredWorkers,
          "requiredSkills" -> d.requiredSkills.map {
            skills => skills.map(
              skill => Json.obj(
                "name" -> skill.name,
                "type" -> skill.parameterType.toString
              )
            )
          },
          "possibleLocations" -> d.possibleLocations,
          "machineNeeds" -> d.machineNeeds.map(m =>
            Json.obj(
              "name" -> m.name
            )
          )
        )
      }

    )

    path: String => {
      JsonUtils.writeJsonFile(path, json)
    }
  }
}
