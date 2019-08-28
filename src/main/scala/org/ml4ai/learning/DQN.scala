package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.methods.HttpPut
import org.apache.http.entity.{ContentType, StringEntity}
import org.ml4ai.mdp.{Exploitation, Exploration, ExplorationDouble, WikiHopState}
import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.ml4ai.utils._

import scala.io.Source
import scala.util.{Failure, Success, Try}


class DQN() extends LazyLogging{

  private implicit val httpClient: CloseableHttpClient = HttpClients.createDefault

  def apply(input:(WikiHopState, Set[String], Set[String])): (Float, Float) = this(Seq(input)).head

  def apply(input:Iterable[(WikiHopState, Set[String], Set[String])]):Seq[(Float, Float)] = {

    val payload =
      compact {
        render {
          input map {
            case (features, entityA, entityB) =>
              ("features" -> features.toFeatures) ~
                ("A" -> entityA) ~
                ("B" -> entityB)
          }
        }
      }

    val response = HttpUtils.httpPut("forward", payload)

    for {
      JArray(values) <- parse(response)
      JObject(value) <- values
      ("Exploration", JDouble(exploration)) <- value
      ("Exploitation", JDouble(exploitation)) <- value
    } yield (exploration.toFloat, exploitation.toFloat)

  }

}

object DQN {
  implicit def actionIndex(action:Action):Int = action match {
    case _:Exploration => 0
    case _:ExplorationDouble => 1
    case _:Exploitation => 2
    case _ => throw new IllegalStateException("Unsupported action type for RL")
  }
}