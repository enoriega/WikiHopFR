package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.methods.{HttpGet, HttpPost}
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.ml4ai.WHConfig
import org.ml4ai.mdp.{Exploitation, Exploration, ExplorationDouble, WikiHopState}
import org.ml4ai.utils._
import org.sarsamora.actions.Action

import scala.language.implicitConversions

class DQN(initToZero:Boolean = false) extends Approximator(initToZero,"dqn")
class LinearQN(initToZero:Boolean = false) extends Approximator(initToZero,"linear")
class MLP(initToZero:Boolean = false) extends Approximator(initToZero,"mlp")

abstract class Approximator(initToZero:Boolean, functionalForm:String) extends LazyLogging{

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

  def save(name:String):Unit = {
    HttpUtils.saveModel(name)
  }

  def reset(): Unit = {
    val endpoint = WHConfig.HttpClient.server
    val arg1 = if(initToZero) "zero_init=true" else "zero_init=false"
    val arg2 = s"approximator=$functionalForm"
    val args = "?" + Seq(arg1, arg2).mkString("&")
    val request = new HttpGet(s"$endpoint/reset$args")
    val _ = httpClient.execute(request)
  }

  def load(modelName:String): Unit = {
    val endpoint = WHConfig.HttpClient.server
    val request = new HttpPost(s"$endpoint/load?approximator=$functionalForm&name=$modelName")
    val _ = httpClient.execute(request)
  }
}

object Approximator {
  implicit def actionIndex(action:Action):Int = action match {
    case _:Exploration => 0
    case _:ExplorationDouble => 1
    case _:Exploitation => 2
    case _ => throw new IllegalStateException("Unsupported action type for RL")
  }
}