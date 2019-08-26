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

  // TODO: Reimplement this in pytorch

//  private val embeddingsDimension = embeddingsHelper.embeddingsDim
//  private val featuresDimension = WikiHopState.numFeatures
//  private val featureVectorSize = 2*embeddingsDimension + featuresDimension
//
//  private val pW = params.addParameters(Dim(10, featureVectorSize))
//  private val pb = params.addParameters(Dim(10))
//  private val pX = params.addParameters(Dim(2, 10))
//  private val pc = params.addParameters(Dim(2))


  def apply(input:(WikiHopState, Set[String], Set[String])): Any = this(Seq(input))

  def apply(input:Iterable[(WikiHopState, Set[String], Set[String])]):Any = {

//    val inputVectors = input.toSeq map {
//      case (state, entityA, entityB) =>
//        val features = vectorizeState(state)
//        val featureVector = Expression.input(Dim(features.size), features)
//        val embA = embeddingsHelper.lookup(entityA)
//        val embB = embeddingsHelper.lookup(entityB)
//
//        Expression.concatenate(featureVector, aggregateEmbeddings(embA), aggregateEmbeddings(embB))
//    }
//
//    val inputMatrix = Expression.concatenateCols(inputVectors:_*)
//
//    val W = Expression.parameter(pW)
//    val b = Expression.parameter(pb)
//    val X = Expression.parameter(pX)
//    val c = Expression.parameter(pc)
//
//    X * Expression.tanh(W*inputMatrix + b) + c

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

    response
  }

  def distance(entityA:Set[String], entityB:Set[String]) = {

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