package org.ml4ai.agents

import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.json4s.JsonAST.{JArray, JDouble, JField, JInt, JObject, JString, JValue}
import org.ml4ai.learning.Approximator
import org.ml4ai.mdp.{RandomAction, WikiHopState}
import org.sarsamora.actions.Action
import org.ml4ai.mdp.actionBuilder
import org.ml4ai.utils.{HttpUtils, buildRandom}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

import scala.util.Random

class EpGreedyPolicy(decay:Iterator[Double], network:Approximator)(implicit rng:Random) extends Policy {

  private implicit val httpClient: CloseableHttpClient = HttpClients.createDefault

  var currentEpsilon:Option[Double] = None

  override def selectAction(state: WikiHopState): Action = {
    val epsilon = decay.next()
    currentEpsilon = Some(epsilon)

    if(rng.nextFloat() <= epsilon)
      RandomAction
    else{

      val payload =
        compact{
          render{
            Seq(("features" -> state.toFeatures) ~
              ("candidates" -> state.candidateEntities.get) ~
              ("iterationsOfIntroduction" -> state.iterationsOfIntroduction) ~
              ("ranks" -> state.ranks) ~
              ("entityUsage" -> state.ranks)
//              ("pairwiseComponents" -> state.pairwiseComponents)
            )
          }
        }

      val response = HttpUtils.httpPut("select_action", payload)

      val x =
        for{
          JArray(elements) <- parse(response)
          JObject(element) <- elements
          JField("index", JInt(ix)) <- element
          JField("A", JArray(entityA)) <- element
          JField("B", JArray(entityB)) <- element
        } yield (ix, entityA.map{ case JString(tok) => tok}.toSet, entityB.map{ case JString(tok) => tok}.toSet)

      val (actionIx, entityA, entityB) = x.head
      // Build the action instance to return
      actionBuilder(actionIx.toInt, entityA, entityB)
    }


//    RandomAction
  }
}
