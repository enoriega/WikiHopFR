package org.ml4ai.agents

import edu.cmu.dynet.{ComputationGraph, Tensor}
import org.ml4ai.learning.DQN
import org.ml4ai.mdp.{RandomAction, WikiHopState}
import org.sarsamora.actions.Action
import org.ml4ai.mdp.actionBuilder
import org.ml4ai.utils.buildRandom

import scala.util.Random

class EpGreedyPolicy(decay:Iterator[Double], network:DQN)(implicit rng:Random) extends Policy {

  def argMax(values:Tensor):Seq[Int] = {
    val columns = values.toSeq().grouped(values.getD().rows().toInt)
    columns.map{
      col =>
        col.zipWithIndex.maxBy(_._1)._2
    }.toSeq
  }

  def max(values:Tensor):Seq[Float] = {
    val columns = values.toSeq().grouped(values.getD().rows().toInt)
    columns.map{
      col =>
        col.max
    }.toSeq
  }

  override def selectAction(state: WikiHopState): Action = {
    val epsilon = decay.next()

    ComputationGraph.renew()

    val candidateEntities:Seq[Seq[String]] = state.candidateEntities.get

    if(rng.nextFloat() <= epsilon)
      RandomAction
    else{

      // Get the top action values for each of the candidate entities
      val actionValues: Seq[(Int, Float, Seq[String], Seq[String])] =
        for{
          entityA <- candidateEntities
          entityB <- candidateEntities
        } yield {
          val actionVals = network.evaluate((state, entityA, entityB))
          val (actionIx, actionValue) = (argMax(actionVals).head, max(actionVals).head)
          (actionIx, actionValue, entityA, entityB)
        }

      // Top values
      val (actionIx, actionValue, entityA, entityB) =
        actionValues.sortBy{
          case (_, actionValue, _, _) =>
            actionValue
        }.reverse.head

      // Build the action instance to return
      actionBuilder(actionIx, entityA, entityB)
    }
  }
}
