package org.ml4ai.agents.baseline

import org.ml4ai.agents.{AgentObserver, BaseAgent}
import org.ml4ai.inference.VerboseRelation
import org.ml4ai.mdp.{Cascade, WikiHopEnvironment}
import org.sarsamora.actions.Action

import scala.util.Random

class CascadeAgent(implicit rng:Random) extends DeterministicAgent {
  /**
    * Selects an action from those available from the environment within runEpoch
    *
    * @return Action selected from the environment associate to this agent
    */
  override protected def selectAction(environment: WikiHopEnvironment): Action = {
    // TODO remember the criteria to select agents in the cascade agent from FR
    val degrees: Map[Set[String], Int] = environment.entityDegrees
    if(degrees.size >= 2) {
      val selectedEntities = degrees.toSeq.sortBy(_._2).reverse.take(2).map(_._1)
      Cascade(selectedEntities.head.toSeq.sorted, selectedEntities(1).toSeq.sorted)
    }
    else if(degrees.isEmpty){
      Cascade(environment.startTokens, environment.endTokens)
    }
    else
      throw new UnsupportedOperationException("Can't do Cascade action on a KG with less than two entities.")
  }


}
