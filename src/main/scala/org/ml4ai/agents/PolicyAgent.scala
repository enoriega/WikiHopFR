package org.ml4ai.agents
import org.ml4ai.agents.baseline.DeterministicAgent
import org.ml4ai.mdp.{RandomAction, WikiHopEnvironment, WikiHopState}
import org.sarsamora.actions.Action

import scala.util.Random

class PolicyAgent(policy:EpGreedyPolicy)(implicit rng:Random) extends DeterministicAgent {
  /**
    * Selects an action from those available from the environment within runEpoch
    *
    * @return Action selected from the environment associate to this agent
    */
  override protected def selectAction(environment: WikiHopEnvironment): Action = {
    // This may seem redundant, however, an agent may not use a policy to select an action. The redundancy is only
    // for the specific case of the policy agent.
    val state = environment.observeState
    state.asInstanceOf[WikiHopState].candidateEntities match {
      case Some(candidates) if candidates.size > 1 =>
        policy.selectAction(state)
      case _ =>
        RandomAction
    }

  }



}
