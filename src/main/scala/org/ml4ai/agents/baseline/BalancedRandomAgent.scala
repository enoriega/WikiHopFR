package org.ml4ai.agents.baseline

import org.ml4ai.mdp.{Exploitation, ExplorationDouble, WikiHopEnvironment}
import org.sarsamora.actions.Action

import scala.util.Random

class BalancedRandomAgent(implicit rng:Random) extends DeterministicAgent {

  /**
    * Selects an action from those available from the environment within runEpoch
    *
    * @return Action selected from the environment associate to this agent
    */
  override protected def selectAction(environment: WikiHopEnvironment): Action = {
    val coin = rng.nextBoolean()

    val actions =
      if(coin) {
        environment.possibleActions filter {
          case _:Exploitation => true
          case _ => false
        }
      }
      else{
        environment.possibleActions filter {
          case _:ExplorationDouble => true
          case _ => false
        }
      }

    rng.shuffle(actions).head
  }
}
