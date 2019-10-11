package org.ml4ai.agents.baseline

import org.ml4ai.agents.{AgentObserver, BaseAgent}
import org.ml4ai.inference.VerboseRelation
import org.ml4ai.mdp.WikiHopEnvironment
import scala.util.Random

/**
  * Base class for deterministic agents. Subclasses will chose actions with different criteria
  */
abstract class DeterministicAgent(implicit rng:Random) extends BaseAgent{

  /**
    * Tail recursive implementation of run episode
    * @param environment to interact with
    * @return The output paths found by the agent
    */
  override def runEpisode(environment: WikiHopEnvironment, monitor:Option[AgentObserver]): Iterable[Seq[VerboseRelation]] = {
    // Call an inner function to keep this strictly tail-recursive
    def helper(environment: WikiHopEnvironment, monitor: Option[AgentObserver]): Iterable[Seq[VerboseRelation]] = {
      try {
        // Base case in which the episode has finished and the outcome can be observed
        if (environment.finishedEpisode) {
          for (m <- monitor)
            m.endedEpisode(environment)

          environment.outcome
        }
        else {
          // Select an action
          val action = selectAction(environment)
          // Log previously to taking the action
          for (m <- monitor)
            m.beforeTakingAction(action, environment)
          // Execute it
          val reward = environment.execute(action).toFloat
          // Log it to the monitor
          for (m <- monitor) {
            m.actionTaken(action, reward, environment.numDocumentsAdded, environment)
            m.concreteActionTaken(environment.lastConcreteAction.get, reward, environment.numDocumentsAdded, environment)
          }

          // Tail recursion
          helper(environment, monitor)
        }
      } catch {
        // If an error happened here, keep track of it
        case t: Throwable =>
          for (m <- monitor)
            m.registerError(t)
          // Re-throw the element to avoid altering the normal logic flow
          throw t
      }
    }

    // Before starting, call the monitor
    for(m <- monitor) m.startedEpisode(environment)

    // Do the actual job
    helper(environment, monitor)
  }
}
