package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.WHConfig
import org.ml4ai.agents.AgentObserver
import org.ml4ai.mdp.{WikiHopEnvironment, WikiHopState}
import org.ml4ai.utils.TransitionMemory
import org.sarsamora.actions.Action

import scala.collection.mutable.ListBuffer

class RuntimeAgentObserver(epsilons:Iterator[Double]) extends AgentObserver with LazyLogging{
  var state:Option[WikiHopState] = None
  var actionLog:ListBuffer[(Double, Double, Action)] = new ListBuffer[(Double, Double, Action)]()
  val memory = new TransitionMemory[Transition](maxSize = WHConfig.Training.transitionMemorySize)
  val stats = new ListBuffer[EpisodeStats]()

  override def startedEpisode(env: WikiHopEnvironment): Unit = ()

  override def beforeTakingAction(action: Action, env: WikiHopEnvironment): Unit = {
    // Save the state observation before taking the action
    state = Some(env.observeState)
  }

  override def actionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = ()


  override def concreteActionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {
    assert(state.isDefined, "The state should be defined at this point")
    val newState = env.observeState
    val epsilon = epsilons.next()
    val transition = Transition(state.get, action, reward, newState)
    memory remember transition
    state = None
    actionLog += Tuple3(epsilon, reward, action)
  }

  override def endedEpisode(env: WikiHopEnvironment): Unit = {
    val id = env.id
    val numIterations = env.iterations
    val papersRead = env.consultedPapers.size

    val success =
      if(env.outcome.nonEmpty)
        true
      else
        false

    val (epsilons, rewards, actions) = actionLog.unzip3

    stats += EpisodeStats(id, numIterations, papersRead, success, epsilons, rewards, actions)
    actionLog.clear()
  }

  override def registerError(throwable: Throwable): Unit = {
    logger.error(throwable.getMessage)
  }
}
