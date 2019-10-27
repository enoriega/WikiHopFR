package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.WHConfig
import org.ml4ai.agents.AgentObserver
import org.ml4ai.mdp.{WikiHopEnvironment, WikiHopState}
import org.ml4ai.utils.TransitionMemory
import org.sarsamora.actions.Action

import scala.collection.mutable.ListBuffer

class TestingAgentObserver extends AgentObserver with LazyLogging{
  var actionLog:ListBuffer[(Double, Action)] = new ListBuffer[(Double, Action)]()
  val stats = new ListBuffer[EpisodeStats]()

  override def startedEpisode(env: WikiHopEnvironment): Unit = ()

  override def beforeTakingAction(action: Action, env: WikiHopEnvironment): Unit = ()

  override def actionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = ()


  override def concreteActionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {
    actionLog += Tuple2(reward, action)
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

    val (rewards, actions) = actionLog.unzip

    stats += EpisodeStats(id, numIterations, papersRead,
      success, Stream.continually(.0), rewards, actions,
      env.entities.size
    )
    actionLog.clear()
  }

  override def registerError(throwable: Throwable): Unit = {
    logger.error(throwable.getMessage)
  }
}
