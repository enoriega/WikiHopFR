package org.ml4ai.agents

import org.ml4ai.{WHConfig, WikiHopInstance}
import org.ml4ai.inference.VerboseRelation
import org.ml4ai.ir.LuceneHelper
import org.ml4ai.mdp.WikiHopEnvironment
import org.sarsamora.actions.Action
import org.ml4ai.utils.{SupportDocs, WikiHopParser, buildRandom, md5Hash}

import scala.util.Random

/**
  * Base class to all of the agent implementations
  */
abstract class BaseAgent(implicit rnd:Random) {

//  protected lazy val rng:Random = buildRandom()

  /**
    * Selects an action from those available from the environment within runEpoch
    * @return Action selected from the environment associate to this agent
    */
  protected def selectAction(environment: WikiHopEnvironment):Action

  /**
    * Runs the MDP over an environment. Must be overridden by the implementor
    * @return The output paths found by the agent
    */
  def runEpisode(environment: WikiHopEnvironment, monitor:Option[AgentObserver]):Iterable[Seq[VerboseRelation]]


  /**
    * Convenience overload that generates an environment from a training instance
    * @param instance WikiHop instance to create an environment from
    * @return The output paths found by the agent
    */
  def runEpisode(instance:WikiHopInstance, monitor:Option[AgentObserver] = None):Iterable[Seq[VerboseRelation]] = {
    // Generate the end points
    val source: String = instance.query.split(" ").last
    val destination: String = instance.answer match {
      case Some(ans) => ans
      case None =>
        throw new UnsupportedOperationException("For now, only training instances are supported")
    }

    val localDocs = instance.supportDocs.map (md5Hash).toSet

    val documentUniverse = WHConfig.Environment.documentUniverse match {
      case "Local" =>
        Some(SupportDocs.localDocs(instance))
      case "Random" =>
//        val randomDocs = rng.shuffle(WikiHopParser.trainingInstances).take(100).flatMap(_.supportDocs.map (md5Hash) ).toSet.take (200)
//        Some(localDocs union randomDocs)
        Some(SupportDocs.randomDocs(instance))

      case "Related" =>
//        val relatedDocs = LuceneHelper.getLexicallySimilarDocuments(source.split(" ").toSet, destination.split(" ").toSet)
//        Some(localDocs union relatedDocs.take(200).toSet)
        Some(SupportDocs.relatedDocs(instance))
      case "AllDocs" => None
      case unsupported =>
        throw new UnsupportedOperationException(s"Document universe of $unsupported kind is not supported")

    }

    // Build the environment with the source and destination
    // This is public as the MDP handler needs it
    val environment: WikiHopEnvironment = new WikiHopEnvironment(instance.id, source, destination, documentUniverse)

    runEpisode(environment, monitor)
  }

}
