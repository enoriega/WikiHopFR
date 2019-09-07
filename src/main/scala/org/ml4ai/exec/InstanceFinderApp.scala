package org.ml4ai.exec

import com.typesafe.scalalogging.LazyLogging
import org.clulab.utils.Serializer
import org.ml4ai.agents.baseline.{CascadeAgent, RandomActionAgent}
import org.ml4ai.agents.{BaseAgent, StatsObserver}
import org.ml4ai.utils.prettyPrintMap
import org.ml4ai.utils.{BenchmarkStats, StatsDatum, WikiHopParser, rng}
import org.ml4ai.{WHConfig, WikiHopInstance}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * This App is meant to find the instances I need according to a criteria (which I define on the fly as I need too)
  */
object InstanceFinderApp extends App with LazyLogging{

  val jsonOutputPath = "selected_instances.json"

  // Take the number of instances
  val allInstances = WikiHopParser.trainingInstances

  val instances = allInstances filter {
    i =>
      // Insert criteria here
      val size = i.supportDocs.size
      size >= 10 && size <= 20
  } take (1000)

  val totalInstances = instances.size
  logger.info(s"About to run FocusedReading on $totalInstances instances")

  val agent = makeAgentFromConfig
  logger.info(s"Agent: $agent")

  import ExecutionContext.Implicits.global

  //implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

  // Async programming =)
  val runs =
    for(instance <- instances) yield Future{
      logger.info(s"Starting ${instance.id}")
      val observer = new StatsObserver
      // Return the instance id along with the outcomes
      val outcome = Try(agent.runEpisode(instance, monitor=Some(observer))) match {
        case Success(paths) if paths.nonEmpty =>
          Some(instance.id)
        case Failure(exception) =>
          logger.error(s"Error in ${instance.id} - ${exception.toString}: ${exception.getMessage}")
          None
        case _ =>
          None
        }

      logger.info(s"Finished ${instance.id}")
      outcome
    }

  val outcomes = Future.sequence(runs)

  // Crunch the numbers with the results. This is a side-effect deferred function
  val results =
    outcomes andThen {
      case Success(os) =>
          val instanceIds = os collect { case Some(id) => id}
          println("Matching instances:")
          instanceIds foreach println
//        val stats = new BenchmarkStats(os)
//
//        logger.info(s"Success rate of ${stats.successRate}. Found a path on ${stats.numSuccesses} out of $totalInstances instances")
//        logger.info(s"Iteration distribution: ${prettyPrintMap(stats.iterationNumDistribution)}")
//        logger.info(s"Papers distribution: ${prettyPrintMap(stats.papersDistribution)}")
//        logger.info(s"Action distribution: ${prettyPrintMap(stats.actionDistribution)}")
//        logger.info(s"Concrete action distribution: ${prettyPrintMap(stats.concreteActionDist)}")
//
//        Serializer.save(stats, s"$jsonOutputPath.ser")
//        stats.toJson(jsonOutputPath)

      case Failure(exception) =>
        logger.error(exception.toString)
    }

  Await.ready(results, Duration.Inf)

  def makeAgentFromConfig:BaseAgent = {
    WHConfig.Benchmark.agentType.toLowerCase match {
      case "random" => new RandomActionAgent
      case "cascade" => new CascadeAgent
    }
  }
}
