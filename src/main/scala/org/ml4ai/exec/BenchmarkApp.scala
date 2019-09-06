package org.ml4ai.exec


import com.typesafe.scalalogging.LazyLogging
import org.clulab.utils.Serializer
import org.ml4ai.{WHConfig, WikiHopInstance}
import org.ml4ai.agents.{BaseAgent, StatsObserver}
import org.ml4ai.agents.baseline.{CascadeAgent, RandomActionAgent}
import org.ml4ai.utils.{BenchmarkStats, StatsDatum, WikiHopParser, rng}

import concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import org.ml4ai.utils.prettyPrintMap

/**
  * Runs an agent over all the training instances of WikiHop and reports results
  */
object BenchmarkApp extends App with LazyLogging{

  val jsonOutputPath = buildOutputFileName(WHConfig.Files.benchmarkOutput)

  // Take the number of instances
  val allInstances = WHConfig.Benchmark.numInstances match {
    case Some(bound) => WikiHopParser.trainingInstances.take(bound)
    case None => WikiHopParser.trainingInstances
  }

  val instances = instancesSlice(allInstances)
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
        case Success(paths) =>
          paths
        case Failure(exception) =>
          logger.error(s"Error in ${instance.id} - ${exception.toString}: ${exception.getMessage}")
          Iterable.empty
        }

      val ret = StatsDatum(instance.id, outcome, observer)
      logger.info(s"Finished ${instance.id}")
      ret
    }

  val outcomes = Future.sequence(runs)

  // Crunch the numbers with the results. This is a side-effect deferred function
  val stats =
    outcomes andThen {
      case Success(os) =>
        val stats = new BenchmarkStats(os)

        logger.info(s"Success rate of ${stats.successRate}. Found a path on ${stats.numSuccesses} out of $totalInstances instances")
        logger.info(s"Iteration distribution: ${prettyPrintMap(stats.iterationNumDistribution)}")
        logger.info(s"Papers distribution: ${prettyPrintMap(stats.papersDistribution)}")
        logger.info(s"Action distribution: ${prettyPrintMap(stats.actionDistribution)}")
        logger.info(s"Concrete action distribution: ${prettyPrintMap(stats.concreteActionDist)}")

        Serializer.save(stats, s"$jsonOutputPath.ser")
        stats.toJson(jsonOutputPath)

      case Failure(exception) =>
        logger.error(exception.toString)
    }

  Await.ready(stats, Duration.Inf)

  def makeAgentFromConfig:BaseAgent = {
    WHConfig.Benchmark.agentType.toLowerCase match {
      case "random" => new RandomActionAgent
      case "cascade" => new CascadeAgent
    }
  }

  def instancesSlice(allInstances: Seq[WikiHopInstance]) = {
    import WHConfig.Benchmark._

    (totalWorkers, workerIndex) match {
      case (Some(tw), Some(wi)) if tw > 0 && wi >= 0 && wi < tw  =>
        // Compute the size of the slices
        val sliceSize = allInstances.size / tw
        val lastWorkerIx = tw - 1
        if(wi < lastWorkerIx){
          val start = sliceSize*wi
          val end = start + sliceSize
          allInstances.slice(start, end)
        }
        else {
          val numToDrop = sliceSize*(tw - 1)
          allInstances.drop(numToDrop)
        }
      case (Some(tw), Some(wi)) =>
        throw new UnsupportedOperationException(s"The worker configuration is not correct. Total workers: $tw\tWorker index: $wi")
      case (None, None) =>
        allInstances
      case _ =>
        throw new UnsupportedOperationException(s"The worker configuration is not correct.")
    }
  }

  def buildOutputFileName(benchmarkOutput: String) = {
    WHConfig.Benchmark.workerIndex match {
      case Some(ix) => s"$benchmarkOutput.$ix"
      case None => benchmarkOutput
    }
  }
}
