package org.ml4ai.exec

import com.typesafe.scalalogging.LazyLogging
import org.clulab.utils.Serializer
import org.ml4ai.agents.baseline.{CascadeAgent, RandomActionAgent}
import org.ml4ai.agents.{BaseAgent, StatsObserver}
import org.ml4ai.utils.{BenchmarkStats, StatsDatum, WikiHopParser, prettyPrintMap, rng, using}
import org.ml4ai.{WHConfig, WikiHopInstance}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * This App is meant to find the instances I need according to a criteria (which I define on the fly as I need too)
  */
object InstanceFinderApp extends App with LazyLogging{

  val jsonOutputPath = "selected_instances.json"

  // Take the number of instances
  val allInstances = WikiHopParser.trainingInstances

  val names =
    using(Source.fromFile("small_instances.txt")){
      s =>
        s.getLines().toSet
    } ++
    using(Source.fromFile("testing_instances.txt")){
      s =>
        s.getLines().toSet
    }

  val instances = allInstances filter {
    i =>
      // Insert criteria here
      //val size = i.supportDocs.size
      !(names contains i.id) //&& size >= 10 && size <= 20
  }

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
          observer.iterations match {
            case Some(i) if i > 1 =>
              logger.info(s"Found instance ${instance.id}")
              Some(instance.id)
            case _ => None
          }

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
