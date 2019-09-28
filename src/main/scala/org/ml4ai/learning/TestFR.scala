package org.ml4ai.learning

import java.io.File
import java.nio.charset.Charset
import java.util.concurrent.{ExecutorService, Executors}

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.ml4ai.agents.{AgentObserver, GreedyPolicy, PolicyAgent}
import org.ml4ai.utils.{FutureUtils, WikiHopParser, lemmatize, prettyPrintMap, rng, using}
import org.ml4ai.{WHConfig, WikiHopInstance}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.language.postfixOps
import scala.util.Success

object TestFR extends App with LazyLogging{

  private implicit val httpClient: CloseableHttpClient = HttpClients.createDefault

  def selectSmall(instances: Seq[WikiHopInstance]):Iterable[WikiHopInstance] = {
    val names =
      using(Source.fromFile("testing_instances.txt")){
        s =>
          s.getLines().toSet
      }
    instances filter (i => names contains i.id)
  }
  
  
  /**
    * Aux function to compute some of the stats that get printed to the log
    * @param stats Sequence of EpisodeStats instances
    * @return
    */
  def computeStats(stats: Seq[EpisodeStats]):(Map[Int, Int], Map[Int, Int]) = {
    val (iterations, papersRead) =
      (stats map {
        s =>
          (s.iterations, s.papersRead)
      }).unzip

    val iterationsDist =
      iterations.groupBy(identity).mapValues(_.size)
    val papersDist =
      papersRead.groupBy(identity).mapValues(_.size)

    (iterationsDist, papersDist)
  }


  // Load the data
  val instances = WikiHopParser.trainingInstances

  val network = new DQN()
  
  val smallInstances = selectSmall(instances)

  // Load file names
  val checkpointName = WHConfig.Testing.modelName
  val statsDump = new File(WHConfig.Testing.statsDump)
  // Trim the file to start from scratch
  FileUtils.write(statsDump, "", Charset.defaultCharset)

  // Preload processors
  lemmatize("Preload processors, please")

  // Operate asynchronously with $maxThreads futures simultaneously
  private val threadPool: ExecutorService = Executors.newFixedThreadPool(WHConfig.Testing.maxThreads)
  private implicit val ec: ExecutionContext = new ExecutionContext {


    def execute(runnable: Runnable) {
      threadPool.submit(runnable)
    }

    def reportFailure(t: Throwable): Unit = {
      logger.error(s"${t.getClass}\t${t.getMessage}")
    }

    def shutdown():Unit = {
      threadPool.shutdown()
    }
  }


  // Let the cores do their work on the first slice of instances
  val slices = smallInstances.grouped(WHConfig.Testing.maxThreads)

  val futures =
    (for(slice <- slices) yield {
      val sliceFutures =
        slice map {
          instance =>
            logger.info(s"Starting with ${instance.id}")
            // Dispatch the agent asynchronously
            val f =
              Future {
                // Set up the body of the future
                val policy = new GreedyPolicy(network)
                val agent = new PolicyAgent(policy)
                val observer: AgentObserver = new RuntimeAgentObserver(Stream.continually(0.0).iterator)
                val outcome = agent.runEpisode(instance, Some(observer))
                (outcome, observer)
              }
            // Attach a non-blocking timeout to the future
            val timedOutFuture = FutureUtils.futureWithTimeout(f, 1.minute)

            timedOutFuture onComplete {
              _ =>
                logger.info(s"Finished with ${instance.id}")
            }
            timedOutFuture
        } toSeq // This call toSeq is necessary to not consume the iterable after awaiting for the results

      // Block on the futures to collect the results and do back propagation
      Await.ready(Future.sequence(sliceFutures), Duration.Inf)
      sliceFutures
    }).flatten.toSeq


  Await.ready(Future.sequence(futures), Duration.Inf)

  // Collect the results
  val (outcomes, observers) =
    (futures map (_.value) collect {
      case Some(Success((outcome, observer))) => (outcome, observer)
    }).unzip

  // Aggregate the results
  val successes = outcomes count (_.nonEmpty)
  // Aggregate observers' data
  val (partialMemories, partialStats) = observers map {
    case o:RuntimeAgentObserver => (o.memory, o.stats)
  } unzip
  // Aggregate the stats
  val stats = partialStats.flatten

  // Do a back propagation step and send info to the log
  val successRate = successes.toFloat / instances.size
  logger.info(s"Success rate of $successRate.")
  val (iterationDist, documentDist) = computeStats(stats)
  logger.info(s"Iterations:\n${prettyPrintMap(iterationDist)}")
  logger.info(s"Papers read:\n${prettyPrintMap(documentDist)}")
  FileUtils.writeLines(statsDump, stats.map(_.toString).asJava, true)

  threadPool.shutdown()

}
