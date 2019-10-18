package org.ml4ai.learning

import java.io.{File, PrintWriter}
import java.nio.charset.Charset
import java.util.concurrent.{ExecutorService, Executors}

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.ml4ai.agents.{AgentObserver, EpGreedyPolicy, PolicyAgent}
import org.ml4ai.mdp._
import org.ml4ai.utils.{FutureUtils, HttpUtils, TransitionMemory, WikiHopParser, lemmatize, prettyPrintMap, rng, using}
import org.ml4ai.{WHConfig, WikiHopInstance}
import org.sarsamora.Decays
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import language.postfixOps
import scala.io.Source
import scala.util.{Failure, Random, Success}
import collection.JavaConverters._
import concurrent.{Await, ExecutionContext, Future}
import concurrent.duration._

object TrainFR extends App with LazyLogging{

  private implicit val httpClient: CloseableHttpClient = HttpClients.createDefault

  def selectSmall(instances: Seq[WikiHopInstance]):Iterable[WikiHopInstance] = {
    val names =
      using(Source.fromFile(WHConfig.Training.instances)){
        s =>
          s.getLines().toSet
      }
    instances filter (i => names contains i.id)
  }

  /**
    * Tests whether the parameters converged since the last update
    * @return
    */
  def converged = false // TODO: Implement this correctly or remove

  /**
    * Updates the network with a minibatch
    */
  def updateParameters(network:Approximator)(implicit rng:Random):Unit = {
    // Sample a mini batch
    val miniBatch = memory.sample(1000)

    val payload =
      compact {
        render {
          miniBatch map {
            case Transition(state, action, reward, nextState) =>

              val features = state.toFeatures

              val (entityA, entityB) = action match {
                case Exploration(single) => (single, single)
                case ExplorationDouble(entityA, entityB) => (entityA, entityB)
                case Exploitation(entityA, entityB) => (entityA, entityB)
                case _ => throw new NotImplementedException
              }

              val indexA = state.candidateEntities.get.indexOf(entityA)
              val indexB = state.candidateEntities.get.indexOf(entityB)

              // TODO verify why is an entity not present in the list
              val extendedFeatures =
                features ++ Map("log_count_a" -> (if(indexA >= 0) state.entityUsage(indexA) else 0.0),
                    "log_count_b" -> (if(indexB >= 0) state.entityUsage(indexB) else 0.0),
                    "intro_a" -> (if(indexA >= 0) state.iterationsOfIntroduction(indexA) else 0.0),
                    "intro_b" -> (if(indexB >= 0) state.iterationsOfIntroduction(indexB) else 0.0),
                    "rank_a" -> (if(indexA >= 0) state.ranks(indexA) else 0.0),
                    "rank_b" -> (if(indexB >= 0) state.ranks(indexB) else 0.0))

              ("state" ->
                ("features" -> extendedFeatures) ~

                  ("A" -> entityA) ~ ("B" -> entityB)) ~
                ("action" ->
                  (action match {
                    case _: Exploitation => "exploitation"
                    case _ => "exploration"
                  })) ~
                ("reward" -> reward) ~
                ("new_state" ->
                  ("features" -> nextState.toFeatures) ~
                  ("candidates" -> nextState.candidateEntities.get) ~
                  ("iterationsOfIntroduction" -> nextState.iterationsOfIntroduction) ~
                  ("ranks" -> nextState.ranks) ~
                  ("entityUsage" -> nextState.ranks))
          }
        }
      }

    HttpUtils.httpPut("backwards", payload)

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

  val numEpisodes = WHConfig.Training.episodes
  val targetUpdate = WHConfig.Training.targetUpdate


  val network:Approximator = WHConfig.Training.approximator match {
    case "dqn" =>
      new DQN()
    case "linear" =>
      new LinearQN()
    case a =>
      val msg = s"Undefined approximator: $a"
      logger.error(msg)
      throw new UnsupportedOperationException(msg)
  }

  network.reset()

  // Set up the ε decay

  def linearDecay(upperBound:Double, lowerBound:Double, steps:Int, delay:Int) = {
    val prefix = Stream.fill(delay)(upperBound)
    val slope = (upperBound - lowerBound) / steps
    val decay =
      for(i <- 0 until steps) yield upperBound - (slope*i)
    val suffix = Stream.continually(lowerBound)

    prefix ++ decay ++ suffix
  }

  val decayFactory = WHConfig.Training.Epsilon.kind match {
    case "linear" => linearDecay _
    case "exponential" => Decays.exponentialDecay _
    case invalid =>
      val err = s"Unsupported ε decay: $invalid"
      logger.error(err)
      throw new UnsupportedOperationException(err)
  }

  val epsilonDecay = decayFactory(WHConfig.Training.Epsilon.upperBound, WHConfig.Training.Epsilon.lowerBound, (numEpisodes*WHConfig.Training.Epsilon.length).toInt, 0).iterator
  val epsilonIterator = epsilonDecay

  val memory = new TransitionMemory[Transition](maxSize = WHConfig.Training.transitionMemorySize)

  val smallInstances = selectSmall(instances)
  val streamIterator = Stream.continually(smallInstances.toStream).flatten.iterator

  // Load file names
  val checkpointSuffix = WHConfig.Training.modelName
  val statsDump = new File(WHConfig.Training.statsDump)
  // Trim the file to start from scratch
  FileUtils.write(statsDump, "", Charset.defaultCharset)

  // Preload processors
  lemmatize("Preload processors, please")

  // Operate asynchronously with $targetUpdate futures simultaneously
  //implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global
  private val threadPool: ExecutorService = Executors.newFixedThreadPool(WHConfig.Training.maxThreads)
  implicit val ec: ExecutionContext = new ExecutionContext {

    def execute(runnable: Runnable) {
      threadPool.submit(runnable)
    }

    def reportFailure(t: Throwable): Unit = {
      logger.error(s"${t.getClass}\t${t.getMessage}")
    }
  }


  // Iterate through the requested number of epochs with a stride value of targetUpdate
  for(ep <- 1 to numEpisodes by targetUpdate) {
    logger.debug(s"Epoch $ep")
    // Take a slice of the instances
    val instancesBatch = streamIterator take targetUpdate toList

    // Let the cores do their work on the first slice of instances
//    val slices = instancesBatch.grouped(WHConfig.Training.maxThreads).toList
    val slices = instancesBatch.grouped(instancesBatch.size).toList

    val futures =
      (for(slice <- slices) yield {
        val sliceFutures =
          slice map {
            instance =>
              // Dispatch the agent asynchronously
              val epsilonValue = epsilonIterator.next()
              val f =
                Future {
                  logger.debug(s"Started ${instance.id}")
                  // Create a constant iterator with the current value of ε
                  val epsilonStream = Stream.continually(epsilonValue)
                  // Set up the body of the future
                  val policy = new EpGreedyPolicy(epsilonStream.iterator, network)
                  val agent = new PolicyAgent(policy)
                  val observer: AgentObserver = new TrainingAgentObserver(epsilonStream.iterator)
                  val outcome = agent.runEpisode(instance, Some(observer))
                  logger.debug(s"Finished ${instance.id}")
                  (outcome, observer)
                }
              // Attach a non-blocking timeout to the future
              val future =
                f//FutureUtils.futureWithTimeout(f, Duration.Inf)

              // Attach an onComplete call back to record in case there's an error or time out
              future onComplete {
                case Failure(exception) =>
                  logger.error(s"Training future failure for ${instance.id}: $exception")
                case _ => () // Anything else just ignore it
              }

              // Return the future
              future
          } toSeq // This call toSeq is necessary to not consume the iterable after awaiting for the results

        // Block on the futures to collect the results and do back propagation
        Await.ready(Future.sequence(sliceFutures), Duration.Inf)

        sliceFutures
      }).flatten.toSeq



    // Collect the results
    val (outcomes, observers) =
      (futures map (_.value) collect {
        case Some(Success((outcome, observer))) => (outcome, observer)
      }).unzip

    // Aggregate the results
    val successes = outcomes count (_.nonEmpty)
    // Aggregate observers' data
    val (partialMemories, partialStats) = observers map {
      case o:TrainingAgentObserver => (o.memory, o.stats)
    } unzip
    // Aggregate the stats
    val stats = partialStats.flatten
    // Expand the memory with all the individual memories
    memory remember partialMemories.flatten

    // Do a back propagation step and send info to the log
    val successRate = successes / targetUpdate.toFloat
    logger.info(s"Current episode: $ep out of $numEpisodes")
    logger.info(s"Current ε = ${epsilonDecay.next()}")
    logger.info(s"Success rate of $successRate for the last $targetUpdate episodes")
    val (iterationDist, documentDist) = computeStats(stats)
    logger.info(s"Iterations:\n${prettyPrintMap(iterationDist)}")
    logger.info(s"Papers read:\n${prettyPrintMap(documentDist)}")
    FileUtils.writeLines(statsDump, stats.map(_.toString).asJava, true)
    updateParameters(network)
    val checkpointName = s"${ep}_$checkpointSuffix"
    logger.info(s"Saving checkpoint as $checkpointName")
    network.save(checkpointName)
  }

  threadPool.shutdown()
}
