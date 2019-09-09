package org.ml4ai.learning

import java.io.File
import java.nio.charset.Charset

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.ml4ai.agents.{AgentObserver, EpGreedyPolicy, PolicyAgent}
import org.ml4ai.mdp._
import org.ml4ai.utils.{HttpUtils, TransitionMemory, WikiHopParser, rng}
import org.ml4ai.{WHConfig, WikiHopInstance}
import org.sarsamora.Decays
import org.sarsamora.actions.Action
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.ml4ai.utils.using
import org.ml4ai.utils.prettyPrintMap

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random
import collection.JavaConverters._

object TrainFR extends App with LazyLogging{

  private implicit val httpClient: CloseableHttpClient = HttpClients.createDefault

  def selectSmall(instances: Seq[WikiHopInstance]):Iterable[WikiHopInstance] = {
    val names =
      using(Source.fromFile("small_instances.txt")){
        s =>
          s.getLines().toSet
      }
    instances filter (i => names contains (i.id))
  }

  /**
    * Tests whether the parameters converged since the last update
    * @return
    */
  def converged = false // TODO: Implement this correctly or remove

  /**
    * Updates the network with a minibatch
    */
  def updateParameters(network:DQN)(implicit rng:Random):Unit = {
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

              ("state" ->
                ("features" -> features) ~ ("A" -> entityA) ~ ("B" -> entityB)) ~
                ("action" ->
                  (action match {
                    case _: Exploitation => "exploitation"
                    case _ => "exploration"
                  })) ~
                ("reward" -> reward) ~
                ("new_state" ->
                  ("features" -> nextState.toFeatures) ~ ("candidates" -> nextState.candidateEntities.get))
          }
        }
      }

    HttpUtils.httpPut("backwards", payload)

  }


  // Load the data
  val instances = WikiHopParser.trainingInstances

  val numEpisodes = WHConfig.Training.episodes
  val targetUpdate = WHConfig.Training.targetUpdate


  val network = new DQN()
  network.reset()

  val policy = new EpGreedyPolicy(Decays.exponentialDecay(WHConfig.Training.Epsilon.upperBound, WHConfig.Training.Epsilon.lowerBound, numEpisodes*10, 0).iterator, network)
  val memory = new TransitionMemory[Transition](maxSize = WHConfig.Training.transitionMemorySize)
  val stats = new ListBuffer[EpisodeStats]()

  val smallInstances = selectSmall(instances)
  val streamIterator = Stream.continually(smallInstances.toStream).flatten.iterator

  val trainingObserver: AgentObserver = new AgentObserver {

    var state:Option[WikiHopState] = None
    var actionLog:ListBuffer[(Double, Action)] = new ListBuffer[(Double, Action)]()

    override def startedEpisode(env: WikiHopEnvironment): Unit = ()

    override def beforeTakingAction(action: Action, env: WikiHopEnvironment): Unit = {
      // Save the state observation before taking the action
      state = Some(env.observeState)
    }

    override def actionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = ()


    override def concreteActionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {
      assert(state.isDefined, "The state should be defined at this point")
      val newState = env.observeState
      val epsilon = policy.currentEpsilon.get
      val transition = Transition(state.get, action, reward, newState)
      memory remember transition
      state = None
      actionLog += Tuple2(epsilon, action)
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

      val (epsilons, actions) = actionLog.unzip

      stats += EpisodeStats(id, numIterations, papersRead, success, epsilons, actions)
      actionLog.clear()
    }

    override def registerError(throwable: Throwable): Unit = {
      logger.error(throwable.getMessage)
    }
  }

  // Load file names
  val checkpointName = WHConfig.Training.modelName
  val statsDump = new File(WHConfig.Training.statsDump)
  // Trim the file to start from scratch
  FileUtils.write(statsDump, "", Charset.defaultCharset)
  var successes = 0

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

  for(ep <- 1 to numEpisodes){
    if(!converged || ep < targetUpdate) {
      logger.debug(s"Epoch $ep")
      val agent = new PolicyAgent(policy)
      val instance = streamIterator.next()
      val outcome = agent.runEpisode(instance, Some(trainingObserver))

      val successful = outcome.nonEmpty
      if (successful)
        successes += 1

      if (ep % targetUpdate == 0) {
        val successRate = successes / targetUpdate.toFloat
        logger.info(s"Current episode: $ep out of $numEpisodes")
        logger.info(s"Current ε = ${policy.currentEpsilon.get}")
        logger.info(s"Success rate of $successRate for the last $targetUpdate episodes")
        successes = 0
        val (iterationDist, documentDist) = computeStats(stats)
        logger.info(s"Iterations:\n${prettyPrintMap(iterationDist)}")
        logger.info(s"Papers read:\n${prettyPrintMap(documentDist)}")
        FileUtils.writeLines(statsDump, stats.map(_.toString).asJava, true)
        stats.clear()
        updateParameters(network)
        logger.info(s"Saving checkpoint as $checkpointName")
        network.save(checkpointName)
      }
    }
  }

  // Do dataset split
  // Define the batch size
  // Collect the observations
  // Perform the update
  // Test for convergence
}
