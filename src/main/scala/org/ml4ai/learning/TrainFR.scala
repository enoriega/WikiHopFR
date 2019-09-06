package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
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

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

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
    var actionLog:ListBuffer[Action] = new ListBuffer[Action]()

    override def startedEpisode(env: WikiHopEnvironment): Unit = ()

    override def beforeTakingAction(action: Action, env: WikiHopEnvironment): Unit = {
      // Save the state observation before taking the action
      state = Some(env.observeState)
    }

    override def actionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = ()


    override def concreteActionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {
      assert(state.isDefined, "The state should be defined at this point")
      val newState = env.observeState
      val transition = Transition(state.get, action, reward, newState)
      memory remember transition
      state = None
      actionLog += action
    }

    override def endedEpisode(env: WikiHopEnvironment): Unit = {
      val id = env.id
      val numIterations = env.iterations
      val papersRead = env.consultedPapers.size

      stats += EpisodeStats(id, numIterations, papersRead, actionLog.toList)
      actionLog.clear()
    }

    override def registerError(throwable: Throwable): Unit = {
      logger.error(throwable.getMessage)
    }
  }

  val checkpointName = WHConfig.Training.modelName
  var successes = 0

  for(ep <- 1 to numEpisodes){
    if(!converged || ep < targetUpdate) {
      logger.info(s"Epoch $ep")
      val agent = new PolicyAgent(policy)
      val instance = streamIterator.next()
      val outcome = agent.runEpisode(instance, Some(trainingObserver))

      val successful = outcome.nonEmpty
      if (successful)
        successes += 1

      if (ep % targetUpdate == 0) {
        val successRate = successes / targetUpdate.toFloat
        logger.info(s"Success rate of $successRate for the last $targetUpdate episodes")
        successes = 0
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
