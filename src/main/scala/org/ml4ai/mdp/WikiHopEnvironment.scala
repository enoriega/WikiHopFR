package org.ml4ai.mdp

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.{WHConfig, WikiHopInstance}
import org.ml4ai.inference._
import org.ml4ai.ir.LuceneHelper
import org.ml4ai.utils.{AnnotationsLoader, WikiHopParser, filterUselessLemmas}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.states.State
import org.ml4ai.utils.buildRandom

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import WikiHopEnvironment.buildKnowledgeGraph
import edu.cmu.dynet.ExpressionVector
import edu.cmu.dynet.{ComputationGraph, Expression}
import org.clulab.embeddings.word2vec
import org.clulab.embeddings.word2vec.Word2Vec
import org.ml4ai.learning.EmbeddingsHelper

class WikiHopEnvironment(val start:String, val end:String, documentUniverse:Option[Set[String]] = None) extends Environment with LazyLogging {

  private implicit val loader:AnnotationsLoader = WikiHopEnvironment.annotationsLoader


  def this(wikiHopKey:String) {
    this(WikiHopEnvironment.getTrainingInstance(wikiHopKey).query.split(" ").last, WikiHopEnvironment.getTrainingInstance(wikiHopKey).answer.get)
  }


  // Control values
  val maxIterations = 10

  // State variables
  private var knowledgeGraph:Option[KnowledgeGraph] = None
  private var currentEntities: Option[Iterable[Seq[String]]] = None

  private var iterationNum:Int = 0
  private val exploredEntities = new mutable.HashSet[Seq[String]]
  private val exploitedEntities = new mutable.HashSet[Seq[String]]
  private val papersRead = new mutable.HashSet[String]
  private val rng = buildRandom()

  val startTokens = start.split(' ').toSeq.distinct.sorted
  val endTokens = end.split(' ').toSeq.distinct.sorted

  private def exploitationEligible(e: Seq[String]) = !(exploitedEntities contains e)
  private def explorationEligible(e: Seq[String]) = !(exploredEntities contains e)



  override def possibleActions: Seq[Action] = {
    // Generate the possible actions to be taken given the current state of the environment
    val actions =
      knowledgeGraph match {
        // If this is the first call and there isn't a KG yet, generate the actions given those two nodes
        case None =>
          List(
            Exploration(startTokens),
            Exploration(endTokens),
            ExplorationDouble(startTokens, endTokens),
            Exploitation(startTokens, endTokens)
          )
        // Otherwise, procedurally generate the list of actions
        case Some(kg) =>
          val ret = new mutable.ListBuffer[Action]

          currentEntities.get foreach { e =>
              if(explorationEligible(e))
                ret += Exploration(e)

              if(exploitationEligible(e))
                ret += Exploitation(e, endTokens)

              currentEntities.get.foreach { currentEntity =>
                if (currentEntity != e && explorationEligible(currentEntity))
                  ret += ExplorationDouble(e, currentEntity)
              }
          }

          ret.toList
      }


    if(actions.isEmpty)
      throw new ActionStarvationException

    // Prepend the random action to the list of candidate actions
    RandomAction :: actions
  }

  /**
    * Computes the scalar reward of taking the current action
    * @param action taken
    * @return
    */
  private def rewardSignal(action: Action, newState:KnowledgeGraph, fetchedPapers:Set[String]):Double = {
    // TODO make the reward function more nuanced
    val newPapers:Int = (fetchedPapers diff papersRead).size
    val newRelations:Int =
      (newState.edges diff
        (knowledgeGraph match {
          case Some(kg) => kg.edges
          case None => Set.empty
        })
      ).size

    val livingReward = WHConfig.Environment.livingReward

    val informationRatio = if(newPapers > 0) newRelations / newPapers else 0

    val standardReward = informationRatio - livingReward

    val outcomeReward: Double =
      if(finishedEpisode) {
        if(outcome.nonEmpty)
          WHConfig.Environment.successReward
        else
          WHConfig.Environment.failureReward
      }
      else
        0

    standardReward + outcomeReward
  }

  /**
    * Contains the last action executed by the environment after processing a "Meta-Action"
    * For example, If the random action was chosen, which concrete action was sampled?
    */
  var lastConcreteAction:Option[Action] = None
  var numDocumentsAdded:Int = 0
  var entitySelectionList:List[(Seq[String], Seq[String])] = Nil

  override def execute(action: Action, persist: Boolean): Double = {
    // Increment the iteration counter
    iterationNum += 1
    // If the random action is selected, transform it to a concrete action randomly
    val (finalAction, fetchedDocs) = action match {
      case Cascade(e1, e2) =>
        val exploitation = Exploitation(e1, e2)
        val fetched = LuceneHelper.retrieveDocumentNames(exploitation, instanceToFilter= documentUniverse)
        if(fetched.nonEmpty)
          (exploitation, fetched)
        else {
          val explore = ExplorationDouble(e1, e2)
          (explore, LuceneHelper.retrieveDocumentNames(explore, instanceToFilter= documentUniverse))
        }
      case RandomAction =>
        val f = selectActionRandomly
        (f, LuceneHelper.retrieveDocumentNames(f, instanceToFilter = documentUniverse))
      case a:Action =>
        (a, LuceneHelper.retrieveDocumentNames(a, instanceToFilter = documentUniverse))
    }

    // Keep track of the entities chosen for the action
    val chosenEntities =
      finalAction match {
        case Exploitation(ea, eb) => (ea, eb)
        case ExplorationDouble(ea, eb) => (ea, eb)
        case Exploration(e) => (e, e)
        case _ =>
          throw new UnsupportedOperationException("Invalid action here")
      }

    // Store them
    entitySelectionList = chosenEntities::entitySelectionList

    lastConcreteAction = Some(finalAction)
    // Generate new KG from the documents
    val expandedDocuments = fetchedDocs union papersRead
    val kg = buildKnowledgeGraph(expandedDocuments)
    val reward = rewardSignal(action, kg, fetchedDocs)

    // Keep track of how many documents were added
    numDocumentsAdded = (fetchedDocs diff papersRead).size

    // Update the knowledge graph and keep track of the new papers
    setKnowledgeGraph(kg)
    papersRead ++= fetchedDocs
    reward
  }

  private def selectActionRandomly:Action = {
    // Select an index randomly
    // Subtract one because we won't consider the element corresponding to the random action
    val ix = rng.nextInt(possibleActions.size - 1)
    // The first element is the random action, therefore we will operate on the tail
    possibleActions.tail(ix)
  }


  def observeState(implicit eh:EmbeddingsHelper):WikiHopState = {
    val (numNodes, numEdges) = knowledgeGraph match {
      case Some(kg) =>
        (kg.entities.size, kg.edges.size)
      case None =>
        (0, 0)
    }

    // TODO Complete this definition with the rest of the features
    WikiHopState(iterationNum, numNodes, numEdges, startTokens, endTokens, Some(topEntities))
  }

  // TODO Deprecate this
  override def observeState: State = {
    val (numNodes, numEdges) = knowledgeGraph match {
      case Some(kg) =>
        (kg.entities.size, kg.edges.size)
      case None =>
        (0, 0)
    }

    // TODO Complete this definition with the rest of the features
    WikiHopState(iterationNum, numNodes, numEdges, startTokens, endTokens, None)
  }

  override def finishedEpisode: Boolean = {
    if(iterationNum >= maxIterations)
      true
      else {
        Try(possibleActions) match {
          case Failure(s:ActionStarvationException) =>
            logger.debug("Action Starvation")
            true
          case _ =>
            knowledgeGraph match {
              case Some(_) =>
                val paths = outcome
                if (paths.nonEmpty)
                  true
                else
                  false
              case None => false
            }
        }
      }
  }

  def outcome:Iterable[Seq[VerboseRelation]] = knowledgeGraph match {
    case Some(kg) =>
      Try(kg.findPath(start, end)) match {
        case Success(v) => v
        case Failure(e) =>
          logger.error(s"$e - ${e.getMessage}")
          Seq.empty
      }
    case None =>
      Seq.empty
  }

  def iterations:Int = iterationNum
  def consultedPapers:Set[String] = papersRead.toSet

  def entityDegrees: Map[Set[String], Int] = knowledgeGraph match {
    case Some(kg) => kg.degrees
    case None => Map.empty
  }

  def entities: Iterable[Seq[String]] = knowledgeGraph match {
    case Some(kg) => this.currentEntities.get
    case None => Seq.empty
  }

  protected def setKnowledgeGraph(knowledgeGraph: KnowledgeGraph): Unit = {
    this.knowledgeGraph = Some(knowledgeGraph)
    this.currentEntities = Some(knowledgeGraph.entities.map { entity => entity.toSeq.sorted })
  }

  def readDocumentUniverse(): Unit = documentUniverse match {
    case None => ()
    case Some(docs) =>
      setKnowledgeGraph(buildKnowledgeGraph(docs))
  }

  private def distance(a:Seq[String], b:Seq[String], helper:EmbeddingsHelper):Float = {
    ComputationGraph.renew()

    val eA = helper.lookup(a)
    val eB = helper.lookup(b)

    val averageA = Expression.average(new ExpressionVector(eA))
    val averageB = Expression.average(new ExpressionVector(eB))

    Expression.l2Norm(averageA - averageB).value().toFloat()
  }

  /**
    * @return top entities to be considered as target of an action
    */
  def topEntities(implicit helper:EmbeddingsHelper):Seq[Seq[String]] = {
    // Fetch the last set of entities chosen
    entitySelectionList match {
      // If there are no entities selected yet, return the end points
      case Nil =>
        Seq(startTokens, endTokens)
      case (lastA, lastB)::tail =>
        // Pre-compute a set for efficiency
        val previouslyChosen = entitySelectionList.toSet
        // Get all the possible pairs to consider and discard the already chosen
        val newPairs =
          for{
            candidate <- currentEntities.get
          } yield { Seq((lastA, candidate), (lastB, candidate))}

        // TODO: Clean this code for legibility
        newPairs.toSeq.flatten.withFilter{
          case (a, b) =>
            if(a == b)
              false
            else if(previouslyChosen contains ((a, b)))
              false
            else if(previouslyChosen contains ((b, a)))
              false
            else
              true
        }.map{
          case (a, b) => (a, b, distance(a, b, helper))
        }.sortBy(_._3).take(WHConfig.Environment.topEntitiesNum).map(_._2)
    }
  }

}

object WikiHopEnvironment extends LazyLogging {


  private def getInstance(data: Iterable[WikiHopInstance], key: String): WikiHopInstance =
    Try(data.filter(_.id == key)) match {
      case Success(values) if values.size == 1 => values.head
      case Success(_) =>
        val msg = "More than one instance with tha same key"
        logger.error(msg)
        throw new UnsupportedOperationException(msg)
      case Failure(exception) =>
        logger.error(exception.getMessage)
        throw exception

    }

  def getTrainingInstance(key: String): WikiHopInstance = getInstance(WikiHopParser.trainingInstances, key)

  def getTestingInstance(key: String): WikiHopInstance = getInstance(WikiHopParser.testingInstances, key)

  lazy val annotationsLoader = new AnnotationsLoader(WHConfig.Files.annotationsFile, cache = WHConfig.Environment.cacheAnnotations)

  def buildKnowledgeGraph(docs: Iterable[String])(implicit loader: AnnotationsLoader): KnowledgeGraph = WHConfig.Environment.knowledgeGraphType match {

    case "Coocurrence" => new CoocurrenceKnowledgeGraph(docs)
    case "OpenIE" => new OpenIEKnowledgeGraph(docs)
    case "NamedEntityLink" => new NamedEntityLinkKnowledgeGraph(docs)
    case t =>
      throw new UnsupportedOperationException(s"Type $t is not a recognized KnowledgeGraph implementation")

  }

}