package org.ml4ai.mdp

import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.HttpClient
import org.apache.http.impl.client.HttpClients
import org.json4s.JsonAST.{JArray, JDouble}
import org.json4s.jackson.JsonMethods.{compact, render}
import org.json4s.native.JsonMethods._
import org.ml4ai.inference._
import org.ml4ai.ir.LuceneHelper
import org.ml4ai.mdp.WikiHopEnvironment.buildKnowledgeGraph
import org.ml4ai.utils.{AnnotationsLoader, HttpUtils, WikiHopParser, buildRandom, sigmoid}
import org.ml4ai.{WHConfig, WikiHopInstance}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

class WikiHopEnvironment(val id:String, val start:String, val end:String, documentUniverse:Option[Set[String]] = None) extends Environment with LazyLogging {

  private implicit val loader:AnnotationsLoader = WikiHopEnvironment.annotationsLoader


  def this(wikiHopKey:String) {
    this(wikiHopKey, WikiHopEnvironment.getTrainingInstance(wikiHopKey).query.split(" ").last, WikiHopEnvironment.getTrainingInstance(wikiHopKey).answer.get)
  }

  def this(wikiHopKey:String, documentUniverse:Option[Set[String]]) {
    this(wikiHopKey, WikiHopEnvironment.getTrainingInstance(wikiHopKey).query.split(" ").last, WikiHopEnvironment.getTrainingInstance(wikiHopKey).answer.get, documentUniverse)
  }

  // Control values
  val maxIterations: Int = WHConfig.Environment.maxIterations
  val immediateRewardEnabled: Boolean = WHConfig.Environment.immediateRewardEnabled

  // State variables
  private var knowledgeGraph:Option[KnowledgeGraph] = None
  private var iterationNum:Int = 0
  private val exploredEntities = new mutable.HashSet[Set[String]]
  private val exploitedEntities = new mutable.HashSet[Set[String]]
  private val entityIntroductionJournal = new mutable.HashMap[Set[String], Int].withDefaultValue(0)
  private val entityUsageJournal = new mutable.HashMap[Set[String], Int].withDefaultValue(0)
  private val papersRead = new mutable.HashSet[String]
  private val rng = buildRandom()
  private val startTokens = start.split(" ").toSet
  private val endTokens = end.split(" ").toSet



  private def exploitationEligible(e: Set[String]) = !(exploitedEntities contains e)
  private def explorationEligible(e: Set[String]) = !(exploredEntities contains e)



//  override def possibleActions: Seq[Action] = {
//    // Generate the possible actions to be taken given the current state of the environment
//    val actions =
//      knowledgeGraph match {
//        // If this is the first call and there isn't a KG yet, generate the actions given those two nodes
//        case None =>
//          (if(!WHConfig.Environment.excludeExplorationSingle)
//            List(Exploration(endTokens), Exploration(startTokens))
//          else
//            Nil) :::
//          List(
//            ExplorationDouble(startTokens, endTokens),
//            Exploitation(startTokens, endTokens)
//          )
//        // Otherwise, procedurally generate the list of actions
//        case Some(kg) =>
//          val currentEntities = kg.entities
//
//          val ret = new mutable.ListBuffer[Action]
//
//          currentEntities foreach {
//            e =>
//
//              if(!WHConfig.Environment.excludeExplorationSingle) {
//                if (explorationEligible(e))
//                  ret += Exploration(e)
//              }
//
//              if(exploitationEligible(e))
//                ret += Exploitation(e, end.split(" ").toSet)
//
//              for(i <- currentEntities if i != e && explorationEligible(i)){
//                ret += ExplorationDouble(e, i)
//              }
//          }
//
//          ret.toList
//      }
//
//
//    if(actions.isEmpty)
//      throw new ActionStarvationException
//
//    // Prepend the random action to the list of candidate actions
//    RandomAction :: actions
//  }

  override def possibleActions: Seq[Action] = {
    // Generate the possible actions to be taken given the current state of the environment
    val actions =
      knowledgeGraph match {
        // If this is the first call and there isn't a KG yet, generate the actions given those two nodes
        case None =>
          (if(!WHConfig.Environment.excludeExplorationSingle)
            List(Exploration(endTokens), Exploration(startTokens))
          else
            Nil) :::
            List(
              ExplorationDouble(startTokens, endTokens),
              Exploitation(startTokens, endTokens)
            )
        // Otherwise, procedurally generate the list of actions
        case Some(kg) =>
          val currentEntities = this.topEntities

          val ret = new mutable.ListBuffer[Action]

          for{
            ea <- currentEntities
            eb <- currentEntities
            if ea != eb
          } {
            ret += ExplorationDouble(ea, eb)
            ret += Exploitation(ea, eb)
          }


//
//          currentEntities foreach {
//            e =>
//
////              if(!WHConfig.Environment.excludeExplorationSingle) {
////                if (explorationEligible(e))
////                  ret += Exploration(e)
////              }
////
////              if(exploitationEligible(e))
//                ret += Exploitation(e, end.split(" ").toSet)
//
////              for(i <- currentEntities if i != e && explorationEligible(i)){
//                ret += ExplorationDouble(e, i)
////              }
//          }

          ret.toList
      }


    if(actions.isEmpty)
      throw new ActionStarvationException

    // Prepend the random action to the list of candidate actions
    RandomAction :: actions
  }

  private val averagePapersReq:Int = WikiHopEnvironment.papersRequired.values.sum / WikiHopEnvironment.papersRequired.size
  /**
    * Computes the scalar reward of taking the current action
    * @param action taken
    * @return
    */
  private def rewardSignal(action: Action, oldState:Option[KnowledgeGraph], fetchedPapers:Set[String], immediateRewardEnabled:Boolean = false):Double = {

    val newPapers: Int = (fetchedPapers diff papersRead).size

    val newRelations: Int =
      knowledgeGraph match {
        case Some(kg) =>
          (kg.edges diff (oldState match {
            case Some(kg) => kg.edges;
            case None => Set.empty
          })).size
        case None =>
          0
      }


//    val successReward =
//      if (WikiHopEnvironment.papersRequired.contains(id)) {
//        Seq(
//          WikiHopEnvironment.papersRequired(id) - papersRead.size,
//          -1
//        ).min
//      }
//      else {
//        logger.error(s"Key $id not found in papers required")
//        Seq(averagePapersReq - papersRead.size, -1).min
//      }
//
//
//
//    val failureReward = - papersRead.size

    val successReward = - (papersRead.size^2)
    val failureReward = - (papersRead.size^2)

//    val successReward = WHConfig.Environment.successReward
//    val failureReward = WHConfig.Environment.failureReward

    val livingReward = WHConfig.Environment.livingReward
    val sigmoidFactor = successReward*0.5 // TODO Parameterize the ratio

    val outcomeReward: Double =
      if (finishedEpisode) {
        if (outcome.nonEmpty)
          successReward
        else
          failureReward
      }
      else
        0

    if(immediateRewardEnabled) {
      val informationRatio = if (newPapers > 0) newRelations / newPapers else 0

      val scaledInformationRatio = sigmoid(informationRatio - sigmoidFactor) * sigmoidFactor

      val standardReward = scaledInformationRatio - livingReward

      standardReward + outcomeReward
    }
    else
      outcomeReward
  }

  /**
    * Contains the last action executed by the environment after processing a "Meta-Action"
    * For example, If the random action was chosen, which concrete action was sampled?
    */
  var lastConcreteAction:Option[Action] = None
  var numDocumentsAdded:Int = 0
  var entitySelectionList:List[(Set[String], Set[String])] = Nil

  override def execute(action: Action, persist: Boolean): Double = {
    // Clear the previously observed state
    cachedObservation = None
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

    // Increase the count of times the entities have been subject of an action
    entityUsageJournal(chosenEntities._1) += 1
    if(chosenEntities._1 != chosenEntities._2)
      entityUsageJournal(chosenEntities._2) += 1

    // Store them
    entitySelectionList = chosenEntities::entitySelectionList

    lastConcreteAction = Some(finalAction)
    // Generate new KG from the documents
    val expandedDocuments = fetchedDocs union papersRead
    val oldState = knowledgeGraph
    if(expandedDocuments.nonEmpty)
      knowledgeGraph = Some(buildKnowledgeGraph(expandedDocuments))
    // Keep track of how many documents were added
    numDocumentsAdded = (fetchedDocs diff papersRead).size

    // Update the knowledge graph and keep track of the new papers
    papersRead ++= fetchedDocs

    val reward = rewardSignal(action, oldState, fetchedDocs, immediateRewardEnabled)



    // Update the journal with the new entities
    knowledgeGraph match {
      case Some(kg) =>
        for(entity <- kg.entities){
          entityIntroductionJournal.getOrElseUpdate(entity, iterationNum)
        }
      case None => ()
    }


    reward
  }

  private def selectActionRandomly:Action = {
    // Select an index randomly
    // Subtract one because we won't consider the element corresponding to the random action
    val ix = rng.nextInt(possibleActions.size - 1)
    // The first element is the random action, therefore we will operate on the tail
    possibleActions.tail(ix)
  }

  // Computes safely how many times an entity has been subject of an action
  private def timesUsed(entity:Set[String]):Int = entityUsageJournal.getOrElse(entity, 0)

  private var cachedObservation:Option[WikiHopState] = None
  def observeState:WikiHopState = cachedObservation match {
    case None =>
      val (numNodes, numEdges) = knowledgeGraph match {
        case Some(kg) =>
          (kg.entities.size, kg.edges.size)
        case None =>
          (0, 0)
      }

      val tEntities = topEntities
      val iterationsOfIntroduction = tEntities map entityIntroductionJournal
      val ranks = tEntities map {
        entity =>
          knowledgeGraph match {
            case Some(kg) => kg.degrees(entity)
            case None => 0
          }
      }
      val entityUsage = tEntities map timesUsed

      val pairs =
        for{
          ea <- tEntities
          eb <- tEntities
          if ea != eb
        } yield (ea, eb)

      // TODO clean this up
      val pairwiseComponents =
        pairs.map{
          case (ea, eb) =>
            val key = (ea, eb)
            val value =
              knowledgeGraph match {
                case Some(kg) =>
                  kg.shareConnectedComponent(ea, eb)
                case None => false
              }

            value
        }




      val (exploreScores, exploitScores) = (luceneExplorationScore(pairs), luceneExploitationScore(pairs))
      if(exploitScores.nonEmpty)
        logger.debug(s"Exploit score:\t${exploitScores.max}")
      if(exploreScores.nonEmpty)
        logger.debug(s"Explore score:\t${exploreScores.max}")

      val ret =
        WikiHopState(iterationNum, numNodes, numEdges, startTokens, endTokens,
          Some(tEntities), iterationsOfIntroduction, ranks, entityUsage, pairwiseComponents, if(finishedEpisode) outcome.nonEmpty else false, exploreScores, exploitScores)

      cachedObservation = Some(ret)
      ret
    case Some(obs) => obs
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

  private var result:Option[Iterable[Seq[VerboseRelation]]] = None
  def outcome:Iterable[Seq[VerboseRelation]] =

    result match {
      case Some(v) => v
      case None =>
        knowledgeGraph match {
          case Some(kg) =>
            Try(kg.findPath(start, end)) match {
              case Success(v) =>
                if(v.nonEmpty)
                  result = Some(v)
                v
              case Failure(e) =>
                logger.error(s"$e - ${e.getMessage}")
                Seq.empty
            }
          case None =>
            Seq.empty
        }
    }



  def iterations:Int = iterationNum
  def consultedPapers:Set[String] = papersRead.toSet

  def entityDegrees: Map[Set[String], Int] = knowledgeGraph match {
    case Some(kg) => kg.degrees
    case None => Map.empty
  }

  def entities:Set[Set[String]] = knowledgeGraph match {
    case Some(kg) => kg.entities.toSet
    case None => Set.empty
  }

  def readDocumentUniverse(): Unit = documentUniverse match {
    case None => ()
    case Some(docs) =>
      this.knowledgeGraph = Some(buildKnowledgeGraph(docs))
  }

  ///////////////////// Entity ranking criteria
  /**
    * Returns the euclidian distance of the pair of entities in the embeddings' vector space
    */
  private def distance(pairs:Seq[(Set[String], Set[String])]):Seq[Float] = {
    import WikiHopEnvironment.httpClient
    import org.json4s.JsonDSL._

    val payload =
      compact {
        render {
          for((a, b) <- pairs) yield
            ("A" -> a) ~ ("B" -> b)
        }
      }

    val response = HttpUtils.httpPut("distance", payload)

    val ret =
      for{
        JArray(vals) <- parse(response)
        JDouble(distance) <- vals
      } yield distance.toFloat

    ret
  }

  /**
    * Returns the added degree of the entities in the knowledge graph
    */
  private def combineDegree(pairs:Seq[(Set[String], Set[String])]):Seq[Float] = {
    pairs map {
      case (ea, eb) =>
        this.knowledgeGraph match {
          case Some(kg) =>
            (kg.degrees.getOrElse(ea, 0) + kg.degrees.getOrElse(eb, 0)).toFloat
          case None => 0f
        }
    }
  }

  /**
    * Returns the average lucene score for an exploit action of the pair of entities
    */
  private def luceneExploitationScore(pairs:Seq[(Set[String], Set[String])]):Seq[Float] = {
    // For each pair, get the average lucene score of its Exploit action as a proxy for their relevance
    pairs map {
      case (ea, eb) =>
        // Build the action
        val act = Exploitation(ea, eb)
        // Score the action as the average lucene score for its results
        LuceneHelper.scoreAction(act)
    }
  }

  /**
    * Returns the average lucene score for an explore action of the pair of entities
    */
  private def luceneExplorationScore(pairs:Seq[(Set[String], Set[String])]):Seq[Float] = {
    // For each pair, get the average lucene score of its Exploit action as a proxy for their relevance
    pairs map {
      case (ea, eb) =>
        // Build the action
        val act = ExplorationDouble(ea, eb)
        // Score the action as the average lucene score for its results
        LuceneHelper.scoreAction(act)
    }
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * @return top entities to be considered as target of an action
    */
  def topEntities:Seq[Set[String]] = {
    // Fetch the last set of entities chosen
    entitySelectionList match {
      // If there are no entities selected yet, return the end points
      case Nil =>
        Seq(this.startTokens, this.endTokens)
      case (lastA, lastB)::_ =>
        // Pre-compute a set for efficiency
        val previouslyChosen = entitySelectionList.toSet

        // Get all the possible pairs to consider and discard the already chosen
        val newPairs = knowledgeGraph match {
          case Some(kg) =>
            for{
              candidate <- kg.entities
            } yield { Seq((lastA, candidate), (lastB, candidate), (startTokens, candidate), (endTokens, candidate))}
          case None =>
            Seq.empty
        }

        val criteria = WHConfig.Environment.entitySelection.toLowerCase match {
          case "distance" => distance _
          case "rank" => combineDegree _
          case "lucene" => luceneExploitationScore _
          case t =>
            val message = s"Unimplemented entity selection criteria: $t"
            logger.error(message)
            throw new NotImplementedError(message)
        }

        // Filter out the elements that have been tested before
        val pairsToTest =
          newPairs.toSeq.flatten.filter{
            case (a, b) =>
              if(a == b)
                false
              else if(previouslyChosen contains ((a, b)))
                false
              else if(previouslyChosen contains ((b, a)))
                false
              else
                true
          }

        // Compute their distances in vector space
        val scores = criteria(pairsToTest)

        // Take the top N entities by their distance
        (pairsToTest zip scores).sortBy(_._2).map(_._1._2).distinct.take(WHConfig.Environment.topEntitiesNum)

    }
  }

}

object WikiHopEnvironment extends LazyLogging {

  implicit val httpClient:HttpClient = HttpClients.createDefault

  val papersRequired:Map[String, Int] = {
    import org.ml4ai.utils.using
    using(Source.fromFile("min_docs.tsv")){
      source =>
        source.getLines().map{
          line =>
            val tokens = line.split("\t")
            val (key, value) = (tokens.head, tokens.tail.length)
            key -> value
        }.toMap
    }
  }

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