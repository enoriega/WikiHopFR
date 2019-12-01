package org.ml4ai.inference

import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.{Document, Sentence}
import org.ml4ai.inference.viz.DotEnabled
import org.ml4ai.utils.AnnotationsLoader
import org.ml4ai.utils._
import org.ml4ai.utils.entityGroundingHash
import scalax.collection.Graph
import scalax.collection.edge.LBase.{LEdge, LEdgeImplicits}
import scalax.collection.edge.Implicits._
import scalax.collection.edge.{LDiEdge, LUnDiEdge}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class KBLabel(relation:Relation)

abstract class KnowledgeGraph(documents:Iterable[(String,Document)]) extends DotEnabled with LazyLogging{

  protected lazy val groupedEntityHashes: Map[Set[String], Int] = {

    def equivalent(a:Set[String], b:Set[String]) = {
      val largeSize =
        if(a.size >= b.size)
          a.size
        else
          b.size

      val intersection = a intersect b

      if(intersection.size.toDouble / largeSize.toDouble >= .5)
        true
      else
        false
    }

    val allLemmas = entityLemmaHashes.keySet

    (for{
      current <- allLemmas
      candidate <- allLemmas
      //if all(current map candidate.contains)
      if equivalent(current, candidate)
    } yield {
      (current, candidate)
    }).groupBy(_._1)
      .mapValues{
        elems =>
          val representative = elems.map(_._2).toSeq.maxBy(_.size)
          entityLemmaHashes(representative)
      } ++ Map(Set.empty[String] -> 0)
  }

  protected lazy val reversedGroupEntityHashes: Map[Int, Set[String]] = groupedEntityHashes map { case (k, v) => v -> k }


  protected def entityHashToText(hash:Int):Set[String] = reversedGroupEntityHashes(hash)


  protected def matchToEntities(text:String):Iterable[Set[String]] = {
    val lemmas = lemmatize(text) map (_.toLowerCase)
    val buckets = entityLemmaHashes.keys
    //val buckets = groupedEntityHashes.keys

    buckets filter {
      bucket =>
        if(bucket.size > lemmas.size)
          all(lemmas map bucket.contains)
        else
          all(bucket map lemmas.contains)
    }
  }

  /**
    * Extracts relation instances from a document object.
    * The endpoints should be expressed as "lemma hashes" and each relation must carry its attributing element
    * @param hash Wikipedia doc md5 hash string
    * @param doc Processor's doc annotated instance
    * @return An iterable with triples which have the following signature: Source hash, destination has, attribution
    */
  protected def extractRelationsFromDoc(hash:String, doc:Document):Iterable[(Int, Int, AttributingElement)]


  /**
    * Provides a precomputed map of all the entities' lemma hash and a text representation for all the entities present
    * in the document support set of this graph
    * @return The hashes of the set of post-processed lemmas on the collection
    */
  protected def buildEntityLemmaHashes: Map[Set[String], Int]

  protected lazy val entityLemmaHashes: Map[Set[String], Int] = buildEntityLemmaHashes

  // Knowledge relation instances
  protected lazy val relations:Iterable[Relation] =
    documents.flatMap{
      d => extractRelationsFromDoc(d._1, d._2)

    }.groupBy(t => (t._1, t._2)).map{
      case (k, v) =>
        val sourceHash = k._1
        val destinationHash = k._2
        val attributions = v.map(_._3)
        Relation(sourceHash, destinationHash, attributions)
    }

  private object MyImplicit extends LEdgeImplicits[KBLabel]; import MyImplicit._

  // Build graph
  protected lazy val graph: Graph[Int, LUnDiEdge] = Graph.from(edges = relations map {
    r =>
      (r.sourceHash ~+ r.destinationHash)(KBLabel(r))
  })

  // Entities belonging to a graph
  lazy val entities:Iterable[Set[String]] = graph.nodes map (e => entityHashToText(e.value))
  lazy val entityTypes:mutable.Map[Set[String], Set[String]] = new mutable.HashMap()
  lazy val edges: collection.Set[VerboseRelation] = graph.edges map (r => relationToVerboseRelation(r.value.relation))
  lazy val degrees: Map[Set[String], Int] = (graph.nodes map (n => entityHashToText(n.value) -> n.degree)).toMap
  protected lazy val pathCache: mutable.Map[(String, String), Iterable[Seq[VerboseRelation]]] = new mutable.HashMap[(String, String), Iterable[Seq[VerboseRelation]]].withDefaultValue(Seq.empty)

  def findPath(source:String, destination:String):Iterable[Seq[VerboseRelation]] =
    pathCache.getOrElseUpdate((source, destination),
      {
        val sourceCandidates = matchToEntities(source)
        if(sourceCandidates.isEmpty)
          throw new NotGroundableElementException(source)

        val destinationCandidates = matchToEntities(destination)
        if(destinationCandidates.isEmpty)
          throw new NotGroundableElementException(destination)

        if(sourceCandidates == destinationCandidates)
          throw new SameGroundedEndpointsException(source, destination)

        (for{
          src <- sourceCandidates
          dst <- destinationCandidates
        } yield {

          //val sh = entityLemmaHashes.getOrElse(src, -1)
          //val dh = entityLemmaHashes.getOrElse(dst, -1)

          val sh = groupedEntityHashes.getOrElse(src, -1)
          val dh = groupedEntityHashes.getOrElse(dst, -1)


          if (sh == -1)
            logger.error(s"$src non-hashable")
          if (dh == -1)
            logger.error(s"$dst non-hashable")

          if(sh == dh)
            throw new SameGroundedEndpointsException(source, destination)

          Try {
            val s = graph get sh
            val d = graph get dh

            s shortestPathTo d match {
              case Some(path) =>
                logger.debug(s"${path.length}")
                Some(
                  path.edges.map{
                    edge => relationToVerboseRelation(edge.relation)
                  }.toSeq
                )
              case None =>
                logger.debug("No path")
                None
            }
          }

        }) collect {
          case Success(Some(path)) => path
        }
      }
    )

  def shareConnectedComponent(a:Set[String], b:Set[String]):Boolean = {
    val sh = groupedEntityHashes.getOrElse(a, -1)
    val dh = groupedEntityHashes.getOrElse(b, -1)

    if (sh == -1)
      logger.error(s"$a non-hashable")
    if (dh == -1)
      logger.error(s"$b non-hashable")

    if(sh == dh)
      throw new SameGroundedEndpointsException(a.mkString(" "), b.mkString(" "))

    val s = graph get sh
    val d = graph get dh

    s pathTo d match {
      case Some(_) =>true
      case None => false
    }
  }

  protected def relationToVerboseRelation(relation:Relation):VerboseRelation =
    VerboseRelation(
      entityHashToText(relation.sourceHash),
      entityHashToText(relation.destinationHash),
      relation.attributions
    )
}