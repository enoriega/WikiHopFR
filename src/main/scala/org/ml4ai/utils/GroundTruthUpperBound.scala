package org.ml4ai.utils

import org.ml4ai.ie.WikificationEntities
import org.ml4ai.inference.{AttributingElement, KBLabel, Relation}
import scalax.collection.Graph
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.Implicits._

object GroundTruthUpperBound extends App {

  // First read the wikification DB
  val wikificationEntities = WikificationEntities.readWikificationDataBase()

  // Now put together the edges of entities
  val edges =
    wikificationEntities.par.flatMap{
      case (docHash, docValues) =>
      // Group by sentence index
      val bySent = docValues.groupBy(e => e.sentence)
      // Generate an "edge" (relation) per each of the different concepts that appear on the same line
        bySent.flatMap{
          case (senIx, entriesInLine) =>
            for{
              a <- entriesInLine.distinct
              b <- entriesInLine.distinct
              if a.wikiId != b.wikiId
            } yield Relation(a.wikiId, b.wikiId, Seq(AttributingElement(None, senIx, docHash)))
        }
    }

  // Cluster together the edges with the same endpoints and aggregate the attributions
  val groupedEdges = edges.groupBy(r => (r.sourceHash, r.destinationHash)).mapValues{
    rels =>
      Relation(rels.head.sourceHash, rels.head.destinationHash, rels.flatMap(_.attributions).seq)
  }.values

  // How many edges are in the global graph?
  println(groupedEdges.size)

  // Build THE global graph
  // Build graph
  val graph: Graph[Int, LUnDiEdge] = Graph.from(edges = groupedEdges.map{
    r =>
      (r.sourceHash ~+ r.destinationHash)(KBLabel(r))
  }.seq)

  print(graph.size)
}
