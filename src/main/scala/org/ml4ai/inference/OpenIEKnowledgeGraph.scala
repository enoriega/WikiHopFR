package org.ml4ai.inference

import org.clulab.processors.{Document, Sentence}
import org.ml4ai.utils.entityGroundingHash
import org.ml4ai.utils.{AnnotationsLoader, filterUselessLemmas}

class OpenIEKnowledgeGraph(documents:Iterable[(String,Document)]) extends KnowledgeGraph(documents) {

  /**
    * Convenience constructor
    * @param documents Md5 hashes for the documents to mine
    * @param loader Implicit AnnotationsLoader instance
    * @return Instance of KnowledgeGraph built from info from the specified documents
    */
  def this(documents:Iterable[String])(implicit loader:AnnotationsLoader) =
    this(documents map (h => (h, loader(h))))

  /**
    * Implementation of the relation extraction from OpenIE relation triples
    * @param hash Wikipedia doc md5 hash string
    * @param doc Processor's doc annotated instance
    * @return An iterable with triples which have the following signature: Source hash, destination has, attribution
    */
  override protected def extractRelationsFromDoc(hash: String, doc: Document): Iterable[(Int, Int, AttributingElement)] = {
    doc.sentences.zipWithIndex.flatMap{
      case (sen, sIx) =>
        implicit val s:Sentence = sen
        s.relations match {
          case Some(rels) =>
            rels map {
              r =>
                val sHash = groupedEntityHashes(filterUselessLemmas(r.subjectLemmas).toSet)
                val dHash = groupedEntityHashes(filterUselessLemmas(r.objectLemmas).toSet)
                if(sHash != 0 && dHash != 0) {
                  Some((sHash, dHash, AttributingElement(Some(r), sIx, hash)))
                }
                else
                  None
            } collect {
              case Some(t) => t
            }
          case None =>
            Seq.empty
        }
    }.toSet
  }


  /**
    * Provides a precomputed map of all the entities' lemma hash and a text representation for all the entities present
    * in the document support set of this graph
    */
  protected override lazy val buildEntityLemmaHashes: Map[Set[String], Int] =
    documents.flatMap{
      d =>
        d._2.sentences.flatMap{
          implicit s =>
            s.relations match {
              case Some(rels) =>
                rels flatMap {
                  r =>
                    val subjectLemmas = filterUselessLemmas(r.subjectLemmas)
                    val objectLemmas = filterUselessLemmas(r.objectLemmas)


                    Seq(
                      subjectLemmas.toSet -> entityGroundingHash(subjectLemmas),
                      objectLemmas.toSet -> entityGroundingHash(objectLemmas)
                    )
                }
              case None => Seq.empty
            }
        }
    }.toMap
}
