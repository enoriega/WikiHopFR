package org.ml4ai.utils

import org.ml4ai.WikiHopInstance
import org.ml4ai.ir.LuceneHelper

object SupportDocs {

  def localDocs(instance:WikiHopInstance):Set[String] = instance.supportDocs.map(md5Hash).toSet

  def randomDocs(instance:WikiHopInstance):Set[String] = {

    val randomDocs = rng.shuffle(WikiHopParser.trainingInstances).take(100).flatMap(_.supportDocs.map(md5Hash)).toSet.take (100)
    localDocs(instance) union randomDocs
  }

  def relatedDocs(instance:WikiHopInstance):Set[String] = {
    val source: String = instance.query.split(" ").last
    val destination: String = instance.answer match {
      case Some(ans) => ans
      case None =>
        throw new UnsupportedOperationException("For now, only training instances are supported")
    }

    val relatedDocs = LuceneHelper.getLexicallySimilarDocuments(source.split(" ").toSet, destination.split(" ").toSet)
    localDocs(instance) union relatedDocs.take(100).toSet
  }
}
