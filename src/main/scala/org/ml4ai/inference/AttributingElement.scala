package org.ml4ai.inference

import org.clulab.processors.RelationTriple

case class AttributingElement(triple:Option[RelationTriple], sentenceIx:Int, document:String) extends Serializable
