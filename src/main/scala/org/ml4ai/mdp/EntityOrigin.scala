package org.ml4ai.mdp

import org.clulab.struct.Interval
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._

object EntityOrigin {
  implicit def serialize(origin:EntityOrigin):JValue = {
    ("hash" -> origin.docHash) ~
      ("sen" -> origin.sentenceIx) ~
      ("interval" -> Seq(origin.wordInterval.start, origin.wordInterval.end))
  }
}

case class EntityOrigin(docHash:String, sentenceIx:Int, wordInterval:Interval)
