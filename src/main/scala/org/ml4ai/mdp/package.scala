package org.ml4ai

import org.clulab.struct.Interval
import org.json4s.JsonAST.JValue
import org.sarsamora.actions.Action
import org.json4s.JsonDSL._

package object mdp {

  def actionBuilder(actionIx:Int, entityA:Set[String], entityB:Set[String]):Action = {
    if(actionIx == 0){
      if(entityA == entityB)
        Exploration(entityA)
      else
        ExplorationDouble(entityA, entityB)
    }
    else{
//      assert(entityA == entityB, "The action must use the different endpoints")
      actionIx match {
        case 1 =>
          Exploitation(entityA, entityB)
        case ix:Int =>
          throw new NotImplementedError(s"Action with code $ix is not valid")
      }
    }

  }
}
