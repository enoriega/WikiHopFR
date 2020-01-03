package org.ml4ai.learning

import java.util.UUID

import org.ml4ai.mdp.WikiHopState
import org.sarsamora.actions.Action
import org.sarsamora.states.State

case class Transition(state:WikiHopState, stateId:UUID, action:Action, reward:Float, nextState:WikiHopState, nextStateId:UUID)
