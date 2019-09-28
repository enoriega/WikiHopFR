package org.ml4ai.agents

import org.ml4ai.learning.DQN

class GreedyPolicy(network:DQN) extends EpGreedyPolicy(Stream.continually(0.0).iterator, network)(org.ml4ai.utils.buildRandom(0))
