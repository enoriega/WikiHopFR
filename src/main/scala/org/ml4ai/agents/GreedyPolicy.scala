package org.ml4ai.agents

import org.ml4ai.learning.Approximator

class GreedyPolicy(network:Approximator) extends EpGreedyPolicy(Stream.continually(0.0).iterator, network)(org.ml4ai.utils.buildRandom(0))
