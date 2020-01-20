SEEDS="1024 0 6754 9802 4729 100"
#SEEDS="1024"
PROBS="0.0 .0625 .125 .1875 .25 .5 .75 1"
#PROBS="1.0"

for SEED in $SEEDS
do
  CASCADE_OUTPUT="cascade_${SEED}.tsv"
  sbt -Dconfig.file=application_deathstar.conf -Drandom.seed="$SEED" -Dtesting.agentType=Cascade -Dtesting.statsDump="$CASCADE_OUTPUT" "runMain org.ml4ai.learning.TestFR"
  for PROB in $PROBS
  do
    OUTPUT="balanced_random_${PROB}_${SEED}.tsv"
    sbt -Dconfig.file=application_deathstar.conf -Drandom.seed="$SEED" -Dtesting.exploreBias="$PROB" -Dtesting.statsDump="$OUTPUT" "runMain org.ml4ai.learning.TestFR"

  done
done
