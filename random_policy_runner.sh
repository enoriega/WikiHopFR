SEEDS="1024 0 6754 9802 4729 100"
PROBS="0 .25 .5 .75 1"

for PROB in $PROBS
do
  for SEED in $SEEDS
  do
    OUTPUT="balanced_random_${PROB}_${SEED}.tsv"
    sbt -Dconfig.file=application_deathstar.conf -Drandom.seed="$SEED" -Dtesting.exploreBias="$PROB" -Dtesting.statsDump="$OUTPUT" "runMain org.ml4ai.learning.TestFR"

  done
done
