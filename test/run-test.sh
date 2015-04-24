#!/bin/bash

intersect=$1

cat data/test.dat | $intersect
tac data/test.dat | $intersect
cat data/test.dat | test/swap-points.sh | $intersect
tac data/test.dat | test/swap-points.sh | $intersect
