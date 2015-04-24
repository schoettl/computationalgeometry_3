#!/bin/bash
usage="usage: $0 <intersect-program>"

if [[ $# != 1 ]]; then
    echo "$usage" >&2
    exit 1
fi

intersect=$1

cat data/test.dat | $intersect
tac data/test.dat | $intersect
cat data/test.dat | test/swap-points.sh | $intersect
tac data/test.dat | test/swap-points.sh | $intersect
