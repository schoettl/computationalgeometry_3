#!/bin/bash
# Run the intersect program

usage="usage: $0 (c++|haskell) [1|2|3|all]"

if [[ $# == 0 ]]; then
    echo "$usage" >&2
    exit 1
fi

cd "$(dirname "$0")/.."
pwd

case $1 in
    h*) cmd=haskell/dist/build/intersect/intersect ;;
    c*) cmd=cpp/intersect ;;
    *)  cmd=false ;;
esac

case $2 in
    1) inputfile=data/s_1000_1.dat ;;
    2) inputfile=data/s_10000_1.dat ;;
    3) inputfile=data/s_100000_1.dat ;;
    *) inputfile=all ;;
esac

if [[ $cmd == false ]]; then
    echo "specify executable: c++ or haskell" >&2
    exit 1
fi

if [[ $inputfile == all ]]; then
    for inputfile in data/*.dat; do
        echo processing $inputfile
        time $cmd < "$inputfile"
    done
else
    echo processing $inputfile
    time $cmd < "$inputfile"
fi
