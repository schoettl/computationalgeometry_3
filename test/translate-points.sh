#!/bin/bash
usage="usage: $0 <deltax> <deltay>"
if [[ $# != 2 ]]; then
    echo "$usage" >&2
    exit 1
fi
awk -v dx="$1" -v dy="$2" '{print $1+dx " " $2+dy " " $3+dx " " $4+dy}'
