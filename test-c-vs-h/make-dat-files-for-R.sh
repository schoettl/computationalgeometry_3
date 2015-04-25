#!/bin/bash
<"$1" awk '/./ && NR>1' \
    | awk 'NF==2;NF==1{print "intersects " $1}' \
    | awk '{if($0~/^processing/) {print $0; printf "lines "; system("wc -l <../" $2)} else print}' \
    | awk '{if($0~/^(real|user|sys)/){split($2, arr, "m"); print $1 FS (arr[1]*60 + arr[2])} else print}' \
    | awk '{print int((NR-1)/6)+1 FS $0}' >"$2"
