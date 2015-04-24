awk 'BEGIN{FS=";"}; {split($2, arr, "m"); print $1 FS $2 FS (arr[1]*60 + arr[2])}' haskell-times.csv > haskell-times-sec.csv

