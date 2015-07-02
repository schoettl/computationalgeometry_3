3. Aufgabe aus Computational Geometry
=====================================

Jakob Schöttl, Markus Stampfl

Ergebnisse von linesweep:

s_100000_1.dat -> 28349
s_10000_1.dat  -> 649
s_1000_1.dat   -> 4
s_1000_10.dat  -> 774


Ergebnisse von 1.

s_1000_10.dat -> 796



erklären, warum...

cat data/xxx.dat | awk '{print $1 RS $3}' | wc -l
cat data/xxx.dat | awk '{print $1 RS $3}' | sort -u | wc -l


erklären, wie optimierung funktionieren würde:
insertIntersections nur mit geänderten nachbarschaften (teilmenge von allen yo linien)
