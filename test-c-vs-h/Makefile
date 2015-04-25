all: timeOverLines_c.png timeOverLines_h.png

timeOverLines_c.png timeOverLines_h.png: auswertung.R
	R --vanilla < auswertung.R > /dev/null

auswertung.R: run-intersect-c.dat run-intersect-h.dat

run-intersect-c.dat: run-intersect-c.txt
	./make-dat-files-for-R.sh run-intersect-c.txt run-intersect-c.dat

run-intersect-h.dat: run-intersect-h.txt
	./make-dat-files-for-R.sh run-intersect-h.txt run-intersect-h.dat

clean:
	rm -f *.dat *.png Rplots.pdf
