all: timeOverLines.png timeOverLines2.png

timeOverLines.png timeOverLines2.png: test-1000-50000.dat
	R --vanilla < auswertung.R > /dev/null

test-1000-50000.dat: test-1000-2000-_-50000.txt
	<test-1000-2000-_-50000.txt awk '!/inputs/' | awk 'NF==1;NF>1{split($$3,arr,":");print arr[1]*60+arr[2]}' | awk '{if(a){print a FS $$0; a=0} else a=$$0}' | awk 'BEGIN{i=1000};{print i FS $$0; i+=1000}' >test-1000-50000.dat

clean:
	rm -f *.dat *.png Rplots.pdf
