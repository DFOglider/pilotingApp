all: 01.out 02.out 03.out 04.out

%.out: %.R
	R --no-save < $< > $@

clean:
	-rm *.out *.png *.pdf
