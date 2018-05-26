all: 01.out

%.out: %.R
	R --no-save < $< >$@

clean:
	-rm *.out *.png *.pdf
