stoll: stoll.o quotes.o
	csc -o stoll stoll.o quotes.o

stoll.o: stoll.scm
	csc -c stoll.scm

quotes.o: quotes.scm
	csc -c quotes.scm

clean:
	rm -f stoll.o quotes.o stoll

.PHONY: all clean
