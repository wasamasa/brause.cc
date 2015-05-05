objects = stoll.o quotes.o

stoll: $(objects)
	csc -o $@ $(objects)

%.o: %.scm
	csc -c -o $@ $<

clean:
	rm -f $(objects)

squeaky: clean
	rm -f stoll

.PHONY: all clean
