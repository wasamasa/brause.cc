OBJS = stoll.o quotes.o

stoll: $(OBJS)
	csc -o $@ $(OBJS)

%.o: %.scm
	csc -c -o $@ $<

clean:
	rm -f $(OBJS)

squeaky: clean
	rm -f stoll

.PHONY: all clean
