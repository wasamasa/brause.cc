OBJS = stoll.o quotes.o
SCM = csc
COMPILE.scm = $(SCM) -c

%.o: %.scm
	$(COMPILE.scm) $(OUTPUT_OPTION) $<

all: stoll

stoll: $(OBJS)
	$(SCM) $(OUTPUT_OPTION) $(OBJS)

clean:
	$(RM) $(OBJS) stoll

.PHONY: all clean
