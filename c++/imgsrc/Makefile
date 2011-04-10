PROG = imgsrc

CC = g++

CFLAGS = -O2 -I.

all: $(PROG)

OBJS = util.o image_handler.o

%.o: %.cxx 
	$(CC) -c -o $@ $< $(CFLAGS)

$(PROG): $(OBJS)
	$(CC) -o $(PROG) $(PROG).cxx $(OBJS) $(CFLAGS)

clean:
	-rm -f *.o *~ $(PROG)
