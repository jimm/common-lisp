NAME = osx_midi
SRC = $(NAME).c
OBJ = $(NAME).o
BUNDLE = $(NAME).dylib

all: $(BUNDLE)

$(BUNDLE): $(SRC)
	gcc -o $(OBJ) -c $(SRC)
	gcc -dynamic -bundle -undefined suppress -flat_namespace \
	    -framework CoreMIDI -framework Foundation -o $(BUNDLE) $(OBJ)

clean:
	rm -f $(BUNDLE) $(OBJ)
