CXX = ca65
CXXIN = src/bricksmasher.asm
CXXOUT = obj/bricksmasher.o
LXX = ld65
LXXIN = obj/bricksmasher.o 
LXXOUT = bin/bricksmasher.nes
FLAGS = -t nes

build:
	$(CXX) $(CXXIN) -o $(CXXOUT) $(FLAGS)
	$(LXX) $(LXXIN) -o $(LXXOUT) $(FLAGS)

clean:
	rm obj/bricksmasher.o 
	rm bin/bricksmasher.nes