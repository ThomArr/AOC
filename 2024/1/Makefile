CC = g++ -Wall -std=c++11
CCO = $(CC) -c
OBJECTS = tools.o
SRC_FILES = tools.cpp main1.cpp main2.cpp
HEADER_FILES = tools.hpp

all: main1 main2

main1: main1.o $(OBJECTS)
	$(CC) -o main1 main1.o $(OBJECTS)
	./main1 input.txt

main2: main2.o $(OBJECTS)
	$(CC) -o main2 main2.o $(OBJECTS)
	./main2 input.txt

tools.o: tools.cpp
	$(CCO) tools.cpp -o tools.o

main1.o: main1.cpp
	$(CCO) main1.cpp -o main1.o

main2.o: main2.cpp
	$(CCO) main2.cpp -o main2.o

format fmt:
	clang-format -i $(SRC_FILES) $(HEADER_FILES)

clean:
	rm -f *.o main1 main2
