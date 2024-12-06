CC = g++ -Wall -std=c++11
CCO = $(CC) -c
SRC_FILES = main1.cpp main2.cpp

all: main1 main2

main1: main1.o 
	$(CC) -o main1 main1.o 
	./main1 input.txt

main2: main2.o 
	$(CC) -o main2 main2.o
	./main2 input.txt

main1.o: main1.cpp
	$(CCO) main1.cpp -o main1.o

main2.o: main2.cpp
	$(CCO) main2.cpp -o main2.o

format fmt:
	clang-format -i $(SRC_FILES)

clean:
	rm -f *.o main1 main2
