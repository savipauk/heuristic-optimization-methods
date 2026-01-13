main: main.cc
	g++ -std=c++23 -Wall -Wpedantic -Werror main.cc -o main

run: main
	./main instances/inst1.txt
