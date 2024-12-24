gfortran -c parser.f90
gfortran -o test test.f90 parser.o
./test entry.txt