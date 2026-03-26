all: app

app: main.o random_generation.o gause_method.o matrix_processing.o lu_decomposition.o
	gfortran -o app main.o random_generation.o gause_method.o matrix_processing.o lu_decomposition.o

main.o: main.f03 gause_method.o random_generation.o matrix_processing.o lu_decomposition.o
	gfortran -c main.f03

random_generation.o: random_generation.f03
	gfortran -c random_generation.f03

gause_method.o: gause_method.f03  
	gfortran -c gause_method.f03

matrix_processing.o: matrix_processing.f03
	gfortran -c matrix_processing.f03

lu_decomposition.o: lu_decomposition.f03
	gfortran -c lu_decomposition.f03

.PHONY: clean
clean:
	del /Q *.o *.mod app.exe
