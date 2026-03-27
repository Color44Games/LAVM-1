app: main.o random_generation.o gause_method.o matrix_processing.o lu_decomposition.o utilities.o func_wrapper.o
	gfortran -o app main.o  random_generation.o gause_method.o matrix_processing.o lu_decomposition.o utilities.o func_wrapper.o

main.o: main.f03 gause_method.o random_generation.o matrix_processing.o lu_decomposition.o utilities.o func_wrapper.o
	gfortran -c main.f03

%.o : %.f03
	gfortran -c $<

.PHONY: clean
clean:
	del /Q *.o *.mod app.exe
