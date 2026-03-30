OBJ = random_generation.o gause_method.o matrix_processing.o lu_decomposition.o utilities.o func_wrapper.o main.o

app: $(OBJ)
	gfortran -o app $^

%.o : %.f03
	gfortran -c $<

.PHONY: clean
clean:
	del /Q *.o *.mod app.exe
