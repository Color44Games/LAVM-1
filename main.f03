program main
    use rand_generation
    use gause_method
    use matrix_processing
    use lu_decomposition
    use utilities
    use wrap_function
    implicit none
    real, allocatable :: matrix_base(:, :), right_part(:, :), united_matrix(:, :)
    real, allocatable :: mt_easy(:, :), answers_easy(:), mt_choose(:, :), answers_choose(:)
    real, allocatable :: vec_y(:), answers_lu(:)
    real(8) :: time_easy, time_choose, time_lu_decomposition, time_lu_solve, time_lu
    integer :: x, i, seed_matrix, seed_right_part, sz
    type(matrix_pair) :: lu_matrix

    call execute_command_line("chcp 65001 > nul")
    call random_seed()
    read *, x

    ! Работа программы
    seed_matrix = get_seed()
    seed_right_part = get_seed()
    matrix_base = create_matrix(seed_matrix, x)
    right_part = create_matrix(seed_right_part, x, 1)
    united_matrix = connect_matrix(matrix_base, right_part)
    sz = size(right_part)

    mt_easy = forward_easy(sz, united_matrix)
    answers_easy = back_substituion(sz, mt_easy)

    mt_choose = forward_choose(sz, united_matrix)
    answers_choose = back_substituion(sz, mt_choose)

    lu_matrix = divide_matrix(sz, matrix_base)
    vec_y = forward_lu(sz, connect_matrix(lu_matrix%mt_l, right_part))
    answers_lu = back_substituion(sz, connect_matrix(lu_matrix%mt_u, reshape(vec_y, [sz, 1])))

    ! Данные для замеров
    sz_measure = sz
    united_mt_measure = united_matrix
    mt_easy_measure = mt_easy
    mt_choose_measure = mt_choose
    mt_base_measure = matrix_base
    mt_l_measure = connect_matrix(lu_matrix%mt_l, right_part)
    mt_u_measure = connect_matrix(lu_matrix%mt_u, reshape(vec_y, [sz, 1]))

    ! Вывод данных
    print *, "Сиды для матриц:", seed_matrix, seed_right_part

    print *, "Соединенная матрица"
    do i = 1, x
        print *, united_matrix(i, :)
    end do
    
    print *, "Ответы"
    do i = 1, x
        print '(3x, I0, " : ", G0.5)', i, answers_easy(i)
        ! Если нужен вывод без e-N 
        ! print '(3x, I0, " : ", F20.15)', i, answers_choose(i)
    end do

    ! print *, "Матрица L"
    ! do i = 1, x
    !     print *, lu_matrix%mt_l(i, :)
    ! end do

    ! print *, "Матрица U"
    ! do i = 1, x
    !     print *, lu_matrix%mt_u(i, :)
    ! end do

    time_easy = time_count_fortran(wrap_gause_easy)
    time_choose = time_count_fortran(wrap_gause_choose)
    time_lu_decomposition = time_count_fortran(wrap_lu_decomposition)
    time_lu_solve = time_count_fortran(wrap_lu_solve)
    time_lu = time_count_fortran(wrap_lu)

    print *, "Легкий способ: ", time_easy
    print *, "Частичный выбор: ", time_choose
    print *, "Разложение LU: ", time_lu_decomposition
    print *, "Решение треугольной системы: ", time_lu_solve
    print *, "Решение LU: ",time_lu


end program main



