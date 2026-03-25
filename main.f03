program main
    use rand_generation
    use gause_method
    use matrix_processing
    implicit none
    real, allocatable :: matrix_base(:, :), right_part(:, :), united_matrix(:, :)
    real, allocatable :: mt(:, :), answers_easy(:)
    integer :: x, i, seed_matrix, seed_right_part, sz

    call execute_command_line("chcp 65001 > nul")
    call random_seed()
    read *, x

    seed_matrix = get_seed()
    seed_right_part = get_seed()
    matrix_base = create_matrix(seed_matrix, x)
    right_part = create_matrix(seed_right_part, x, 1)
    united_matrix = connect_matrix(matrix_base, right_part)
    sz = size(right_part)
    
    mt = forward_easy(sz, united_matrix)
    answers_easy = back_substituion(sz, mt)

    ! Вывод данных
    print *, "Сиды для матриц:", seed_matrix, seed_right_part
    
    print *, "Соединенная матрица"
    do i = 1, x
        print *, united_matrix(i, :)
    end do
    
    print *, "Готовая мартица"
    do i = 1, x
        print *, mt(i, :)
    end do
    
    print *, "Ответы"
    do i = 1, x
        print *, i, ':', answers_easy(i)
    end do

end program main



