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
    right_part = create_matrix(seed_right_part, 1, x)
    united_matrix = connect_matrix(matrix_base, right_part)
    sz = size(right_part)
    
    ! Вывод данных
    print *, "Сиды для матриц:", seed_matrix, seed_right_part

    ! print *, "Исходная матрица A:"
    ! do i = 1, x
    !     print *, matrix_base(:, i)
    ! end do

    ! print *, "Правая часть:"
    ! do i = 1, x
    !     print *, right_part(:, i)
    ! end do

    print *, "Соединенная матрица"
    do i = 1, x
        print *, united_matrix(:, i)
    end do

    mt = forward_easy(sz, united_matrix)
    print *, "Готовая мартица"
    do i = 1, x
        print *, mt(:, i)
    end do
    answers_easy = back_substituion(sz, mt)
    print *, "Ы"
    do i = 1, x
        print *, answers_easy(i)
    end do

end program main



