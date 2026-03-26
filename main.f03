program main
    use rand_generation
    use gause_method
    use matrix_processing
    implicit none
    real, allocatable :: matrix_base(:, :), right_part(:, :), united_matrix(:, :)
    real, allocatable :: mt_easy(:, :), answers_easy(:), mt_choose(:, :), answers_choose(:)
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
    mt_easy = forward_easy(sz, united_matrix)
    answers_easy = back_substituion(sz, mt_easy)
    mt_choose = forward_choose(sz, united_matrix)
    answers_choose = back_substituion(sz, mt_choose)

    ! Вывод данных
    print *, "Сиды для матриц:", seed_matrix, seed_right_part

    print *, "Соединенная матрица"
    do i = 1, x
        print *, united_matrix(i, :)
    end do
    
    ! print *, "Готовая мартица 1"
    ! do i = 1, x
    !     print *, mt_easy(i, :)
    ! end do

    ! print *, "Готовая мартица 2"
    ! do i = 1, x
    !     print *, mt_choose(i, :)
    ! end do
    
    print *, "Ответы (Легкий способ)"
    do i = 1, x
        print '(3x, I0, " : ", G0.8)', i, answers_easy(i)
    end do

    print *, "Ответы (Частичный выбор по стобцу)"
    do i = 1, x
        print '(3x, I0, " : ", G0.8)', i, answers_choose(i)
        !Если нужен вывод без e-N 
        !print '(3x, I0, " : ", F20.15)', i, answers_choose(i)
    end do

end program main



