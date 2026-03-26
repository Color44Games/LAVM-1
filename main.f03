program main
    use rand_generation
    use gause_method
    use matrix_processing
    use lu_decomposition
    implicit none
    real, allocatable :: matrix_base(:, :), right_part(:, :), united_matrix(:, :)
    real, allocatable :: mt_easy(:, :), answers_easy(:), mt_choose(:, :), answers_choose(:)
    real, allocatable :: vec_y(:), answers_lu(:)
    integer :: x, i, seed_matrix, seed_right_part, sz
    type(matrix_pair) :: lu_matrix

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

    lu_matrix = divide_matrix(sz, matrix_base)
    vec_y = forward_substitution(sz, connect_matrix(lu_matrix%mt_l, right_part))
    answers_lu = back_substituion(sz, connect_matrix(lu_matrix%mt_u, reshape(vec_y, [sz, 1])))

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
        print '(3x, I0, " : ", G0.5)', i, answers_easy(i)
    end do

    print *, "Ответы (Частичный выбор по стобцу)"
    do i = 1, x
        print '(3x, I0, " : ", G0.5)', i, answers_choose(i)
        !Если нужен вывод без e-N 
        !print '(3x, I0, " : ", F20.15)', i, answers_choose(i)
    end do

    ! print *, "Матрица L"
    ! do i = 1, x
    !     print *, lu_matrix%mt_l(i, :)
    ! end do

    ! print *, "Матрица U"
    ! do i = 1, x
    !     print *, lu_matrix%mt_u(i, :)
    ! end do

    print *, "Ответы (LU)"
    do i = 1, x
        print '(3x, I0, " : ", G0.5)', i, answers_lu(i)
    end do


end program main



