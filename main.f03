program main
    use rand_generation
    use gause_method
    use matrix_processing
    use lu_decomposition
    use utilities
    use wrap_function
    implicit none
    real, allocatable :: matrix_base(:, :), right_part(:, :), united_matrix(:, :)
    real, allocatable :: vec_y(:), answers_lu(:), mt_easy(:, :), answers_easy(:)
    real(8) :: time_easy, time_choose, time_lu_decomposition, time_lu_solve, time_lu
    real :: ds
    integer :: x, sz, i, seed_matrix, seed_right_part, command, time_cmd
    type(matrix_pair) :: lu_matrix

    command = -1
    time_cmd = -1
    seed_matrix = 0
    call execute_command_line("chcp 65001 > nul")
    call random_seed()

    ! Интерфейс для работы с программой
    print '(A, $)', "Введите размер матрицы: "
    read *, sz

    do while (command /= 0)
        print *, "=== Возможные команды ==="
        print *, "1) Сгененировать сид"
        print *, "2) Задать сид"
        print *, "3) Сгенерировать матрицу"
        print *, "4) Вывести LU-разложение"
        print *, "5) Замерить время"
        print *, "6) Выбрать диапазон чисел в матрице"
        print *, "0) Выход"
        print '(A, $)', "Выберите команду: "
        
        read *, command

        select case (command)
        case (1)
            seed_matrix = get_seed()
            print *, "Ваш сид: ", seed_matrix
        case (2)
            print '(A, $)', "Введите сид: "
            read *, seed_matrix
        case (3)
            matrix_base = create_matrix(seed_matrix, sz)
            lu_matrix = divide_matrix(sz, matrix_base)

            print *, "Ваша матрица"
            do i = 1, sz
                print *, matrix_base(i, :)
            end do
        case (4)
            print *, "Матрица L"
            do i = 1, sz
                print *, lu_matrix%mt_l(i, :)
            end do

            print *, "Матрица U"
            do i = 1, sz
                print *, lu_matrix%mt_u(i, :)
            end do
        case (5)
            print *, "=== Возможные методы для замера ==="
            print *, "1) Простой метод Гаусса"
            print *, "2) Метод Гаусса с частичным выбором"
            print *, "3) LU-разложение"
            print *, "4) Решение треугольной системы"
            print *, "5) Полное LU-решение"
            print '(A, $)', "Выберите команду: "
            read *, time_cmd

            ! Количество тестов для замера времени
            select case (time_cmd)
            case (3)
            case default
                print '(A, $)', "Количество тестов для замера: "
                read *, x
            end select

            sz_measure = sz
            mt_base_measure = matrix_base
            time_easy = 0.0_8
            time_choose = 0.0_8
            time_lu_decomposition = 0.0_8
            time_lu_solve = 0.0_8
            time_lu = 0.0_8

            select case (time_cmd)
            case (1)
                do i = 1, x
                    seed_right_part = get_seed()
                    right_part = create_matrix(seed_matrix, sz, 1)

                    united_mt_measure = connect_matrix(matrix_base, right_part)
                    mt_easy_measure = forward_easy(sz, united_matrix)

                    time_easy = time_easy + time_count_fortran(wrap_gause_easy)
                end do
                print *, "Время (Легкий способ): ", time_easy
            case (2)
                do i = 1, x
                    seed_right_part = get_seed()
                    right_part = create_matrix(seed_matrix, sz, 1)

                    united_mt_measure = connect_matrix(matrix_base, right_part)
                    mt_choose_measure = forward_choose(sz, united_matrix)

                    time_choose = time_choose + time_count_fortran(wrap_gause_choose)
                end do
                print *, "Время (Частичный выбор): ", time_choose
            case (3)
                time_lu_decomposition = time_lu_decomposition + time_count_fortran(wrap_lu_decomposition)
                print *, "Время (LU-Разложение): ", time_lu_decomposition
            case (4)
                do i = 1, x
                    seed_right_part = get_seed()
                    right_part = create_matrix(seed_right_part, sz, 1)

                    vec_y = forward_lu(sz, connect_matrix(lu_matrix%mt_l, right_part))
                    mt_l_measure = connect_matrix(lu_matrix%mt_l, right_part)
                    mt_u_measure = connect_matrix(lu_matrix%mt_u, reshape(vec_y, [sz, 1]))

                    time_lu_solve = time_lu_solve + time_count_fortran(wrap_lu_solve)
                end do
                print *, "Время (Решение треугольной системы): ", time_lu_solve
            case (5)
                time_lu = time_count_fortran(wrap_lu_decomposition)
                do i = 1, x
                    seed_right_part = get_seed()
                    right_part = create_matrix(seed_right_part, sz, 1)

                    vec_y = forward_lu(sz, connect_matrix(lu_matrix%mt_l, right_part))
                    mt_l_measure = connect_matrix(lu_matrix%mt_l, right_part)
                    mt_u_measure = connect_matrix(lu_matrix%mt_u, reshape(vec_y, [sz, 1]))

                    time_lu = time_lu + time_count_fortran(wrap_lu_solve)

                    answers_lu = back_substituion(sz, connect_matrix(lu_matrix%mt_u, reshape(vec_y, [sz, 1])))
                    ds = discrepancy(matrix_base, answers_lu, right_part(:, 1))
                end do
                print *, "Время (Решение LU): ", time_lu
                print *, "Невязка: ", ds
            case default
                print *, "Введенная команда не найдена"
            end select
        case (6)
            print *, "Нынешний диапазон: ", low_number, " - ", high_number

            print '(A, $)', "Введите минимальное число: "
            read *, low_number
            low_number = real(low_number, 4)

            print '(A, $)', "Введите максимальное число: "
            read *, high_number
            high_number = real(high_number, 4)
        case (0)
            print *, "Выход из программы"
        case default
            print *, "Введенная команда не найдена"
        end select
    end do
end program main
