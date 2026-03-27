module wrap_function
    use gause_method
    use lu_decomposition
    implicit none

    ! Данные для измерения времени
    integer :: sz_measure
    real, allocatable :: united_mt_measure(:, :), mt_easy_measure(:, :), mt_choose_measure(:, :)
    real, allocatable :: mt_base_measure(:, :), mt_l_measure(:, :), mt_u_measure(:, :)

    ! Переменные с результатами работы функций
    real, allocatable :: res_matrix(:, :), res_arr(:)
    type(matrix_pair) :: res_mt_pair
contains

    ! Обертка для легкого способа метода Гаусса
    subroutine wrap_gause_easy()
        res_matrix = forward_easy(sz_measure, united_mt_measure)
        res_arr = back_substituion(sz_measure, mt_easy_measure)
    end subroutine wrap_gause_easy

    ! Обертка для способа Гаусса с частичным выбором
    subroutine wrap_gause_choose()
        res_matrix = forward_choose(sz_measure, united_mt_measure)
        res_arr = back_substituion(sz_measure, mt_choose_measure)
    end subroutine wrap_gause_choose

    ! Обертка для LU-разложения
    subroutine wrap_lu_decomposition()
        res_mt_pair = divide_matrix(sz_measure, mt_base_measure)
    end subroutine wrap_lu_decomposition

    ! Обертка для решения треугольных систем
    subroutine wrap_lu_solve()
        res_arr = forward_lu(sz_measure, mt_l_measure)
        res_arr = back_substituion(sz_measure, mt_u_measure)
    end subroutine wrap_lu_solve

    ! Обертка для решения LU методом
    subroutine wrap_lu()
        res_mt_pair = divide_matrix(sz_measure, mt_base_measure)
        res_arr = forward_lu(sz_measure, mt_l_measure)
        res_arr = back_substituion(sz_measure, mt_u_measure)
    end subroutine wrap_lu
end module wrap_function