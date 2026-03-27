module lu_decomposition
    implicit none
    private
    public matrix_pair, divide_matrix, forward_lu

    type :: matrix_pair
        real, allocatable :: mt_l(:, :)
        real, allocatable :: mt_u(:, :)
    end type matrix_pair
contains

    ! Раскладываем исходнюу матрицу на L и U -матрицы
    function divide_matrix(size_matrix, matrix) result(mt)
        integer, intent(in) :: size_matrix
        real, intent(in) :: matrix(:, :)
        integer :: i, j, k
        real :: summ
        type(matrix_pair) :: mt

        allocate(mt%mt_l(size_matrix, size_matrix), mt%mt_u(size_matrix, size_matrix))
        mt%mt_l = 0
        mt%mt_u = 0

        do i = 1, size_matrix
            mt%mt_l(i, i) = 1
        end do

        do j = 1, size_matrix
            do i = 1, size_matrix
                summ = 0.0
                if (i <= j) then
                    do k = 1, i - 1
                        summ = summ + mt%mt_l(i, k) * mt%mt_u(k, j)
                    end do
                    mt%mt_u(i, j) = matrix(i, j) - summ
                else 
                    do k = 1, j - 1
                        summ = summ + mt%mt_l(i, k) * mt%mt_u(k, j)
                    end do
                    mt%mt_l(i, j) = (matrix(i, j) - summ) / mt%mt_u(j, j)
                end if
            end do
        end do
    end function divide_matrix

    ! Вычисляем вектор y для уравнения Ly = b
    function forward_lu(size_matrix, mt_l) result(vec_y)
        integer, intent(in) :: size_matrix
        real :: mt_l(:, :)
        integer :: i, j
        real :: vec_y(size_matrix)

        vec_y = mt_l(:, size_matrix + 1)
        vec_y(1) = mt_l(1, size_matrix + 1)

        do j = 1, size_matrix - 1
            do i = j + 1, size_matrix
                vec_y(i) = vec_y(i) - mt_l(i, j) * vec_y(j)
                mt_l(i, size_matrix + 1) = vec_y(i)
            end do
        end do
    end function forward_lu
end module lu_decomposition