module matrix_processing
    implicit none
    private
    public connect_matrix
contains

    ! Соединяем 2 матрицы в одну
    function connect_matrix(matrix_a, matrix_rp) result(matrix)
        real, intent(in) :: matrix_a(:, :), matrix_rp(:, :)
        real, allocatable :: matrix(:, :)
        integer :: size_matrix

        size_matrix = size(matrix_rp)
        allocate(matrix(size_matrix, size_matrix + 1))

        matrix(:, 1:size_matrix) = matrix_a
        matrix(:, size_matrix + 1) = matrix_rp(1:size_matrix, 1)
    end function connect_matrix
end module matrix_processing
