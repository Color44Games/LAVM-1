module gause_method
    implicit none
    private
    public forward_easy, back_substituion

    contains

        ! Обнуляем элементы на главной диагонали
        function forward_easy(size_matrix, matrix) result(mt)
            integer, intent(in) :: size_matrix
            real, intent(in) :: matrix(:, :)    
            integer :: i, j
            real, allocatable :: factor, mt(:, :)
        
            allocate(mt(size_matrix + 1, size_matrix))
            mt = matrix

            do i = 1, size_matrix - 1
                do j = i + 1, size_matrix
                    factor = mt(i, j) / mt(i, i)
                    mt(:, j) = mt(:, j) - (factor * mt(:, i))
                end do
            end do
        end function forward_easy

        ! Находим значения переменных
        function back_substituion(size_matrix, mt) result(answers)
            integer, intent(in) :: size_matrix
            real, intent(in) :: mt(:, :)
            integer :: i, j
            real, allocatable :: answers (:)

            allocate(answers(size_matrix))
            answers(size_matrix) = mt(size_matrix + 1, size_matrix) / mt(size_matrix, size_matrix)

            do i = size_matrix - 1, 1
                answers(i) = mt(size_matrix, i)
                do j = i + 1, size_matrix
                    answers(i) = answers(i) / mt(i, j)
                end do 
            end do
            

        end function back_substituion
end module gause_method