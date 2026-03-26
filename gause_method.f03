module gause_method
    implicit none
    private
    public forward_easy, forward_choose, back_substituion

    contains

        ! Обнуляем элементы на главной диагонали (простой способ)
        function forward_easy(size_matrix, matrix) result(mt)
            integer, intent(in) :: size_matrix
            real, intent(in) :: matrix(:, :)    
            integer :: i, j
            real :: mt(size_matrix, size_matrix + 1), factor
        
            mt = matrix

            do j = 1, size_matrix - 1
                do i = j + 1, size_matrix
                    factor = mt(i, j) / mt(j, j)
                    mt(i, j + 1:) = mt(i, j + 1:) - (factor * mt(j, j + 1:))
                    mt(i, j) = 0.0
                end do
            end do
        end function forward_easy

        function forward_choose(size_matrix, matrix) result(mt)
            integer, intent(in) :: size_matrix
            real, intent(in) :: matrix(:, :)
            integer :: i, j, pivot(1), pivot_ind
            real :: mt(size_matrix, size_matrix + 1), factor
            real, allocatable :: temp_row(:)

            mt = matrix

            do j = 1, size_matrix - 1
                pivot = maxloc(abs(mt(j:size_matrix, j)))
                pivot_ind= pivot(1) + j - 1

                if(pivot_ind /= j) then
                    allocate(temp_row(size_matrix + 2 - j))
                    temp_row = mt(j, j:size_matrix + 1)
                    mt(j, j:size_matrix + 1) = mt(pivot_ind, j:size_matrix + 1)
                    mt(pivot_ind, j:size_matrix + 1) = temp_row
                    deallocate(temp_row)
                end if

                do i = j + 1, size_matrix
                    factor = mt(i, j) / mt(j, j)
                    mt(i, j + 1:) = mt(i, j + 1:) - (factor * mt(j, j + 1:))
                    mt(i, j) = 0.0
                end do
            end do
            
        end function forward_choose

        ! Находим значения переменных
        function back_substituion(size_matrix, mt) result(answers)
            integer, intent(in) :: size_matrix
            real, intent(in) :: mt(:, :)
            integer :: i, j
            real :: answers(size_matrix), summ
            
            answers(size_matrix) = mt(size_matrix, size_matrix + 1) / mt(size_matrix, size_matrix)

            do j = size_matrix - 1, 1, -1
                summ = 0.0
                do i = size_matrix, j + 1, -1
                    summ = summ + mt(j, i) * answers(i)
                end do 
                answers(j) = (mt(j, size_matrix + 1) - summ) / mt(j, j)
            end do
        end function back_substituion
end module gause_method