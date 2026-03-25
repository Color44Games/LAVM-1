module rand_generation
    implicit none
    private
    public :: get_seed, create_matrix, low_number, high_number

    integer :: low_seed = 10 ** 5
    integer :: high_seed = 10 ** 7
    real :: low_number = 1.0
    real :: high_number = 10.0 ** 3
contains

    ! Создание сида для матрциы
    function get_seed() result(seed)
        integer :: seed
        real :: rand_value

        call random_number(rand_value)
        seed = low_seed + floor((real(high_seed - low_seed + 1)) * rand_value)
    end function get_seed

    ! Генерация матрицы по сиду
    function create_matrix(seed_matrix, x, y) result(matrix)
        integer, intent(in) :: x, seed_matrix
        integer, intent(in), optional :: y
        integer, allocatable :: seed_arr(:)
        real, allocatable :: matrix(:, :)
        integer :: i, length_array
        
        call random_seed(size = length_array)
        allocate(seed_arr(length_array))

        if (present(y)) then
            allocate(matrix(x, y))
        else
            allocate(matrix(x, x))
        endif

        do i = 1, length_array
            seed_arr(i) = abs(ieor(seed_matrix, 123456789) * 265443 + i * 987654321)
        end do
        
        call random_seed(put = seed_arr)
        call random_number(matrix)
        matrix = low_number + real(high_number - low_number + 1.0) * matrix
    end function create_matrix
end module rand_generation