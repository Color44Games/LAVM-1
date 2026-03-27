module utilities
    use iso_c_binding
    implicit none
    private
    public time_count_fortran, time_count_c, discrepancy

    interface 
        function c_clock() bind(c, name="clock")
            import :: c_long
            integer(c_long) :: c_clock
        end function c_clock
    end interface

    abstract interface
        subroutine generic_proc()
        end subroutine generic_proc
    end interface
contains
    
    ! Подсчет времени работы функции через фортрановский таймер
    function time_count_fortran(proc) result(time_f)
        procedure(generic_proc) :: proc 
        integer(8) :: start_f, end_f, rate_f 
        real(8) :: time_f
        
        call system_clock(count_rate= rate_f)
        call system_clock(start_f)
        call proc()
        call system_clock(end_f)

        time_f = real(end_f - start_f, 8) / rate_f
    end function time_count_fortran

    ! Подсчет времени через clock() из С
    function time_count_c(proc) result(time_c)
        procedure(generic_proc) :: proc
        real(8), parameter :: clock_per_sec = 1000.0_8
        real(8) :: time_c
        integer(c_long) :: start_c, end_c

        start_c = c_clock()
        call proc()
        end_c = c_clock()

        time_c = real(end_c - start_c, 8) / clock_per_sec
    end function time_count_c

    ! Подсчет невязки
    function discrepancy(matrix_a, vec_x, right_part) result(discrepancy_rel)
        real, intent(in) :: matrix_a(:, :), vec_x(:), right_part(:)
        real :: discrepancy_rel, residual(size(right_part))

        residual = matmul(matrix_a, vec_x) - right_part
        discrepancy_rel = norm2(residual) / norm2(right_part)
    end function discrepancy
end module utilities