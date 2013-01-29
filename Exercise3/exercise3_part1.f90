! Exercise 3 part 1: approximating sin(x)
program Exercise3_part1
    integer, parameter :: longreal=selected_real_kind(16)
    real(longreal) :: x, result
    integer :: N
    write(*,*) "Give x and N:"
    read(*,*) x, N
    write(*,*) "-------------------"
    result = sin_series(x, N)
    write(*,*) "-------------------"
    write(*,*) "result:     ", result
    write(*,*) "true sin(x):", sin(x)

contains
    real(longreal) function sin_series(x, N)
        integer :: i, k, N, limit
        real(longreal) :: x, S, term
        S = 0.0
        
        limit = 2*N
        write(*,*) "          k       sum                       sum/sin(x)"
        
        do i = 1,limit
            k = 2*i - 1
            term = x_factorial(x, k)
            if (term < 1e-16) exit
            S = S + term * (-1)**(i+1)
            write(*,*) k, S, S/sin(x)
        end do
        sin_series = S
    end function
    
    ! Function that computes x^n / n! with better precision    
    real(longreal) function x_factorial(x, n)
        integer :: n, i
        real(longreal) :: x
        x_factorial = 1.0
        do i = 1,n
            x_factorial = x_factorial * x / i
        end do
    end function

end program