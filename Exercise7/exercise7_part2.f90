program Exercise8_part2
    integer :: n
    real :: S
    real, parameter :: pi = 3.141592653
    real, external :: f
    
    do n = 0,10
        S = trapezoid(f, 0.0, pi/2, 2**n)
        write(*,*) 2**n, S
    end do 

contains
    real function trapezoid(f,a,b,N)
        interface
            real function f(x)
                real, intent(in) :: x
            end function
        end interface
        real, intent(in) :: a,b
        integer, intent(in) :: N
        integer :: i
        real :: x0, y0, x1, y1, delta
        
        trapezoid = 0.0
        delta = (b-a) / N
        
        x0 = a
        y0 = f(a)
        do i = 1,N
            x1 = x0 + delta
            y1 = f(x1)
            trapezoid = trapezoid + delta*(y0+y1)/2
            x0 = x1
            y0 = y1
        end do
    end function
    
end program
