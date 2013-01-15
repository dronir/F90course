program exercise1_3
    ! Solving exp(x)-x^2 = 2 via iteration
    implicit none
    double precision delta, epsilon
    double precision x0, x1
    
    x0 = 1.0 ! Initial guess
    epsilon = 1e-12 ! required precision
    delta = 1 ! has to be >epsilon at first
    
    ! Iterate until desired precision
    do while (delta > epsilon)
        x1 = log(2 + x0**2)
        delta = abs(x0-x1)
        x0 = x1
    end do
    
    write(*,*) "Found solution:", x0
    write(*,*) "Value at solution (should be zero):", exp(x0)-x0**2-2
end program
