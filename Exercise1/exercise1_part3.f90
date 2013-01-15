program exercise1_3
    ! Solving exp(x)-x^2 = 2 via iteration
    implicit none
    double precision delta, epsilon
    double precision x0, x1
    integer i
    
    epsilon = 1e-12 ! required precision
    
    write(*,*) "Using x = log(2 + x^2) form"
    ! Iterate until desired precision
    x0 = 1.0 ! Initial guess
    delta = 1 ! has to be >epsilon at first
    i = 0
    do while (delta > epsilon)
        x1 = log(2 + x0**2)
        delta = abs(x0-x1)
        x0 = x1
        i = i + 1
    end do
    
    write(*,*) "Found solution:", x0
    write(*,*) "Value at solution (should be zero):", exp(x0)-x0**2-2
    write(*,*) "Number of iterations:", i

    ! The second method does not converge properly,
    ! because x1 = sqrt(exp(x0) - 2) easily produces a value which makes
    ! exp(x1) < 2, and therefore the sqrt produces NaN
    write(*,*)
    write(*,*) "Using x = sqrt(exp(x) - 2) form"
    ! Iterate until desired precision
    x0 = 2.0 ! Initial guess
    delta = 1 ! has to be >epsilon at first
    i = 0
    do while (delta > epsilon)
        x1 = sqrt(exp(x0) - 2)
        delta = abs(x0-x1)
        x0 = x1
        i = i + 1
    end do
    
    write(*,*) "Found solution:", x0
    write(*,*) "Value at solution (should be zero):", exp(x0)-x0**2-2
    write(*,*) "Number of iterations:", i
end program
