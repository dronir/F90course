! Newton's method for solving x - ln(x) = 2
program Exercise2_2
    real a, result

    write(*,*) "Give starting value"
    read(*,*) a
    write(*,*)
    
    result = solve(a)
    
    write(*,*) "Final results:"
    write(*,*) "  x  =", result
    write(*,*) "f(x) =", f(result)

    
contains
    ! Solver function
    real function solve(x0)
        real x0, xm, eps
        integer i
        eps = 1e-7
        do i = 1,1000
            xm = x0 - f(x0) / df(x0)
            if (abs(x0-xm) < eps) exit
            x0 = xm
        end do
        if (i<1000) then
            write(*,'(A,E8.2,A,I3)') "Convergence within ", eps, " at step ", i
        else
            write(*,*) "No convergence in 1000 steps, solution probably diverged:", x0
        end if
        solve = xm
    end function
    
    ! Function to be solved
    real function f(x)
        real x
        f = x - log(x) - 2.0
    end function
    
    ! Derivative of f(x)
    real function df(x)
        real x
        df = 1 - 1.0/x
    end function
end program