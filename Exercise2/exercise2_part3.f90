! Secand method for solving x - ln(x) = 2
! This method diverges for some starting values, for example, a=0.01, b=1.0
program Exercise2_2
    real a, b, result

    write(*,*) "Give start of first secant"
    read(*,*) a
    write(*,*) "Give end of first secant"
    read(*,*) b
    write(*,*)
    
    result = solve(a, b)
    
    write(*,*) "Final results:"
    write(*,*) "  x  =", result
    write(*,*) "f(x) =", f(result)

    
contains
    ! Solver function
    real function solve(x0, x1)
        real x0, x1, eps
        real y0, y1, xm
        integer i
        eps = 1e-7
        do i = 1,1000
            y0 = f(x0)
            y1 = f(x1)
            xm = x1 - y1 * (x0 - x1) / (y0 - y1)
            x0 = x1
            x1 = xm
            if (abs(x0-x1) < eps) exit
        end do
        if (i<1000) then
            write(*,'(A,E8.2,A,I3)') "Convergence within ", eps, " at step ", i
        else
            write(*,*) "No convergence in 1000 steps, solution probably diverged:", x1
        end if
        solve = x1
    end function
    
    ! Function to be solved
    real function f(x)
        real x
        f = x - log(x) - 2.0
    end function
end program