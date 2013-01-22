! Interval halving method for solving x - ln(x) = 2
program Exercise2_2
    real a, b, result

    write(*,*) "Give start of first interval"
    read(*,*) a
    write(*,*) "Give end of first interval"
    read(*,*) b
    write(*,*)
    
    result = solve(a, b)
    
    write(*,*) "Final results:"
    write(*,*) "  x  =", result
    write(*,*) "f(x) =", f(result)

    
contains
    ! Solver function
    real function solve(a, b)
        real a, b, eps
        real y1, y2, xm, ym
        integer i
        eps = 1e-7
        i = 0
        do
            i = i + 1
            y1 = f(a)
            y2 = f(b)
            xm = (a+b)/2
            ym = f(xm)
            if (y1*ym < 0.0) then
                b = xm
            else
                a = xm
            end if
            if (abs(a-b) < eps) exit
        end do
        write(*,'(A,E8.2,A,I3)') "Convergence within ", eps, " at step ", i
        solve = a
    end function
    
    ! Function to be solved
    real function f(x)
        real x
        f = x - log(x) - 2.0
    end function
end program