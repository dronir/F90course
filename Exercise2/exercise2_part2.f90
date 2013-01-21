! Interval halving method for solving x - ln(x) = 2
program Exercise2_2
    real a, b, result

    write(*,*) "Give start of first interval"
    read(*,*) a
    write(*,*) "Give end of first interval"
    read(*,*) b
    
    result = solve(a, b)
    
    
    write(*,*) "Final results:"
    write(*,*) "x =", result
    write(*,*) "y =", f(result)

    stop
    
contains
    real function solve(a, b)
        real a, b, eps
        real y1, y2, xm, ym
        eps = 1e-7
        do
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
        solve = a
    end function
    
    real function f(x)
        real x
        f = x - log(x) - 2.0
    end function
end program