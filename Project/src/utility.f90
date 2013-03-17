module UtilityFunctions
    real, parameter :: pi = 3.14159265358979

    ! g value and coefficients for Lancoz approximation
    integer, parameter :: g = 7
    real, parameter, dimension(9) :: coefs = (/ 0.99999999999980993, &
    676.5203681218851, -1259.1392167224028, 771.32342877765313, &
    -176.61502916214059, 12.507343278686905, -0.13857109526572012, &
    9.9843695780195716e-6, 1.5056327351493116e-7 /)
    
contains
    ! Gamma function from the Lancoz approximation
    recursive function gamma(z) result (gam)
        real :: gam
        real, intent(in) :: z
        real :: z0, x, t
        integer :: i
        
        if (z < 0.5) then
            gam = pi / (sin(pi*z) / gamma(1-z))
        else
            z0 = z - 1
            x = coefs(1)
            do i = 2,g+2
                x = x + coefs(i)/(z0 + i - 1)
            end do
            t = z0 + g + 0.5
            gam = sqrt(2*pi) * t**(z0+0.5) * exp(-t) * x
        end if
    end function
    
    ! Lower incomplete gamma function, series expression
    real function lgamma(s, z) result (res)
        real, intent(in) :: s, z
        real :: term
        integer :: k
        res = 0.0
        do k = 0,30
            term = (-1.0)**k / fact(k)
            term = term * z**(s + k) / (s + k)
            res = res + term
        end do
    end function
    
    ! Factorial function
    integer function fact(n)
        integer :: i, n
        fact = 1
        do i = n,1,-1
            fact = fact * i
        end do
    end function
    
end module
