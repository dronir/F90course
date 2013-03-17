module ModChiSq
    use UtilityFunctions
    
contains
    
    ! pdf for chi-squared distribution
    real function pdfChiSq(x,k) result (P)
        real, intent(in) :: x,k
        real :: halfk
        if (x<0) then
            P = 0.0
        else
            halfk = k/2
            P = 1.0 / (2**halfk * gamma(halfk))
            P = P * x**(halfk - 1) * exp(-x/2)
        end if
    end function

    ! cdf for chi-squared distribution
    real function cdfChiSq(x,k) result (P)
        real, intent(in) :: x,k
        real :: halfk
        if (x<0) then
            P = 0.0
        else
            halfk = k/2
            P = 1.0 / (gamma(halfk)) * lgamma(halfk, x/2)
        end if
    end function
    
    ! approximation for ChiSq median
    real function medianChiSq(k) result (y)
        real, intent(in) :: k
        y = k * (1 - 2 / (9*k))**3
    end function
    
    
end module
