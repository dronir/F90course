module ModChiSq
    use UtilityFunctions
    
contains
    
    ! pdf for chi-squared distribution
    real function pdfChiSq(x,k) result (P)
        real, intent(in) :: x,k
        real :: halfk
        halfk = k/2
        P = 1.0 / (2**halfk * gamma(halfk))
        P = P * x**(halfk - 1) * exp(-x/2)
    end function
    
end module
