module ModNormal
    use UtilityFunctions
    
contains

! Probability distribution function for normal distribution
real function pdfNorm(x,mu,sigma)
    real, intent(in) :: x,mu,sigma
    pdfNorm = 1.0 / (sqrt(2*pi) * sigma)
    pdfNorm = pdfNorm * exp(-(x-mu)**2 / (2 * sigma**2))
end function

! Cumulative distribution function for normal distribution
! by scaling to a standard normal and using the approximation
! defined below.
real function cdfNormal(x, mu, sigma)
    real, intent(in) :: x,mu,sigma
    real :: Z
    Z = (x - mu)/sigma
    cdfNormal = cdfStdNormal(Z)
end function

! Approximation of the CDF for a standard normal distribution
real function cdfStdNormal(x) result (P)
    real, intent(in) :: x
    real :: t
    t = 1.0 / (1 + 0.33267 * x)
    P = exp(-x**2 / (2 * sqrt(2*pi)))
    P = P * (0.4361836*t - 0.1201676*t**2 + 0.9372980*t**3)
    P = 1 - P
end function


end module
