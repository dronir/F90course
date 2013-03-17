module ModUniform
contains

! Probability distribution function for uniform distribution
real function pdfUnif(x,a,b)
    real, intent(in) :: x, a, b
    if (x>=a .and. x<=b) then
        pdfUnif = 1.0 / (b-a)
    else
        pdfUnif = 0.0
    end if
end function
    
! Cumulative distribution function for uniform distribution
real function cdfUnif(x,a,b)
    real, intent(in) :: x,a,b
    cdfUnif = min(max((x-a) / (b-a), 0.0), 1.0)
end function
    
real function meanUnif(a,b) result (y)
    real, intent(in) :: a,b
    y = (a+b)/2.0
end function

real function medianUnif(a,b) result (y)
    real, intent(in) :: a,b
    y = (a+b)/2.0
end function

real function stdUnif(a,b) result (y)
    real, intent(in) :: a,b
    y = (b-a) / sqrt(12.0)
end function

    
end module
