module ModUniform
contains 
    ! Probability distribution function for uniform distribution
    ! This would be simple to just inline in the P(dist,x) function,
    ! but written out for completeness.
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
end module
