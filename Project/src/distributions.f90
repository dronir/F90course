module Distributions
    integer, parameter :: DIST_UNIFORM = 0
    integer, parameter :: DIST_NORM = 1
    integer, parameter :: DIST_CHISQ = 2
    
    type Distribution
        integer :: kind
        real :: a, b
    end type
    
contains
    ! Generic probability distribution function
    real function P(dist, x)
        real, intent(in) :: x
        type(Distribution), intent(in) :: dist

        select case (dist%kind)
        case(0)
            P = pUnif(x, dist%a, dist%b)
        case default
            P = 0.0 
        end select

    end function
    
    
    ! Probability distribution function for uniform distribution
    real function pUnif(x,a,b)
        real, intent(in) :: x,a, b
        pUnif = 1.0 / (b-a)
    end function
    
    ! Cumulative distribution function for uniform distribution
    real function FUnif(x,a,b)
        real, intent(in) :: x,a,b
        FUnif = min(max((x-a) / (b-a), 0.0), 1.0)
    end function
    
end module
