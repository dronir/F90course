module Distributions
    integer, parameter :: DIST_UNIFORM = 0
    integer, parameter :: DIST_NORMAL = 1
    integer, parameter :: DIST_CHISQ = 2
    
    type Distribution
        integer :: kind
        real :: a, b
    end type
    
contains
    ! Constructors for prettier generation of Distribution type objects
    ! Uniform constructor
    type(Distribution) function Uniform(a, b)
        real, intent(in) :: a,b
        Uniform%kind = DIST_UNIFORM
        Uniform%a = a
        Uniform%b = b
    end function
    
    ! Normal constructor
    type(Distribution) function Normal(mu, sigma)
        real, intent(in) :: mu,sigma
        Normal%kind = DIST_NORMAL
        Normal%a = mu
        Normal%b = sigma
    end function
    
    ! Chi-squared constructor
    type(Distribution) function ChiSq(k)
        integer, intent(in) :: k
        ChiSq%kind = DIST_CHISQ
        ChiSq%a = k
        ChiSq%b = 0.0
    end function
    
    
    ! Generic probability distribution function
    real function Pdf(dist, x)
        real, intent(in) :: x
        type(Distribution), intent(in) :: dist

        select case (dist%kind)
        case(0)
            P = pdfUnif(dist%a, dist%b)
        case(1)
            P = pdfNorm(x, dist%a, dist%b)
        case default
            P = 0.0 
        end select

    end function
    
    
    ! Probability distribution function for uniform distribution
    ! This would be simple to just inline in the P(dist,x) function,
    ! but written out for completeness.
    real function pdfUnif(a,b)
        real, intent(in) :: a, b
        pdfUnif = 1.0 / (b-a)
    end function
    
    ! Cumulative distribution function for uniform distribution
    real function cdfUnif(x,a,b)
        real, intent(in) :: x,a,b
        cdfUnif = min(max((x-a) / (b-a), 0.0), 1.0)
    end function
    
    ! Probability distribution function for normal distribution
    real function pdfNorm(x,a,b)
        real, intent(in) :: x,a,b
        pdfNorm = min(max((x-a) / (b-a), 0.0), 1.0)
    end function
    
    ! Cumulative distribution function for normal distribution
!    real 
    
end module
