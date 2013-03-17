module Distributions
    use UtilityFunctions
    use ModUniform
    use ModNormal
    use ModChiSq
    
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
        case(DIST_UNIFORM)
            Pdf = pdfUnif(x, dist%a, dist%b)
        case(DIST_NORMAL)
            Pdf = pdfNormal(x, dist%a, dist%b)
        case(DIST_CHISQ)
            Pdf = pdfChiSq(x, dist%a)
        case default
            Pdf = 0.0
        end select
    end function
    
    ! Generic cumulative distribution function
    real function Cdf(dist, x)
        real, intent(in) :: x
        type(Distribution), intent(in) :: dist
        select case (dist%kind)
        case(DIST_UNIFORM)
            Cdf = cdfUnif(x, dist%a, dist%b)
        case(DIST_NORMAL)
            Cdf = cdfNormal(x, dist%a, dist%b)
        case(DIST_CHISQ)
            Cdf = cdfChiSq(x, dist%a)
        case default
            Cdf = 0.0
        end select
    end function
    
    ! Generic mean
    real function mean(dist)
        type(Distribution), intent(in) :: dist
        select case (dist%kind)
        case(DIST_UNIFORM)
            mean = meanUnif(dist%a,dist%b)
        case(DIST_NORMAL)
            mean = dist%a
        case(DIST_CHISQ)
            mean = dist%a
        case default
            mean = 0.0
        end select
    end function
    
    ! Generic std
    real function std(dist)
        type(Distribution), intent(in) :: dist
        select case (dist%kind)
        case(DIST_UNIFORM)
            std = stdUnif(dist%a,dist%b)
        case(DIST_NORMAL)
            std = dist%b
        case(DIST_CHISQ)
            std = sqrt(2*dist%a)
        case default
            std = 0.0
        end select
    end function
    
    ! Generic median
    real function median(dist)
        type(Distribution), intent(in) :: dist
        select case (dist%kind)
        case(DIST_UNIFORM)
            median = meanUnif(dist%a,dist%b)
        case(DIST_NORMAL)
            median = dist%a
        case(DIST_CHISQ)
            median = medianChiSq(dist%a)
        case default
            median = 0.0
        end select
    end function
    



    
end module
