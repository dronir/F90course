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
            mean = (dist%a + dist%b)/2.0
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
            std = (dist%b - dist%a) / 3.464101615
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
            median = (dist%a + dist%b)/2.0
        case(DIST_NORMAL)
            median = dist%a
        case(DIST_CHISQ)
            median = medianChiSq(dist%a)
        case default
            median = 0.0
        end select
    end function
    
    ! Find cdf critical p-value, i.e. the value of x
    ! for which Cdf(x) = xCrit
    real function xCrit(dist, pvalue)
        type(Distribution), intent(in) :: dist
        real, intent(in) :: pvalue
        real :: a, b, half, phalf, error
        select case (dist%kind)
        case(DIST_UNIFORM)
            ! Easy to do analytically for the uniform distribution
            xCrit = pvalue*(dist%b - dist%a) + dist%a
            return
        case(DIST_NORMAL)
            ! For the others, solve using binary search.
            ! Set starting values based on distribution.
            a = -1.0
            b = 1.0
        case(DIST_CHISQ)
            a = 0.0
            b = 1.0
            xCrit = 0.0
        case default
            ! Default case, no real error handling for now.
            xCrit = 0.0
            return
        end select

        ! First ensure that the required point is
        ! in the interval, by moving and stretching
        ! the interval.
        do while (Cdf(dist, a).gt.pvalue)
            b = a
            a = 2*a
        end do
        do while (Cdf(dist, b).lt.pvalue)
            a = b
            b = 2*b
        end do

        ! Then use binary search to find the value
        error = 1.0
        do while (error .gt. 1e-10)
            half = (a+b)/2.0
            phalf = Cdf(dist,half)
            if (phalf.gt.pvalue) then
                b = half
            elseif (phalf.lt.pvalue) then
                a = half
            end if
            error = abs(phalf-pvalue)
        end do
        
        xCrit = half
        
    end function



    
end module
