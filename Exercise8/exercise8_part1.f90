! Compute the volume of an n-dimensional unit sphere
! through Monte-Carlo.
program Exercise8_part1
    real :: v_cube, v_sphere, r
    integer :: i, N, count
    integer, parameter :: dim = 3
    real, dimension(dim) :: point
    
    N = 1000000
    count = 0
    
    v_cube = 2.0**dim
    
    ! Generate points in a cube and count those which
    ! are inside the unit sphere.
    do i = 1,N
        call random_number(point)
        point = 2.0 * (point - 0.5)
        r = norm(point)
        if (r .lt. 1.0) then
            count = count + 1
        end if
    end do
    
    v_sphere = v_cube * count / N
    
    write(6,*) "  Dimension:", dim
    write(6,*) "     Nsteps:", N
    write(6,*) "Monte-Carlo:", v_sphere
    write(6,*) " Analytical:", volume(dim, 1.0)
    
contains
    ! Function to compute the norm of an n-dimensional vector
    real function norm(vector)
        real, dimension(:) :: vector
        integer :: N,i
        N = size(vector)
        norm = 0.0
        do i = 1,N
            norm = norm + vector(i)**2
        end do
        norm = sqrt(norm)
    end function
    
    ! Funtion to compute the true volume of n-dimensional sphere
    real function volume(dim, R)
        integer :: dim
        real :: R
        volume = 3.14159265**(dim/2.0) * R**dim
        if (mod(dim,2).eq.0) then
            ! even dimension
            volume = volume / factorial(dim/2)
        else
            ! odd dimension
            volume = volume / oddfact(dim/2)
        end if
    end function
    
    ! Factorial function
    integer function factorial(n)
        integer :: n, i
        factorial = 1
        do i = 2,n
            factorial = factorial * i
        end do
    end function
    
    ! Half-integer factorial
    real function oddfact(m)
        integer :: m
        oddfact = factorial(2*m + 2) * 1.77245385091
        oddfact = oddfact / factorial(m+1)
        oddfact = oddfact / 4**(m+1)
    end function
    
end program