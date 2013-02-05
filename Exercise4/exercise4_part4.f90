! Vector type and operations
program Exercise4_part4
    type Vector
        real :: x,y,z
    end type

    type (Vector) :: x, y, z, c, v3
    real :: d, n, s3
    
    x = new_vector( 3.0,-4.0, 0.0)
    y = new_vector(-1.0, 1.0, 1.0)
    z = new_vector( 1.0, 0.0, 0.0)
    
    n = norm(x)
    
    write(*,'(A)', advance='no') "x = "
    call print_vector(x)
    write(*,'(A)', advance='no') "y = "
    call print_vector(y)
    write(*,'(A)', advance='no') "z = "
    call print_vector(z)
    
    d = dot(x,y)
    v3 = cross(x,y)
    s3 = striple(x,y,z)
    c = vtriple(x,y,z)
    
    write(*,*) "norm(x) = ", n
    write(*,*) "dot(x,y) =", d
    write(*,'(A)', advance='no') "cross(x,y) = "
    call print_vector(v3)
    write(*,*) "striple(x,y,z) =", s3
    write(*,'(A)', advance='no') "vtriple(x,y,z) = "
    call print_vector(c)

contains
    ! Vector constructor
    type(Vector) function new_vector(x,y,z)
        real :: x,y,z
        new_vector%x = x
        new_vector%y = y
        new_vector%z = z
    end function

    ! Vector norm
    real function norm(v)
        type(Vector) :: v
        norm = sqrt(v%x**2 + v%y**2 + v%z**2)
    end function

    ! Scalar product
    real function dot(v1, v2)
        type(Vector) :: v1, v2
        dot = v1%x*v2%x + v1%y*v2%y + v1%z*v2%z
    end function
    
    ! Cross product
    type(Vector) function cross(v1, v2)
        type(Vector) :: v1, v2
        real :: a,b,c
        a = v1%y * v2%z - v1%z * v2%y
        b = v1%x * v2%z - v1%z * v2%x
        c = v1%x * v2%y - v1%y * v2%x
        cross%x = a
        cross%y = -b
        cross%z = c
    end function

    ! Scalar triple product
    real function striple(v1, v2, v3)
        type(Vector) :: v1, v2, v3
        striple = dot(v1, cross(v2, v3))
    end function

    ! Vector triple product
    type(Vector) function vtriple(v1, v2, v3)
        type(Vector) :: v1, v2, v3
        vtriple = cross(v1, cross(v2, v3))
    end function

    ! Pretty print vector
    subroutine print_vector(v)
        type(Vector) :: v
        write(*,'(A, F9.5, A, F9.5, A, F9.5, A)') "Vector(", v%x, ", ", v%y, ", ", v%z, ")"
    end subroutine


end program
