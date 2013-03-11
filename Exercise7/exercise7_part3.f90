program Exercise7_part3
    integer :: n
    real :: S
    real, parameter :: pi = 3.141592653
    real, external :: f
    
    write(*,*)
    write(*,*) "    Nsteps    Integral         Difference"
    do n = 2,11
        S = quadrature(f, 0.0, pi/2, n)
        write(*,*) n, S, (1-S)
    end do 
    
contains
    ! Compute Gaussian quadrature of f over (a,b) with N points
    real function quadrature(f,a,b,N)
        interface
            real function f(x)
                real, intent(in) :: x
            end function
        end interface
        real, intent(in) :: a,b
        integer, intent(in) :: N
        integer :: i, start
        real, dimension(ceiling(N/2.0)) :: w, x
        real :: y, S
        
        call read_gauss_coefs(N,w,x)
        
        if (mod(N,2) .eq. 0) then
            S = 0.0
            start = 1
        else
            S = w(1) * f((b+a)/2)
            start = 2
        end if
        
        do i = start,ceiling(N/2.0)
            y = x(i) * (b-a)/2 + (b+a)/2
            S = S + w(i) * f(y)
            y = -x(i) * (b-a)/2 + (b+a)/2
            S = S + w(i) * f(y)
        end do
        quadrature = S * (b-a) / 2
    end function
    
    ! Subroutine to read quadrature coefficients from file
    subroutine read_gauss_coefs(N, w, x)
        integer, intent(in) :: N
        real, dimension(ceiling(N/2.0)) :: w, x
        real :: a,b
        integer :: i, j, k, dummy

        open(1, file='gauss.dat', status='old')
        
        do i = 2,N
            k = ceiling(i/2.0)
            read(1,*) dummy
            do j = 1,k
                read(1,'(F24.15,F22.15)') a, b
                if (i .eq. N) then
                    w(j) = b
                    x(j) = a
                end if
            end do
        end do
        
        close(1)
    end subroutine
    
end program
