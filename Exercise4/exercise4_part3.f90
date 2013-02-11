! Solve equation system with Jacobian iteration
program Exercise1_part3
    integer, parameter :: n = 4
    real, parameter :: epsilon = 1e-6
    
    integer :: i,j,iter
    real :: M(n), A(n,n), x(n), b(n), y(n)
    real :: delta, s
    
    ! Matrix with +10 on the diagonal.
    ! With the original matrix the iteration below
    ! does not converge.
    A(1,:) = (/ 11, 2, 3, 4 /)
    A(2,:) = (/ 1, 13, 1, 2 /)
    A(3,:) = (/ 2, 1, 11, 1 /)
    A(4,:) = (/ 3, 1, 0, 11 /)
    b = (/ 20, 11, 6, 4 /)
    
    x = 0.0
    
    do i = 1,n
        M(i) = A(i,i)
        A(i,i) = 0.0
    end do
    
    iter = 0
    delta = 1.0
    do while (delta > epsilon)
        do i = 1,n
            s = 0.0
            do j = 1,n
                s = s - A(i,j)*x(j)
            end do
            y(i) = (s + b(i)) / M(i)
        end do
        delta = 0.0
        do i = 1,n
            delta = delta + (x(i)-y(i))**2
        end do
        x = y
        write(*,*) "(", x, "),   delta:", delta
        iter = iter + 1
        if (iter > 20) exit
    end do


end program
