! LU decomposition
program Exercise4_part2
    integer, parameter :: n = 4
    
    real :: A(n,n), L(n,n), U(n,n)
    real :: s
    integer i,j,m,k
    
    L = 0.0
    U = 0.0
    
    A(1,:) = (/ 1, 2, 3, 4 /)
    A(2,:) = (/ 1, 3, 1, 2 /)
    A(3,:) = (/ 2, 1, 1, 1 /)
    A(4,:) = (/ 3, 1, 0, 1 /)
    
    do i = 1,n
        L(i,1) = A(i,1)
    end do
    
    U(1,1) = 1
    do j = 2,n
        U(1,j) = A(1,j) / L(1,1)
    end do
    
    do m = 2,n
        do i = m,n
            s = 0.0
            do k = 1,m-1
                s = s + L(i,k) * U(k,m)
            end do
            L(i,m) = A(i,m) - s
        end do
        U(m,m) = 1
        do i = m+1, n
            s = 0.0
            do k = 1,m-1
                s = s + L(m,k) * U(k,i)
            end do
            U(m,i) = (A(m,i) - s) / L(m,m)
        end do
    end do
    
    write(*,*) "Upper matrix:"
    call print_matrix(U)
    write(*,*) "Lower matrix:"
    call print_matrix(L)
    
contains
    subroutine print_matrix(A)
        real :: A(:,:)
        do i = 1,size(A,1)
            write(*,*) A(i,:)
        end do
    end subroutine
    
end program
