! Tridiagonal equation solver
program Exercise4_part1
    integer, parameter :: n = 3
    real :: diag(n), udiag(n-1), ldiag(n-1), B(n)
    integer :: i

    diag = (/ 1, 2, 2 /)
    udiag = (/-1, 0/)
    ldiag = (/0, -1/)
    b = (/1,0,1 /)
    
    ! Using algorithm found at http://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm#Method
    ! Replace B vector with solution, also overwrite upper diagonal.

    ! Forward sweep
    udiag(1) = udiag(1) / diag(1)
    B(1) = B(1) / diag(1)
    do i = 2,n-1
        udiag(i) = udiag(i) / (diag(i) - udiag(i-1)*ldiag(i-1))
        B(i) = (B(i) - B(i-1)*ldiag(i-1)) / (diag(i) - udiag(i-1)*ldiag(i-1))
    end do
    B(n) = (B(n) - B(n-1)*ldiag(n-1)) / (diag(n) - udiag(n-1)*ldiag(n-1))
    
    ! Back substitution
    do i = n-1,1,-1
        B(i) = B(i) - udiag(i) * B(i+1)
    end do
    
    write(*,*) "Solution is:"
    write(*,*) B

end program
