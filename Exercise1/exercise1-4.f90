program exercise1_4
    ! Compute harmonic series up to degree given by user
    integer N, i
    double precision S
    
    write(*,*) "Please enter number of terms"
    read(*,*) N
    
    do i = 1,N
        S = S + 1.0/i
    end do
    
    write(*,*) "The sum equals", S
    
end program
