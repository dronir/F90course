program Exercise3_part2
    integer :: a(10), b(20)
    integer :: i

    ! First part, copy odd elements of b to a
    a = 0
    b = (/ (100*i, i = 1,20) /)
    write(*,*) "b: ", b
    write(*,*)
    
    write(*,*) "### First part"
    a = b(::2)
    write(*,*) "a:", a
    write(*,*)
    
    ! Second part, copy elements 1,2,3,4,5,11,13,17,20
    a = b((/ 1,2,3,4,5,10,11,13,17,20 /))
    write(*,*) "### Second part"
    write(*,*) "a:", a
    write(*,*)
    
    ! Third part, replace negative values with their squares
    a = (/ (10*i*(-1)**i, i = 1,10) /)
    write(*,*) "### Third part"
    write(*,*) "a before:", a
    where (a < 0) a = a**2
    write(*,*) "a after: ", a    
    

end program