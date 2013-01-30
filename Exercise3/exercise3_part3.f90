! 2D-array magic
program Exercise3_part3
    integer :: A(5,5)
    integer :: temp
    integer :: i,j
    integer :: i_min(2), i_max(2)
    do i = 1,5
        do j = 1,5
            A(i,j) = (i-3)-(2*j-2)
        end do
    end do
    write(*,*) "Array A at start:"
    call print_matrix(A)
 
    i_min = minloc(A)
    i_max = maxloc(A)

    write(*,*) "Largest value at: ", i_max
    write(*,*) "Smallest value at:", i_min
    write(*,*)

    temp = A(i_min(1), i_min(2))
    A(i_min(1), i_min(2)) = A(i_max(1), i_max(2))
    A(i_max(1), i_max(2)) = temp
    write(*,*) "Array A at end:"
    call print_matrix(A)
    
contains
    subroutine print_matrix(A)
        integer :: A(:,:)
        integer :: lower(2), upper(2)
        lower = lbound(A)
        upper = ubound(A)
        do i = lower(1),upper(1)
            write(*,*) A(i,:)
        end do
    end subroutine
    
end program
