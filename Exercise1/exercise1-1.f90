program exercise1_1
    ! A program that asks three real numbers and prints their mean value
    implicit none
    real a,b,c,mean
    
    write(*,*) "Give three real values, separated by commas"
    read(*,*) a,b,c
    
    mean = (a+b+c) / 3.0
    
    write(*,*) "The mean is", mean
end program
