program DistributionTests
    use Distributions
    
    type(Distribution) :: test1
    integer :: i
    real :: d, x
    
    write(6,*)
    write(6,*) "Testing U(-1, 1) distribution"
    write(6,*) "============================="
    test1 = Uniform(-1.0, 1.0)
    do i = 0,21
        d = (i/21.0)
        x = 3*d - 1.5
        write(6,*) x, Pdf(test1, x)
    end do
    
    write(6,*)
end program
