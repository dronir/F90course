program DistributionTests
    use Distributions
    
    type(Distribution) :: test1, test2, test3
    integer :: i
    real :: d, x, trueP, trueC
    
    write(6,*)
    write(6,*) "Testing U(-1, 1) distribution"
    write(6,*) "============================="
    write(6,*) "       x               pdf              cdf"
    test1 = Uniform(-1.0, 1.0)
    do i = 0,21
        d = (i/21.0)
        x = 3*d - 1.5
        write(6,*) x, Pdf(test1, x), Cdf(test1,x)
    end do

    write(6,*)
    write(6,*) "Testing N(0, 1) distribution"
    write(6,*) "============================="
    write(6,*) "       x               pdf              cdf"
    test2 = Normal(0.0, 1.0)
    do i = 0,21
        d = (i/21.0)
        x = 3*d - 1.5
        write(6,*) x, Pdf(test2, x), Cdf(test2,x)
    end do

    write(6,*)
    write(6,*) "Testing ChiSq(2) distribution"
    write(6,*) "============================="
    write(6,*) "       x               pdf           true pdf            cdf            true cdf"
    test3 = ChiSq(2)
    do i = 0,21
        d = (i/21.0)
        x = 3*d
        trueP = 0.5 * exp(-x/2)
        trueC = 1 - exp(-x/2)
        write(6,*) x, Pdf(test3, x), trueP, Cdf(test3,x), trueC
    end do
    
    write(6,*)
end program
