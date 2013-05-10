program DistributionTests
    use Distributions
    
    type(Distribution) :: test1, test2, test3
    integer :: i
    real :: d, x, trueP, trueC
    
    test1 = Uniform(0.0, 2.0)
    write(6,*)
    write(6,*) "Testing U(0,2) distribution"
    write(6,*) "============================="
    write(6,*) "mean:", mean(test1)
    write(6,*) "median:", median(test1)
    write(6,*) "std:", std(test1)
    write(6,*) "       x               pdf              cdf"
    do i = 0,10
        d = (i/10.0)
        x = 3*d -0.5
        write(6,*) x, Pdf(test1, x), Cdf(test1,x)
    end do
    write(6,*)
    write(6,*) "xCrit(0.25):", xCrit(test1, 0.25)
    write(6,*) "xCrit(0.50):", xCrit(test1, 0.50)
    write(6,*) "xCrit(0.75):", xCrit(test1, 0.75)

    test2 = Normal(0.0, 1.0)
    write(6,*)
    write(6,*) "Testing N(0,1) distribution"
    write(6,*) "============================="
    write(6,*) "mean:", mean(test2)
    write(6,*) "median:", median(test2)
    write(6,*) "std:", std(test2)
    write(6,*) "       x               pdf           true pdf            cdf"
    do i = 0,10
        d = (i/10.0)
        x = 3*d - 1.5
        trueP = exp(-x**2 / 2) / sqrt(2*pi)
        write(6,*) x, Pdf(test2, x), trueP, Cdf(test2,x)
    end do
    write(6,*)
    write(6,*) "xCrit(0.25):", xCrit(test2, 0.25), "true (approx.): -0.674"
    write(6,*) "xCrit(0.50):", xCrit(test2, 0.50), "true (exactly): 0.0)"
    write(6,*) "xCrit(0.75):", xCrit(test2, 0.75), "true (approx.): 0.674"

    test3 = ChiSq(2)
    write(6,*)
    write(6,*) "Testing ChiSq(2) distribution"
    write(6,*) "============================="
    write(6,*) "mean:", mean(test3)
    write(6,*) "median:", median(test3)
    write(6,*) "std:", std(test3)
    write(6,*) "       x               pdf           true pdf            cdf            true cdf"
    do i = 0,10
        d = (i/10.0)
        x = 3*d
        trueP = 0.5 * exp(-x/2)
        trueC = 1 - exp(-x/2)
        write(6,*) x, Pdf(test3, x), trueP, Cdf(test3,x), trueC
    end do
    write(6,*)
    write(6,*) "xCrit(0.25):", xCrit(test3, 0.25)
    write(6,*) "xCrit(0.50):", xCrit(test3, 0.50)
    write(6,*) "xCrit(0.75):", xCrit(test3, 0.75)
    
    write(6,*)
end program
