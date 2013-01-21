! Harmonic series function
program Exercise2_1
    integer N
    real result, reverse
    write(*,*) "Enter number of terms:"
    read(*,*) N
    result = Harmonic(N)
    write(*,*) "Result is        ", result
    reverse = HarmonicReverse(N)
    write(*,*) "Reverse result is", result


! Function definitions follow
contains

real function Harmonic(n)
    integer n, i
    Harmonic = 0.0
    do i = 1,n
        Harmonic = Harmonic + 1.0/i
    end do
end function

real function HarmonicReverse(n)
    integer n, i, k
    HarmonicReverse = 0.0
    do i = 1,n
        k = n-i+1
        HarmonicReverse = HarmonicReverse + 1.0/k
    end do
end function

end program