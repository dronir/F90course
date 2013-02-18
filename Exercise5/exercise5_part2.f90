! Pade approximation of sin(x) from Taylor coefficients
program Exercise5_part2
    real :: x
    real :: coefs(5)
    integer :: i
    
    ! Compute Pade coefs from sine Taylor coefs
    coefs = pade_coef((/ 0.0, 1.0, 0.0, -1/6.0, 0.0 /))
    
    ! Print Pade coefs
    write(*,*) "a =", coefs(1)
    write(*,*) "b =", coefs(2)
    write(*,*) "c =", coefs(3)
    write(*,*) "d =", coefs(4)
    write(*,*) "e =", coefs(5)
    write(*,*)
    
    ! Print approximation up to pi/2
    write(*,*) "    x              sin(x)          pade approx."
    do i = 0,20
        x = 0.5 * 3.141592 * i / 21
        write(*,*) x, sin(x), pade_approx(x, coefs)
    end do
    
    
    
contains
    real function pade_approx(x, pade_coefs)
        real :: x,a,b,c,d,e
        real :: pade_coefs(5)
        a = pade_coefs(1)
        b = pade_coefs(2)
        c = pade_coefs(3)
        d = pade_coefs(4)
        e = pade_coefs(5)
        pade_approx = (a + b*x + c*x**2) / (1 + d*x + e*x**2)
    end function
    
    ! Compute Pade coefficients a,b,c,d,e given 
    ! five first Taylor series coefficients.
    function pade_coef(taylor)
        real :: taylor(5)
        real :: pade_coef(5)
        real :: t,u,v,w,q
        t = taylor(1)
        u = taylor(2)
        v = taylor(3)
        w = taylor(4)
        q = taylor(5)
        pade_coef(5) = (v*q - w**2) / (u*w - v**2)
        pade_coef(4) = -(pade_coef(5)*v/w + q/w)
        pade_coef(3) = v + u*pade_coef(4) + t*pade_coef(5)
        pade_coef(2) = u + t*pade_coef(4)
        pade_coef(1) = t
    end function
    
end program
