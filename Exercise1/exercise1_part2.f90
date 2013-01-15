program exercise1_2
    ! Compute rectangular coordinates from sphericals
    ! theta-angle chosen to go from 0 at the positive z-axis
    ! to pi at the negative z-axis
    implicit none
    real rho, phi, theta
    real x, y, z
    
    write(*,*) "Please enter rho:"
    read(*,*) rho
    write(*,*) "Please enter phi:"
    read(*,*) phi
    write(*,*) "Please enter theta:"
    read(*,*) theta
    
    x = rho * cos(phi) * sin(theta)
    y = rho * sin(phi) * sin(theta)
    z = rho * cos(theta)
    
    write(*,*) "x =", x
    write(*,*) "y =", y
    write(*,*) "z =", z
end program
