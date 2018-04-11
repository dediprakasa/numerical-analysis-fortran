program trapezoid
    
    implicit none
    real*8::a,b,h,integral
    real*8, external::func
    
    ! Determining the limits of integration
    a=0.d0
    b=2.d0
    
    ! Determining the width of quadrature
    h=b-a
    
    ! Calculating the result of integration
    integral=0.5*h*(func(b)+func(a))
    print *, 'The result is ', integral
           
end program
