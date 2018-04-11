function func(x)

    implicit none
    real*8 :: func
    real*8, intent(in) :: x
    
    func = sqrt(1 + x**2)


end function
