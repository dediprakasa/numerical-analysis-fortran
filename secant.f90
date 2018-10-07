program secant
	
	implicit none
	real, parameter :: eps=0.00001
	real :: a,b,c,x,rel_err, df
	integer :: iter

! User inputs the boundaries
	write(*,*) 'Input boundary a and b!'
	read(*,*) a, b




! Iteration loop
	iter=0
	do
		! Count iteration
		iter = iter+1

		df = (b-a)/(func(b)-func(a))
		x = b - func(b)*df
		
		if (abs(func(b)) .lt. eps) exit
	
		a = b
		b = x

		
	end do

! Display the results
	write(*,*) 'Root = ', x
	write(*,*) 'Convergence achieved after ', iter, ' iteration.'


	contains
	
! Function definition
	function func(x) result(y)
	
		implicit none
		real, intent(in) :: x
		real :: y

		y = cos(x)-x

		return
	
	end function

end program secant
