program bisection

	implicit none
	real, parameter::eps=0.001
	real :: a,b,c,x,rel_err

! User inputs the boundaries
	do
		write(*,*) 'Input boundary a and b!'
		read(*,*) a, b
		
		if (func(a)*func(b) < 0.d0) exit
		
	end do
	
! Define c as the midpoint between a and b
	c = (a+b)/2.
	
! Iteration loop
	do
		! Update the value of a and b
		if (func(a)*func(c) < 0.) then
			b = c
		else
			a = c
		end if
	
		! Define x as the new midpoint between a and b
		x = (a+b)/2.
		
		! Set the value of the relative error
		rel_err = abs((c-x)/x)
		
		! Convergence check
		if (rel_err < eps) exit
		
		! Update the value of c with the new midpoint x
		c = x
		
	end do
	
! Display the results
	write(*,*) 'Root = ', x
	
	stop
	
	contains
	
! Function definition
	function func(x) result(y)
	
		implicit none
		real, intent(in) :: x
		real :: y

		y = cos(x)-x

		return
	
	end function
	
end program bisection