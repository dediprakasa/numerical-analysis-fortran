program BiSecant

	implicit none
	integer :: input

	! User choose which method to use
	do
		write(*,*) 'Input 1 for bisection and 2 for secant!'
		read(*,*) input
		if ((input == 1) .or. (input == 2)) then
			exit
		else
			write(*,*) 'Input is not identified. Input 1 for bisection and 2 for secant!'
		end if
		
	end do

		if (input == 1) then
			call Bisection(func)
		else
			call Secant(func)
		end if

	contains

!======================================================================
	subroutine Bisection(f)

		implicit none
		real, parameter::eps=0.001
		real :: f,a,b,c,x,rel_err
		integer :: iter

		do
			write(*,*) 'Input boundary a and b!'
			read(*,*) a, b
			
			if (f(a)*f(b) < 0.d0) then
				exit
			else
				write(*,*) "The boundaries don't bracket the root function, input other values!"
			end if
		end do
	
		! Define c as the midpoint between a and b
		c = (a+b)/2.
		
		! Iteration loop
		iter=0
		do
			! Count iteration
			iter = iter+1
	
			! Update the value of a and b
			if (f(a)*f(c) < 0.) then
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
		write(*,*) 'Convergence achieved after ', iter, ' iteration.'
		return

	end subroutine

!======================================================================

	subroutine Secant(f)

		implicit none
		real, parameter :: eps=0.00001
		real :: f,a,b,c,x,rel_err, df
		integer :: iter

		! User inputs the boundaries
		write(*,*) 'Input boundary a and b!'
		read(*,*) a, b




		! Iteration loop
		iter=0
		do
			! Count iteration
			iter = iter+1

			df = (b-a)/(f(b)-f(a))
			x = b - f(b)*df
			
			if (abs(f(b)) .lt. eps) exit
		
			a = b
			b = x

			
		end do

		! Display the results
		write(*,*) 'Root = ', x
		write(*,*) 'Convergence achieved after ', iter, ' iteration.'

		return

	end subroutine

!======================================================================

	function func(x) result(y)
	
		implicit none
		real, intent(in) :: x
		real :: y

		y = cos(x)-x**2

		return
	
	end function

end program Bisecant














