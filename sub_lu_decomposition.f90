subroutine lu_decomposition(n, a)

	implicit none
	integer, intent(in) :: n
	integer :: i, j, k, s
	real*8, dimension(n,n) :: a
	real*8 :: c, detl, detu
	real, allocatable :: l(:,:), u(:,:)
	
	allocate(l(n,n))
	allocate(u(n,n))
	
	do i= 1,n-1
		do j= i+1,n
			l(i,j) = 0.
			u(j,i) = 0.
		end do
	end do

	do i=1,n
		u(i,i) = 1.
	end do 

	do i=1,n
		l(i,1) = a(i,1)
	end do

	do j=2,n	
		u(1,j) = a(1,j)/l(1,1)
	end do

	do k=2,n-1

		do i=k,n
			c = 0.
			do s=1,k-1
				c = c + l(i,s)*u(s,k)
			end do
			l(i,k) = a(i,k) - c
		end do

		do j=k+1,n
			c = 0.
			do s = 1,k-1
				c = c + l(k,s)*u(s,j)
			end do
			u(k,j) = (a(k,j) - c)/l(k,k)
		end do

	end do

	c = 0.
	do s=1,n-1
		c = c + l(n,s)*u(s,n)
	end do
	l(n,n) = a(n,n) - c 

	! determinan
	detl = 1.
	detu = 1.
	do i=1,n
		detl=detl*l(i,i)
		detu = detu * u(i,i)
	end do
	
	write(*,*)
	write(*,*) 'Matriks L = '
	do i = 1, n
		write(*,*) l(i,:)
	end do
	
	write(*,*) 
	write(*,*) 'Determinan matriks L =', detl
	
	write(*,*)
	write(*,*) 'Matriks U = '
	do i = 1, n
		write(*,*) u(i,:)
	end do
	
	write(*,*) 
	write(*,*) 'Determinan matriks U =', detu
	
	deallocate(l)
	deallocate(u)
	
	return

end subroutine lu_decomposition
