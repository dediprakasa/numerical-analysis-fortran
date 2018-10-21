subroutine lu_decomposition(n, a)

	implicit none
	integer, intent(in) :: n
	integer :: i, j, k, s, g, kk, jj, ii
	real*8, dimension(n,n) :: a
	real*8 :: c, detl, detu
	real*8, allocatable :: l(:,:), u(:,:), cc(:,:)
	
	allocate(l(n,n))
	allocate(u(n,n))
	allocate(cc(n,n))
	

!	do i= 1,n-1
!		do j= i+1,n
!			l(i,j) = 0.
!			u(j,i) = 0.
!		end do
!	end do

	l(:,:) = 0.d0
	u(:,:) = 0.d0
	
	do i=1,n
		u(i,i) = 1.    ! Benar
	end do
	
	do k = 1, n - 1
		do i = 1, n-1
			if (a(1,1) == 0.d0) then
				do j = 1, n
					cc(i,j)	  = a(i,j)
			   		a(i,j)    = a(i+1,j)
			   		a(i+1,j)  = cc(i,j)
		   		end do
		   	else
		   		exit
			end if
		end do
	end do

	do i=1,n
		l(i,1) = a(i,1)		! Benar
	end do

	do j=2,n	
		u(1,j) = a(1,j)/l(1,1)		! Benar
	end do
	

	
	
!	do k=2,n
!		do i=k,n
!			c = 0.
!			do s=1,k-1
!				c = c + l(i,s)*u(s,k)
!			end do
!			l(i,k) = a(i,k) - c
!		end do
!
!	end do
!
!	do i = 2, n - 1
!		do k=i+1,n
!			c = 0.
!			do s = 1,i-1
!				c = c + l(i,s)*u(s,k)
!			end do
!			u(i,k) = (a(i,k) - c)/l(i,i)
!		end do
!	end do

		do ii = kk + 1, n - 1
	  		if (l(kk+1,kk+1) == 0.d0) then
	   			do jj = kk, n
	   				cc(kk+1,j) = l(kk+1,jj)
	   				l(kk+1,jj)  = l(kk+2,jj)
	   				l(kk+2,jj)  = cc(kk+1,jj)
	   				cc(kk+1,jj) = u(kk+1,jj)
	   				u(k+1,jj)  = u(kk+2,jj)
	   				u(kk+2,jj)  = cc(kk+1,jj)
	   			end do
	 		else
	   			exit
	   		end if
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
	write(*,*) '**************Proses dekomposisi selesai!**************'
	
	write(*,*)
	write(*,*) 'Matriks L = '
	do i = 1, n
		write(*,*) l(i,:)
	end do
	
	write(*,*)
	write(*,*) 'Matriks U = '
	do i = 1, n
		write(*,*) u(i,:)
	end do

	
	write(*,*) 
	write(*,*) 'Determinan matriks L =', detl
	

	
	write(*,*) 
	write(*,*) 'Determinan matriks U =', detu
	
	deallocate(l)
	deallocate(u)
	deallocate(cc)
	
	return

end subroutine lu_decomposition
