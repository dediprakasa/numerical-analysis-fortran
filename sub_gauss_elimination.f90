subroutine GaussElimination(n, mata, matb)

	implicit none
	integer, intent(in)	:: n
	real*8 :: mata(n,n), matb(n), matx(n) 
	real*8 :: c, e, d
	integer :: i, j, k
	real*8, allocatable :: A(:,:), B(:), X(:), cc(:,:)
	
	allocate(A(n,n))
	allocate(B(n))
	allocate(X(n))	
	allocate(cc(n,n))	
	
	A = mata
	B = matb
	X = matx
	

	do k = 1, n - 1
		do i = 1, n-1
			if (A(1,1) == 0.d0) then
				do j = 1, n
					cc(i,j)	  = A(i,j)
			   		A(i,j)    = A(i+1,j)
			   		A(i+1,j)  = cc(i,j)
		   		end do
		   		
		   		c = B(i)
		   		B(i) = B(i+1)
		   		B(i+1) = c
		   		
		   	else
		   		exit
			end if
		end do

	end do
	

	
	do k = 1, n - 1
	
 		d = 1.d0/A(k,k)
 		e = B(k)
 		
 		do i = k + 1, n
  			c = A(i,k)*d
  			do j = k, n
   				A(i,j) = A(i,j) - c*A(k,j)
  			end do
  			
  			B(i)=B(i)-c*e
  			
 		end do
 		
 		do i = k + 1, n
  			if (A(k+1,k+1) == 0.d0) then
   				do j = k, n
    				cc(k+1,j) = A(k+1,j)
    				A(k+1,j)  = A(k+2,j)
    				A(k+2,j)  = cc(k+1,j)
   				end do
   				
   				c = B(k+1)
   				B(k+1) = B(k+2)
   				B(k+2) = c
   				
   				exit
  			end if
 		end do 		
 		
	end do
	

	X(n) = B(n)/A(n,n)
	do i = n - 1, 1, -1
 		e = 0.d0
 		do j = i + 1, n
  			e = e + A(i,j)*X(j)
 		end do

 		X(i)=(B(i) - e)/A(i,i)

	end do
	
!	matx = X
	
	write(*,*)
	write(*,*) '**************Proses triangularisasi selesai!**************'
	
	write(*,*)
	write(*,*) "Matriks A = "
	do i = 1, n
		write(*,*) A(i,:)
	end do
	
	write(*,*)
	write(*,*) "Matriks B = "
	do i = 1, n
		write(*,*) B(i)
	end do
	
	write(*,*)
	write(*,*) 'Matriks X = '
	do i = 1, n
		write(*,*) X(i)
	end do
	
	deallocate(A)
	deallocate(B)
	deallocate(X)
	deallocate(cc)
	
	return
	
end subroutine
	
	
	
	
