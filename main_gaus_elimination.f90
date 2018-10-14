program tes

	implicit none
	integer :: i, n, nn
	real*8, allocatable :: A(:,:), B(:)
	
	write(*,*) 'Matriks A berukuran n x n. Masukkan nilai n!'
	read(*,*) n
	
	allocate(A(n,n))
	allocate(B(n))
!	allocate(X(n))
	
	write(*,*)
	write(*,*) 'Masukkan nilai matriks A'
	do i = 1, n
		read(*,*) A(i,:)
	end do
	
	write(*,*) 'Masukkan nilai matriks kolom B'
	do i = 1, n
		read(*,*) B(i)
	end do
	
	write(*,*) 'Matriks A = '
	do i = 1, n
		write(*,*) A(i,:)
	end do
	
	write(*,*)
	write(*,*) 'Matriks B = '
	do i = 1, n
		write(*,*) B(i)
	end do
	
	call GaussElimination(n, A, B)
	

	deallocate(A)
	deallocate(B)
!	deallocate(X)
end program	
