

	read ---> mata & matb

	mata

	langkah 0
	sudah ada mata dan matb

	langkah 1
	mata(2,1) = mata(2,1) - mata(2,1)/mata(1,1) * mata(1,1)
	mata(2,2) = mata(2,2) - mata(2,1)/mata(1,1) * mata(1,2) 
	.
	.
	mata(2,n) = mata(2,n) - mata(2,1)/mata(1,1) * mata(1,n) 

!==================================================================

	do j=1,n
		mata(2,j) = mata(2,j) - mata(2,1)/mata(1,1) * mata(1,j)
	end do
	matb(2) = matb(2) - mata(2,1)/mata(1,1) * matb(1)
	
	do j=1,n
		mata(3,j) = mata(3,j) - mata(3,1)/mata(1,1) * mata(1,j)
	end do
	matb(3) = matb(3) - mata(3,1)/mata(1,1) * matb(1)
	..
	do j=1,n
		mata(i,j) = mata(i,j) - mata(i,1)/mata(1,1) * mata(1,j)
	end do
	matb(i) = matb(i) - mata(i,1)/mata(1,1) * matb(1)

!===================================================================
	langkah 1
	do i=2,n
		do j=1,n
			mata(i,j) = mata(i,j) - mata(i,1)/mata(1,1) * mata(1,j)
		end do
		matb(i) = matb(i) - mata(i,1)/mata(1,1) * matb(1)		
	end do


!===================================================================

	langkah 2
	do i=3,n
		do j=2,n
			mata(i,j) = mata(i,j) - mata(i,2)/mata(2,2) * mata(2,j)
		end do
		matb(i) = matb(i) - mata(i,2)/mata(2,2) * matb(2)		
	end do

!===================================================================


	langkah n-1
	do i=n,n
		do j=2,n
			mata(i,j) = mata(i,j) - mata(i,1)/mata(1,1) * mata(1,j)
		end do
		matb(i) = matb(i) - mata(i,1)/mata(1,1) * matb(1)		
	end do

!===================================================================

	do k=1,n-1
		do i=k+1,n
			do j=k,n
				mata(i,j) = mata(i,j) - mata(i,k)/mata(k,k) * mata(k,j)
			end do
			matb(i) = matb(i) - mata(i,k)/mata(k,k) * matb(k)	
		end do
	end do

!===================================================================

	do k=1,n-1
		d=1./mata(k,k)
		e=matb(k)
		do i=k+1,n
			c=mata(i,k)*d
			do j=k,n
				mata(i,j) = mata(i,j) - c*mata(k,j)
			end do
			matb(i)=matb(i) - c*e	
		end do
	end do

!===================================================================

	matx(n) = matb(n)/mata(n,n)

	matx(n-1) = (matb(n-1)-mata(n-1,n)*matx(n))/mata(n-1,n-1)
	matx(n-2) = (matb(n-2)-mata(n-2,n)*matx(n)-mata(n-2,n-1)*matx(n-1))/mata(n-2,n-2)
	...
	matx(1) = (matb(1)-mat(1,n)*matx(n)-mata(1,n-1)*matx(n-1)
				+ ............................ + mata(1,2)*matx(2))/mata(1,1)

	matx(i) = (matb(i)-mat(i,n)*matx(n)-mata(i,n-1)*matx(n-1)
				+ ............................ + mata(i,i+1)*matx(i+1))/mata(i,i)

	z = mat(i,n)*matx(n)-mata(i,n-1)*matx(n-1) + ............................ + mata(i,i+1)*matx(i+1)

	matx(i) = (matb(i) - z)/mata(i,i)



!===================================================================

	do k=1,n-1

	cari mata(s,k) /= 0., s=k+1,n

	if (mata(k+1,k) /= 0.) then
		do j=k,n
			mata(k,j) <--> mata(k+1,j)
			mata(k,j) -> v
			mata(k+1,j) -> mata(k,j)
			v -> mata(k,j)
		end do
	end if

!===================================================================


	if (mata(k+1,k) /= 0.) then
		do j=k,n
			v = mata(k,j)
			mata(k,j) = mata(k+2,j)
			mata(k+2,j) = v
		end do
		v = matb(k)
		matb(k) = matb(k+1)
		matb(k+1) = v
	end if
	
	if (mata(k+2,k) /= 0.) then
		do j=k,n
			v = mata(k,j)
			mata(k,j) = mata(k+2,j)
			mata(k+2,j) = v
		end do
		v = matb(k)
		matb(k) = matb(k+2)
		matb(k+2) = v
	end if



	do s=k+1,n
		if (mata(s,k) /= 0.) then
			do j=k,n
				v			= mata(k,j)
				mata(k,j) 	= mata(s,j)
				mata(s,j) 	= v
			end do
			v 		= matb(k)
			matb(k) = matb(s)
			matb(s) = v
			exit
		end if
	end do




























