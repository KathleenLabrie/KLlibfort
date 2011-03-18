	SUBROUTINE DLINEQ (N, A, B)
C
C Double precision solution of N equations in N unknowns. Uses Numerical
C Recipes routines.
C
C The ordering of the array elements for A is the 'logical' way - i.e.
C the first N elements of A correspoind to the coefficients of the first
C linear equation, etc ...
C
	parameter (nmax=5625)
	real*8 a(n,n), b(n), d, temp
	integer indx(nmax)
c KL
	integer ksize
	ksize = ifix(sqrt(real(nmax)))
	if (n .gt. nmax) then
	  write(*,*) 'ERROR: In dlineq: array is too large (max:',
     &			ksize,' x',ksize,')'
	  write(*,*) 'Exiting.'
	  stop
	endif
c end KL
	do 1 j=1,n-1
	do 1 i=j+1,n
	temp=a(i,j)
	a(i,j)=a(j,i)
1	a(j,i)=temp
	call dludcmp (a, n, n, indx, d)
	call dlubksb (a, n, n, indx, b)
	return
	end
