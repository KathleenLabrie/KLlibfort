	SUBROUTINE LINEQ (N, A, B)
C
C Single precision solution of N equations in N unknowns. Uses Numerical
C Recipes routines.
C
C The ordering of the array elements for A is the 'logical' way - i.e.
C the first N elements of A correspoind to the coefficients of the first
C linear equation, etc ...
C
	parameter (nmax=500)
	real*4 a(n,n), b(n), d, temp
	integer indx(nmax)
	do 1 j=1,n-1
	do 1 i=j+1,n
	temp=a(i,j)
	a(i,j)=a(j,i)
1	a(j,i)=temp
	call ludcmp (a, n, n, indx, d)
	call lubksb (a, n, n, indx, b)
	return
	end
