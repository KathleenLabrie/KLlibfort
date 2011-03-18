	subroutine ludcmp (a, n, np, indx, d)
	integer n, np, indx(n), nmax
	real d, a(np,np), tiny
	parameter (nmax=500, tiny=1.e-30)
	integer i, imax, j, k
	real aamax, dum, sum, vv(nmax)
	d=1.
	do 12 i=1,n
	aamax=0.
	do 11 j=1,n
11	if (abs(a(i,j)) .gt. aamax) aamax=abs(a(i,j))
	if (aamax .eq. 0.) pause 'Singular matrix in ludcmp.'
12	vv(i)=1./aamax
	do 19 j=1,n
	do 14 i=1,j-1
	sum=a(i,j)
	do 13 k=1,i-1
13	sum=sum-a(i,k)*a(k,j)
14	a(i,j)=sum
	aamax=0.
	do 16 i=j,n
	sum=a(i,j)
	do 15 k=1,j-1
15	sum=sum-a(i,k)*a(k,j)
	a(i,j)=sum
	dum=vv(i)*abs(sum)
	if (dum .ge. aamax) then
	  imax=i
	  aamax=dum
	end if
16	continue
	if (j .ne. imax) then
	  do 17 k=1,n
	  dum=a(imax,k)
	  a(imax,k)=a(j,k)
17	  a(j,k)=dum
	  d=-d
	  vv(imax)=vv(j)
	end if
	indx(j)=imax
	if (a(j,j) .eq. 0.) a(j,j)=tiny
	if (j .ne. n) then
	  dum=1./a(j,j)
	  do 18 i=j+1,n
18	  a(i,j)=a(i,j)*dum
	end if
19	continue
	return
	end
