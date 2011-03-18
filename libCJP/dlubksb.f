	subroutine dlubksb (a, n, np, indx, b)
	integer n,np,indx(n)
	real*8 a(np,np), b(n)
	integer i, ii, j, ll
	real*8 sum
	ii=0
	do 12 i=1,n
	ll=indx(i)
	sum=b(ll)
	b(ll)=b(i)
	if (ii .ne. 0) then
	  do 11 j=ii,i-1
11	  sum=sum-a(i,j)*b(j)
	else if (sum .ne. 0.d0) then
	  ii=i
	end if
12	b(i)=sum
	do 14 i=n,1,-1
	sum=b(i)
	do 13 j=i+1,n
13	sum=sum-a(i,j)*b(j)
14	b(i)=sum/a(i,i)
	return
	end
