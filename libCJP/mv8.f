	subroutine mv8 (a, b, n)
	real*8 a(1), b(1)
	do 1 i=1,n
1	b(i)=a(i)
	return
	end
