	subroutine minmax (x, n, xmin, xmax)
	real x(1)
	xmin=1.e30
	xmax=-1.e30
	do 1 i=1,n
	xmin=amin1(xmin,x(i))
	xmax=amax1(xmax,x(i))
1	continue
	return
	end
