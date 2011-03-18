	subroutine minmaxi (x, n, xmin, xmax, imin, imax)
	real x(1)
	xmin=1.e30
	xmax=-1.e30
	do 1 i=1,n
	if (x(i) .lt. xmin) then
	  xmin=x(i)
	  imin=i
	else if (x(i) .gt. xmax) then
	  xmax=x(i)
	  imax=i
	end if
1	continue
	return
	end
