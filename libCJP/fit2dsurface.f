	subroutine fit2dsurface (xy, z, zf, n, p, perr, npar0)
c
c Unweighted fit of a linear, quadratic, cubic, etc surface z(x,y). Note
c that xy is the array [x1, y1, x2, y2, ...]. By doing this there is a
c sneaky way to avoid max size parameters (see 'quadxy'). 
c
c Easy to modify to do higher orders: just change npar0 in calling
c args ('quadxy' will take care of itself). npar=6 is quadratic, 3 is
c linear, 10 is cubic, etc.
c
c Note: if you ever want to compute the function outside lqf, just using
c x and y coords as input, use 'surfxy', not 'auxsurfxy'.
c
	real xy(2,n), z(n), zf(n)
	real p(1), perr(1), e1(100)
	common /fit2darr/ npar
	external auxsurfxy
	npar=npar0
	do 2 i=1,npar
2	p(i)=0.
	call lqf (xy, z, zf, wt, e1, perr, p, 0.0, n, npar, 50, nd,
     &    1.e-7, auxsurfxy)
	return
	end

	function auxsurfxy (p, d, xy, l)
	common /fit2darr/ npar
	real p(1), d(1), xy(1)
c Kinda sneaky ... xy(1) is really the l'th element of the 2d array 
c xy(2,N), not what we want. We want elements 2*l-1 and 2*l. Ergo ...
	x=xy(l)
	y=xy(l+1)
c Because of the excessive sneakiness above, use another routine
c to actually calculate the function as a fcn of x and y, in case
c that is ever needed outside the context of 'lqf'.
	auxsurfxy=surfxy (p, d, x, y)
	return
	end

	function surfxy (p, d, x, y)
	real p(1), d(1)
	common /fit2darr/ npar
c Compute function in terms of x and y. Called by auxsurfxy when
c lqf is used. Here we go ...
	d(1)=1.
	surfxy=d(1)*p(1)
	ipar=1
	do 1 iorder=1,3
	do 2 i=iorder,0,-1
	j=iorder-i
	ipar=ipar+1
	d(ipar)=(x**i)*(y**j)
	surfxy=surfxy+d(ipar)*p(ipar)
2	if (ipar .eq. npar) go to 5
1	continue
5	return
	end
