	SUBROUTINE NEWTON (XSTART, DX, XFINAL, TOL, F)
c
c Solve F(X) = 0 for X iteratively. XSTART is the starting value.
c DX is the range over which to compute derivative numerically.
c XFINAL is the solution which is obtained when F(X) changes by
c <= TOL between successive iterations.
c
	EXTERNAL F
	X0=XSTART
C
9	SLOPE=(F(X0+DX)-F(X0-DX))/(2*DX)
	FX0=F(X0)
	XFINAL=X0-FX0/SLOPE
	FXFINAL=F(XFINAL)
	IF (ABS(FXFINAL-FX0) .LE. TOL) RETURN
	X0=XFINAL
	GO TO 9
	END
