	SUBROUTINE LINFIT (X, Y, SIG, N, A, B, SIGA, SIGB)
C 
C Weighted linear least squares fit to Y=aX+b
C See Bevington p.118.
C
	REAL X(1),Y(1),SIG(1)
	SUMX=0.
	SUMY=0.
	SUMXY=0.
	SUMXX=0.
	SUMSIGM2=0.
	DO 1 I=1,N
	SUMX=SUMX+X(I)/SIG(I)**2
	SUMY=SUMY+Y(I)/SIG(I)**2
	SUMXY=SUMXY+X(I)*Y(I)/SIG(I)**2
	SUMXX=SUMXX+(X(I)/SIG(I))**2
1	SUMSIGM2=SUMSIGM2+(1./SIG(I)**2)
	DELTA=SUMSIGM2*SUMXX-SUMX**2
	A=(SUMXX*SUMY-SUMX*SUMXY)/DELTA
	B=(SUMSIGM2*SUMXY-SUMX*SUMY)/DELTA
	SIGA=SQRT(SUMXX/DELTA)
	SIGB=SQRT(SUMSIGM2/DELTA)
	RETURN
	END
