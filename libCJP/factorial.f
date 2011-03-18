	FUNCTION FACTORIAL (N)
C
C Simplistic factorial - prone to overflows for large N!!
C
	FACTORIAL=1.
	IF (N.LE.1) RETURN
	DO 1 I=2,N
1	FACTORIAL = FACTORIAL*I
	RETURN
	END
