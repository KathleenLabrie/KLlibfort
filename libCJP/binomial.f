	FUNCTION BINOMIAL (NX, N, P)
C
C Binomial function - Bevington pp. 30-31
C
	BINOMIAL = FACTORIAL(N) / (FACTORIAL(NX) * FACTORIAL(N-NX)) *
     &    (P**NX) * ((1.-P)**(N-NX))
	RETURN
	END
