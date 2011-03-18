	SUBROUTINE SHUFFLE (A, N, INDEX, TEMP)
C
C Reorder an array A according to the ordering in INDEX ...
C
	DIMENSION A(1), TEMP(1), INDEX(1)
	DO 1 I=1,N
1	TEMP(I)=A(INDEX(I))
	DO 2 I=1,N
2	A(I)=TEMP(I)
	RETURN
	END
