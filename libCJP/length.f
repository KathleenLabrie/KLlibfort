	FUNCTION LENGTH(CHAR)
	CHARACTER*(*) CHAR
	L=LEN(CHAR)
	DO 1 I=L,1,-1
	LENGTH=I
1	IF (CHAR(I:I) .NE. ' ') RETURN
	LENGTH=1
	RETURN
	END

