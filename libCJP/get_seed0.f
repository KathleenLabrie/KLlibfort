	SUBROUTINE GET_SEED0 ()
c Modified for Linux June 2000 - better ... will work until about 2035 AD.
	INTEGER*4 ISEED, IARRAY(3)
	integer time
cccc	EXTERNAL ITIME
	DATA NFLUSH/2000/
cccc	CALL ITIME (IARRAY)
cccc	SECNDS=3600.*IARRAY(1)+60.*IARRAY(2)+IARRAY(3)
c First get time since Jan 1 1970 ...
	isec=time()
	if (isec .lt. 900000000 .or. isec .gt. 2000000000)
     &    stop 'Error in time function.'
c Randomize it using Press's quick 'n dirty randomizer ...
	iseed=1664525*isec+1013904223
c Initialize ...
	Y=RAN0(iseed)
c Flush ...
	DO 1 I=1,NFLUSH
1	Y=RAN0(0)
	RETURN
	END
