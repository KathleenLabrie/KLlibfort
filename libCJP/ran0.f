	function ran0 (idum)
c Modified slightly from Press et al.; idum=0 means do not initialize.
	parameter (ia=16807, im=2147483647, am=1./im, iq=127773,
     &    ir=2836, mask=123459876)
c Use my get_seed0 to initialize ...
	integer k
	common /random/ iseed
	if (idum .ne. 0) iseed=idum
	iseed=ieor(iseed,mask)
	k=iseed/iq
	iseed=ia*(iseed-k*iq)-ir*k
	if (iseed .lt. 0) iseed=iseed+im
	ran0=am*iseed
	iseed=ieor(iseed,mask)
	return
	end
