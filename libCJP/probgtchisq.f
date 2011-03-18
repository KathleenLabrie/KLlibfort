	function probgtchisq (chisq, nu)
c
c Returns the probability that a chisquared value > chisq will
c be obtained if a model is correct, given nu degrees of freedom.
c This works for big nu - not terribly accurate at the high chisq
c end but OK. Uses the asymptotic form of the chisqare distribution
c (a gaussian with mean nu and std dev sqrt(2 vu).
c
	sqrtnu=sqrt(float(nu))
	probgtchisq=(1.-erf((chisq-nu)/(2.*sqrtnu))) /
     &    (1.+erf(sqrtnu/2.))
	return
	end
