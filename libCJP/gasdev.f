	function gasdev(sig)
c Numerical recipes p. 280
c initialize ran0 with my get_seed
	integer iset
	real gset
	save iset, gset
	data iset/0/
	if (iset .eq. 0) then
1	  v1=2.*ran0(0)-1.
	  v2=2.*ran0(0)-1.
	  rsq=v1*v1+v2*v2
	  if (rsq .ge. 1. .or. rsq .eq. 0.) go to 1
	  fac=sqrt(-2.*log(rsq)/rsq)
	  gset=v1*fac
	  gasdev=v2*fac
	  iset=1
	else
	  gasdev=gset
	  iset=0
	end if
	gasdev=gasdev*sig
	return
	end
