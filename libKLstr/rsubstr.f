c  Find last occurence of a substring

	function rsubstr (str, sub)
	character*(*) str, sub
	l = length(str)
	lsub = length(sub)
	rsubstr = 0
	i=1
	do while (i .le. l)
	  k = index(str(i:), sub(1:lsub))
	  if (k .eq. 0) return
	  rsubstr = k + i-1
	  i = k+1
	enddo
	return
	end
