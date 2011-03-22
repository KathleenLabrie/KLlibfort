c  Find the true length of a string, not the length of the vector

	function length (str)
	character*(*) str
	l=len(str)
	do 1 i=l,1,-1
	length=i
1	if (str(i:i) .ne. ' ') return
	length=0
	return
	end
