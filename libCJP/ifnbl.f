	function ifnbl (str, i1)
c Find the first non-blank character in a string, starting at
c character i1 ...
	character*(*) str
	l=length(str)
	do 1 i=i1,l
	ifnbl=i
1	if (str(i:i) .ne. ' ') return
	ifnbl=0
	return
	end
