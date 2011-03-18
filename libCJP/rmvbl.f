	subroutine rmvbl (str)
c
c Remove blanks in string 'str'. Done in place.
c
	character*(*) str
9	l=length(str)
	ibl=index(str,' ')
	if (ibl .gt. l) return
	inbl=ifnbl(str,ibl)
	if (ibl .gt. 1) then
	  str=str(1:ibl-1)//str(inbl:)
	else
	  str=str(inbl:)
	end if
	go to 9
	end
