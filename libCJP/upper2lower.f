	subroutine upper2lower (str)
c
c In-place translation of upper case to lower case ...
c
	character str*(*), c*1
	byte b
	equivalence (b, c)
	l=length(str)
	do 1 i=1,l
	c=str(i:i)
	if (c .ge. 'A' .and. c .le. 'Z') then
	  b=b+32
	  str(i:i)=c
	end if
1	continue
	return
	end
