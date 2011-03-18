	subroutine lower2upper (str)
c
c In-place translation of lower case to upper case ...
c
	character str*(*), c*1
	byte b
	equivalence (b, c)
	l=length(str)
	do 1 i=1,l
	c=str(i:i)
	if (c .ge. 'a' .and. c .le. 'z') then
	  b=b-32
	  str(i:i)=c
	end if
1	continue
	return
	end
