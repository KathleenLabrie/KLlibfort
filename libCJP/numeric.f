	logical function numeric (str)
c
c Tests whether a line contains only numeric data (01234567890.+-EeDd,
c or tab or space). But, if the line contains *only* tabs or spaces,
c it is considered non-numeric.
c
	character str*(*)
	logical empty
	l=length(str)
	numeric=.false.
	empty=.true.
	do 1 i=1,l
	ich=ichar(str(i:i))
c Check whether there's something other than a space or tab ...
	if (ich .ne. 32 .and. ich .ne. 9) empty=.false.
c numbers 0-9
	if (.not.( (ich .ge. 48 .and. ich .le. 57) .or.
c + or -
     &    str(i:i) .eq. '+' .or. str(i:i) .eq. '-' .or.
c D,d,E,e ...
     &    str(i:i) .eq. 'E' .or. str(i:i) .eq. 'e' .or.
     &    str(i:i) .eq. 'D' .or. str(i:i) .eq. 'd' .or.
c tab or space or decimal ...
     &    ich .eq. 9 .or. ich .eq. 32 .or. str(i:i) .eq. '.' ) ) 
     &    return
1	continue
	if (empty) return
	numeric=.true.
	return
	end
