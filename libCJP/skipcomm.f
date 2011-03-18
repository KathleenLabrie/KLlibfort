	subroutine skipcomm (lun, lunout)
c
c Skip over comment lines in logical unit 'lun'. If 'lunout' > 0
c then copy to lunout.
c
	character line*200
	nskip=0
	do 1 i=1,999999
	read (lun, '(a)') line
	if (line(1:1) .ne. '#' .and. line(1:1) .ne. '!') go to 5
	if (lunout .gt. 0 .and. lunout .le. 99) write(lunout,'(a)')
     &    line(1:length(line))
1	nskip=nskip+1
5	backspace lun
c	write(6,'(a,i4,a)') 'Skipped over ', nskip,
c     &   ' comment lines at beginning of file.'
	return
	end
