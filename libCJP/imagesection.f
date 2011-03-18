	subroutine imagesection (fname, ix1, ix2, iy1, iy2, ier)
c
c Process image section. Doesn't handle '*' yet, gags on blanks. 'fname'
c is altered to be just file name before image section.
c
c   ier=0 --> all OK   
c   ier=1 --> no image section specified 
c   ier=-1 --> ghastly error
c
	character fname*(*)
	integer idelim(5), ixysec(4)
	ier=0
	idelim(1)=index(fname,'[')
	if (idelim(1) .le. 0) go to 99
	idelim(2)=index(fname(idelim(1)+1:),':')+idelim(1)
	idelim(3)=index(fname(idelim(1)+1:),',')+idelim(1)
	idelim(4)=index(fname(idelim(2)+1:),':')+idelim(2)
	idelim(5)=index(fname,']')
	do 10 i=1,5
10	if (idelim(i) .le. 0) go to 97
	do 12 i=2,5
12	if (idelim(i) .le. idelim(i-1)) go to 97
	do 11 i=idelim(1),idelim(5)
11	if (fname(i:i) .eq. ' ') go to 97
	do 1 i=1,4
	ic1=idelim(i)+1
	ic2=idelim(i+1)-1
1	ixysec(i)=nint(str2real(fname(ic1:ic2)))
	ix1=ixysec(1)
	ix2=ixysec(2)
	iy1=ixysec(3)
	iy2=ixysec(4)
	fname=fname(1:idelim(1)-1)
	return
99	ier=1
	return
97	ier=-1
	return
	end
