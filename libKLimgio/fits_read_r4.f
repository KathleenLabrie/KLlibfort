	subroutine fits_read_r4 (fname0, buff, nmax, nx, ny, ixoff,
     &	iyoff, ier)
c
c Subroutine to read a FITS image.  Handles image sections in fname.
c [But doesn't handle "*" specifications in image sections yet!]
c
	character fname0*(*), fname*80, msg*70
	real*4 buff(1)
	integer*4 nx, ny, ixysec(4)
	integer*4 status
	integer*4 cfitsread
c
c Separate image name and image section
c
	fname=fname0
	call imagesection (fname, ixysec(1), ixysec(2), ixysec(3),
     &		ixysec(4), ierimgsec)
ccc	write(6,'(a,a)')'	Image: ',fname(1:length(fname))

c Error in imagesection
       if (ierimgsec .eq. -1) then
	  go to 97
	endif
c
c Call the C function 'cfitsread' to open and read the image [section]
c
	status = cfitsread (fname, length(fname), ixysec, ierimgsec, buff, 
     &		nmax, nx, ny, ier)
	if (ier < 0) then
		goto 99
	endif
	ixoff=ixysec(1)-1
	iyoff=ixysec(3)-1

ccc	write(6,50)'     Section:',nx,ny,'   Corner:',
ccc  & 	ixysec(1),ixysec(3)
50	format(a,i5,' x',i5,a,'(',i4,',',i4,')')

c
c We're done here
c
	return
c
c An error occurred somewhere along the way ...
c
97	write(6,100)'Error: invalid image section!'
	ier=-9998
	return

99	return
100	format(/5x,'',a,''/)
	return
	end
