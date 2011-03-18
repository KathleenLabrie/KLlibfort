        subroutine iraf_read_r4 (fname0, buff, nmax, nx, ny, ixoff,
     &    iyoff, ier)
c
c Subroutine to read an IRAF image. Handles image sections in fname.
c [But doesn't handle "*" specifications in image sections yet!]
c
        character fname0*(*), fname*80, msg*70
        real*4 buff(1)
        integer*4 im, nx, ny, ixysec(4)
c
c Open image and find its size (use only portion of name before
c image section specification - stripped off by 'imagesection').
c
	fname=fname0
	call imagesection (fname, ixysec(1), ixysec(2), ixysec(3),
     &    ixysec(4), ierimgsec)
        call imopen (fname, 1, im, ier)
        if (ier .ne. 0) go to 99
	call imgkwi (im, 'naxis1', nx0, ier)
        if (ier .ne. 0) go to 99
	call imgkwi (im, 'naxis2', ny0, ier)
        if (ier .ne. 0) go to 99
ccc	write(6,'(a,a)')'       Image: ',fname(1:length(fname))
ccc	write(6,50)'   Full size:',nx0,ny0
c
c Process image section ...
c
	if (ierimgsec .eq. 1) then
	  ixysec(1)=1
	  ixysec(2)=nx0
	  ixysec(3)=1
	  ixysec(4)=ny0
	else if (ierimgsec .eq. -1 .or. ixysec(1) .lt. 1 .or. ixysec(1)  
     &    .gt. nx0 .or. ixysec(2) .lt. 1 .or. ixysec(2) .gt. nx0 .or.
     &    ixysec(3) .lt. 1 .or. ixysec(3) .gt. ny0 .or. ixysec(4)
     &    .lt. 1 .or. ixysec(4) .gt. ny0) then
	  go to 97
	end if
	nx=ixysec(2)-ixysec(1)+1
	ny=ixysec(4)-ixysec(3)+1
	ixoff=ixysec(1)-1
	iyoff=ixysec(3)-1
ccc	write(6,50)'     Section:',nx,ny,'   Corner:',
ccc     &    ixysec(1),ixysec(3)
50	format(a,i5,' x',i5,a,'(',i4,',',i4,')')
c
c Check that image is not too big ...
c
	ntot=nx*ny
	if (ntot .gt. nmax) go to 98
c
c Read image and close it ...
c
        call imgs2r (im, buff, ixysec(1), ixysec(2), ixysec(3), 
     &    ixysec(4), ier)
        if (ier .ne. 0) go to 99
        call imclos (im, ier)
        if (ier .ne. 0) go to 99
        return
c
c An error occurred somewhere along the way ...
c
97	write(6,100)'Error: invalid image section!'
	ier=-9998
	return
98	write (6,100)'Error: picture is too big!'
	ier=-9999
	return
99      call imemsg (ier, msg)
        write(6,100) msg
100     format(/5x,'',a,''/)
        return
        end
