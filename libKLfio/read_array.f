	subroutine read_array (fnm, za, nx, ny, xoff, yoff)
	implicit none
c
c  Read a list of z values to the array 'za'.
c
	character*(*) fnm
	integer nx, ny, xoff, yoff
	real za(nx,ny),ajunk(nx+xoff,ny),junk
	integer i,j

c  Initialize
	do 1 j=1,ny,1
	do 1 i=1,nx,1
1	  za(i,j) = 0

c  Open the file
	open(10, file=fnm, status='old', err=91, access='sequential',
     &		form='formatted')

c  Read the file, get the z values
	do 20 j=1,ny+yoff,1
	  if (j.gt.yoff) then
	    read(10,*,err=92,end=25) (ajunk(i,j-yoff), i=1,nx+xoff,1)
	  else
	    read(10,*,err=92,end=25) junk
	  end if
20	continue
25	continue

c  Close file and return
	close(unit=10, err=93)

c  Get rid of front columns and return
	do 30 j=1,ny,1
	do 30 i=1,nx,1
30	  za(i,j) = ajunk(i+xoff,j)
	return

c  Error messages
91	write(*,*) 'ERROR: Unable to open file ',fnm(1:index(fnm,' '))
	write(*,*) 'Aborting...'
	stop
92	write(*,*) 'ERROR: Unable to read file ',fnm(1:index(fnm,' '))
	write(*,*) 'Aborting...'
	stop
93	write(*,*) 'ERROR: Unable to close file ',fnm(1:index(fnm,' '))
	write(*,*) 'Continuing'
	return

	return
	end
