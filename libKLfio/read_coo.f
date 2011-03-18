	subroutine read_coo (fnm, x, y, nmax, n)
	implicit none
	character*(*) fnm
	integer nmax, n, i
	real*4 x(nmax), y(nmax)
c
c  Open the file
c
	open(10, file=fnm, status='old', err=91, access='sequential', 
     &		form='formatted')
c
c  Read the file, get the coordinates
c
	do 20 i=1,nmax,1
20	read(10,*,err=92,end=25) x(i), y(i)
25	n=i-1
c
c  Close the file and return
c
	close(unit=10,err=93)
	return

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
