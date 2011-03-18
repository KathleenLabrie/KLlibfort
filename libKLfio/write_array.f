	subroutine write_array (fnm, za, nx, ny)
	implicit none
c
c  Read a list of z values to the array 'za'.
c
	character*(*) fnm
	character str*4,fmt*20
	integer nx, ny
	real za(nx,ny)
	integer i,j

c  Open the file
	open(10, file=fnm, status='new', err=91, access='sequential',
     &		form='formatted')

c  Write the file (the z values)
	write(str,*) nx
	fmt='('//str//'(f10.8,1x))'
	do 1 j=1,ny,1
1	write(10,fmt) (za(i,j), i=1,nx,1)

c  Close file and return
	close(unit=10, err=93)
	return

c  Error messages
91	write(*,*) 'ERROR: Unable to open file ',fnm(1:index(fnm,' '))
	write(*,*) 'Aborting...'
	stop
92	write(*,*) 'ERROR: Unable to write to file ',fnm(1:index(fnm,' '))
	write(*,*) 'Aborting...'
	stop
93	write(*,*) 'ERROR: Unable to close file ',fnm(1:index(fnm,' '))
	write(*,*) 'Continuing'
	return

	return
	end
