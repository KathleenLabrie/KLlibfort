	program test
	implicit none
	integer i,j,nx,ny,xoff,yoff,k,l,n,xmax,ymax
	parameter (xmax=10,ymax=10)
	real array(xmax,ymax)
	data nx/7/,ny/3/,xoff/0/,yoff/0/
	character fmt*20,str*4
	
	write(str,*) nx
	
	fmt='('//str//'(f10.8,1x))'
	
	call read_array('test.dat',array,nx,ny,xoff,yoff)
	n=nx*ny
	do 1 j=ny,1,-1
	do 1 i=nx,1,-1
	  l=(n-1)/xmax + 1
	  k=n-(l-1)*xmax
	  array(i,j)=array(k,l)
1	n=n-1
c	do 2 j=1,ny,1
c2	write(*,fmt) (array(i,j), i=1,nx,1)
	call write_array('testout.dat',array,nx,ny)
	end
