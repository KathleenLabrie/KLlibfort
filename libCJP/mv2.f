	subroutine mv2 (ia, ib, n)
	integer*2 ia(1), ib(1)
	do 1 i=1,n
1	ib(i)=ia(i)
	return
	end
