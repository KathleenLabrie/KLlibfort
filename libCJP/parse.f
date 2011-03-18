	subroutine parse (str, nval, cval, rval)
c
c A routine to parse a line and return words in BOTH string
c and numeric form. Has been tested. The numeric string holds
c the character equivalent of the first 4 characters if it is
c not actually a numerically valid string
c
c Needs routines ifnbl and length.
c
c Should work for both f77 and g77 compilers.
c
	character cval(1)*(*), str*(*), c4*4, fmt*80
	real rval(1), r4
	equivalence (r4,c4)
c
	nval=0	
	l=length(str)
	i2=0
5	i1=ifnbl(str,i2+1)
	i2=index(str(i1:), ' ')+i1-2
	if (i1 .ge. 1 .and. i1 .le. l .and. i2 .ge. 1 .and. i2 .le. l
     &    .and. i2 .ge. i1) then
	  nval=nval+1
	  cval(nval)=str(i1:i2)
	  lval=i2-i1+1
	  if (lval .gt. 1) then
	    write (fmt,'(2h(f,i2,3h.0))')lval
	    read (cval(nval), fmt, err=11) rval(nval)
ccccc9	    format(f<lval>.0)
	  else
	    read (cval(nval), '(i1)', err=11) ival
	    rval(nval)=ival
	  end if
	  go to 5
11	  c4=cval(nval)(1:4)
	  rval(nval)=r4
	  go to 5
	end if
c
	return
	end
