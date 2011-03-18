	function str2real (str)
c Converts a string into a real number ...
	character*(*) str, fmt*10, num*2
	ll=length(str)
	write(num,'(i2)')ll
	if (ll .le. 9) fmt='(f'//num(2:2)//'.0)'
	if (ll .gt. 9) fmt='(f'//num(1:2)//'.0)'
	read (str,fmt,err=98) str2real
	return
98	write(6,*)'Warning: string cannot be converted to real,',
     &    'set to zero.'
	str2real=0.
	return
	end
