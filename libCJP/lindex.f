	function lindex (str, str1)
c Like index, except looks for *last* match of str1 with str.
	character str*(*), str1*(*)
	lindex=0
	ls=length(str)
	ls1=length(str1)
	if (ls1 .gt. ls .or. ls .le. 0 .or. ls1 .le. 0) return
	do 1 i=1,ls-ls1+1
1	if (str(i:i+ls1-1) .eq. str1) lindex=i
	return
	end
