	function airmass ( delta, hourangle, latitude)
c
c Airmass calculation - all arguments are in radians!
c
	real latitude
        sinalt=sin(latitude)*sin(delta)+cos(latitude)*cos(delta)*
     &    cos(hourangle)
        airmass=1./sinalt
        return
        end
