	subroutine fits_write_r4 (fname, buff, nx, ny, ier)
c
c Subroutine to create a FITS image.  (Mirrors iraf_write_r4)
c
	character fname*(*), msg*70
	real*4 buff(1)
	integer*4 im, axlen(7)
c
c St
