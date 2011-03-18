        subroutine iraf_write_r4 (fname, buff, nx, ny, ier)
c
c Subroutine to create an IRAF image and store a picture in it.
c (Copied from rdimg program for Photometrics CCD data).
c
        character fname*(*), msg*70
        real*4 buff(1)
        integer*4 im, axlen(7)
c
c Store appropriate sizes in array axlen ...
c
        do 1 i=1,7
1       axlen(i)=0
        axlen(1)=nx
        axlen(2)=ny
c
c Create and open image, then write it and close it ...
c
        call imcrea (fname, axlen, 2, 6, ier)
        if (ier .ne. 0) go to 99
        call imopen (fname, 3, im, ier)
        if (ier .ne. 0) go to 99
        call imps2r (im, buff, 1, nx, 1, ny, ier)
        if (ier .ne. 0) go to 99
        call imclos (im, ier)
        if (ier .ne. 0) go to 99
        return
c
c An error occurred somewhere along the way ...
c
99      call imemsg (ier, msg)
        write(6,100) msg
100     format(/5x,'',a,''/)
        return
        end
