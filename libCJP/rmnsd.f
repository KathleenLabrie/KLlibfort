       subroutine rmnsd (x, n, rmn, rsd)
c
c Calculate mean, standard deviation of an array of numbers ...
c
       real x(1)
       s=0.
       ss=0.
       do 1 i=1,n
       s=s+x(i)
    1  ss=ss+x(i)**2
       rmn=s/n
       rsd=sqrt(ss/(n-1)-(s**2)/(n*(n-1)))
       return
       end
