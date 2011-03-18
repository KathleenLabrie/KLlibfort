       function erf(x)
c
c Error function - Abramowitz and Stegun p.299 (7.1.26).
c
       double precision a(5),t,p,sum,tpower
       data a/.254829592,-.284496736,1.421413741,-1.453152027,
     &   1.061405429/,p/.3275911/
       xabs=abs(x)
       t=1.d0/(1.d0+p*xabs)
       tpower=1.d0
       sum=0.d0
       do 1 i=1,5
       tpower=tpower*t
    1  sum=sum+a(i)*tpower
       erf=1.-sum*exp(-amin1(88.,x**2))
       if(x.lt.0.)erf=-erf
       return
       end
