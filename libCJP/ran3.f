      FUNCTION RAN3(IDUM)
c
c From Numerical Recipes. RAN3 returns a random # between 0 and 1.
c Set IDUM to any negative value to initialize or re-initialize.
c (Use GET_SEED3.)
c
c > This assumes that variables are saved between function calls!
c          
      PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1.E-9)                
      DIMENSION MA(55)                                                          
      COMMON /RANDOM3/ IFF, INEXT, INEXTP, MA                                 
      DATA IFF /0/
      IF(IDUM.LT.0.OR.IFF.EQ.0)THEN                                             
        IFF=1                                                                   
        MJ=MSEED-IABS(IDUM)                                                     
        MJ=MOD(MJ,MBIG)                                                         
        MA(55)=MJ                                                               
        MK=1                                                                    
        DO 11 I=1,54                                                            
          II=MOD(21*I,55)                                                       
          MA(II)=MK                                                             
          MK=MJ-MK                                                              
          IF(MK.LT.MZ)MK=MK+MBIG                                                
          MJ=MA(II)                                                             
11      CONTINUE                                                                
        DO 13 K=1,4                                                             
          DO 12 I=1,55                                                          
            MA(I)=MA(I)-MA(1+MOD(I+30,55))                                      
            IF(MA(I).LT.MZ)MA(I)=MA(I)+MBIG                                     
12        CONTINUE                                                              
13      CONTINUE                                                                
        INEXT=0                                                                 
        INEXTP=31                                                               
        IDUM=1                                                                  
      ENDIF                                                                     
      INEXT=INEXT+1                                                             
      IF(INEXT.EQ.56)INEXT=1                                                    
      INEXTP=INEXTP+1                                                           
      IF(INEXTP.EQ.56)INEXTP=1                                                  
      MJ=MA(INEXT)-MA(INEXTP)                                                   
      IF(MJ.LT.MZ)MJ=MJ+MBIG                                                    
      MA(INEXT)=MJ                                                              
      RAN3=MJ*FAC                                                               
      RETURN                                                                    
      END                                                                       
