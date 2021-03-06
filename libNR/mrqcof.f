      SUBROUTINE MRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NALP,          
     *CHISQ,FUNCS)                                                              
      PARAMETER (MMAX=20)                                                       
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),ALPHA(NALP,NALP),BETA(MA),         
     *    DYDA(MMAX),LISTA(MFIT),A(MA)                                          
      DO 12 J=1,MFIT                                                            
        DO 11 K=1,J                                                             
          ALPHA(J,K)=0.                                                         
11      CONTINUE                                                                
        BETA(J)=0.                                                              
12    CONTINUE                                                                  
      CHISQ=0.                                                                  
      DO 15 I=1,NDATA                                                           
        CALL FUNCS(X(I),A,YMOD,DYDA,MA)                                         
        SIG2I=1./(SIG(I)*SIG(I))                                                
        DY=Y(I)-YMOD                                                            
        DO 14 J=1,MFIT                                                          
          WT=DYDA(LISTA(J))*SIG2I                                               
          DO 13 K=1,J                                                           
            ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(LISTA(K))                             
13        CONTINUE                                                              
          BETA(J)=BETA(J)+DY*WT                                                 
14      CONTINUE                                                                
        CHISQ=CHISQ+DY*DY*SIG2I                                                 
15    CONTINUE                                                                  
      DO 17 J=2,MFIT                                                            
        DO 16 K=1,J-1                                                           
          ALPHA(K,J)=ALPHA(J,K)                                                 
16      CONTINUE                                                                
17    CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
