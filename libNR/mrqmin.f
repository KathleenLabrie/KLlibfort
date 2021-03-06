      SUBROUTINE MRQMIN(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,                          
     *    COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)                                   
      PARAMETER (MMAX=20)                                                       
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),A(MA),LISTA(MA),                   
     *  COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRY(MMAX),BETA(MMAX),DA(MMAX)            
      IF(ALAMDA.LT.0.)THEN                                                      
        KK=MFIT+1                                                               
        DO 12 J=1,MA                                                            
          IHIT=0                                                                
          DO 11 K=1,MFIT                                                        
            IF(LISTA(K).EQ.J)IHIT=IHIT+1                                        
11        CONTINUE                                                              
          IF (IHIT.EQ.0) THEN                                                   
            LISTA(KK)=J                                                         
            KK=KK+1                                                             
          ELSE IF (IHIT.GT.1) THEN                                              
            PAUSE 'Improper permutation in LISTA'                               
          ENDIF                                                                 
12      CONTINUE                                                                
        IF (KK.NE.(MA+1)) PAUSE 'Improper permutation in LISTA'                 
        ALAMDA=0.001                                                            
        CALL MRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NCA,CHISQ,F        
     *UNCS)                                                                     
        OCHISQ=CHISQ                                                            
        DO 13 J=1,MA                                                            
          ATRY(J)=A(J)                                                          
13      CONTINUE                                                                
      ENDIF                                                                     
      DO 15 J=1,MFIT                                                            
        DO 14 K=1,MFIT                                                          
          COVAR(J,K)=ALPHA(J,K)                                                 
14      CONTINUE                                                                
        COVAR(J,J)=ALPHA(J,J)*(1.+ALAMDA)                                       
        DA(J)=BETA(J)                                                           
15    CONTINUE                                                                  
      CALL GAUSSJ(COVAR,MFIT,NCA,DA,1,1)                                        
      IF(ALAMDA.EQ.0.)THEN                                                      
        CALL COVSRT(COVAR,NCA,MA,LISTA,MFIT)                                    
        RETURN                                                                  
      ENDIF                                                                     
      DO 16 J=1,MFIT                                                            
        ATRY(LISTA(J))=A(LISTA(J))+DA(J)                                        
16    CONTINUE                                                                  
      CALL MRQCOF(X,Y,SIG,NDATA,ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,CHISQ,FU        
     *NCS)                                                                      
      IF(CHISQ.LT.OCHISQ)THEN                                                   
        ALAMDA=0.1*ALAMDA                                                       
        OCHISQ=CHISQ                                                            
        DO 18 J=1,MFIT                                                          
          DO 17 K=1,MFIT                                                        
            ALPHA(J,K)=COVAR(J,K)                                               
17        CONTINUE                                                              
          BETA(J)=DA(J)                                                         
          A(LISTA(J))=ATRY(LISTA(J))                                            
18      CONTINUE                                                                
      ELSE                                                                      
        ALAMDA=10.*ALAMDA                                                       
        CHISQ=OCHISQ                                                            
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
