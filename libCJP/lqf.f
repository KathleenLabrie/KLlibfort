      SUBROUTINE LQF  (X,Y,YF,W,E1,E2,P,WZ,N,M,NI,ND,EP,AUX) 
      DIMENSION X(N),Y(N),YF(1),P(50),E1(50),E2(50),W(1)
      DIMENSION C(1275),V(50),D(50),CU(50,50),VV(50,1) 
      LOGICAL SWITCH
      EQUIVALENCE (V(1),VV(1,1)) 
      external aux
      data lun/7/
      IF (N .LE.M) GO TO 200
      SWITCH=.FALSE.
      IF(NI.LT.0) SWITCH=.TRUE.
      NII=IABS(NI)
      ND=1 
      IF(SWITCH) GO TO 1000 
      WRITE(lun,71)
1000   NT=1 
      IV=0 
5     IJ=0 
      DO 10 I=1,M 
      V(I) = 0.0 
      DO 10 J=1,I 
      IJ=IJ + 1 
10    C(IJ) = 0.0 
      XX=0. 
      TT=0. 
      DO 20 L=1,N 
      IF(WZ) 6,7,6 
6     WT=W(L) 
22    GO TO 8 
7     WT=1. 
8     U=AUX(P,D,X(L),L) 
      XX=XX+WT*(U-Y(L))*(U-Y(L)) 
      IJ = 0 
      DO 30 J=1,M 
      DO 30 I=J,M 
      IJ = IJ + 1 
30    C(IJ) = C(IJ) + WT*D(I)*D(J) 
      DO 40 I=1,M 
40    V(I)=V(I)+WT*(Y(L)-U)*D(I) 
20    CONTINUE 
      IF(SWITCH) GO TO 1001
      WRITE(lun,3)(P(I),I=1,M),XX 
1001   IF(IV.EQ.1) GO TO 45 
      IF(NT-NII) 35,45,55 
35    CALL SSOLMT(C,VV,1,IJ,M,KEY) 
      IF (KEY .EQ. 1) GO TO 65 
      DO 75  I=1,M 
      P(I)=P(I)+V(I) 
      TC=ABS(V(I)/P(I)) 
      IF(TC.GT.TT) TT=TC 
75    CONTINUE 
      NT=NT+1 
      IF(TT.LT.EP) IV=1 
      GO TO 5 
45    DO 46 I=1,M 
      DO 46 J=1,M 
46    CU(I,J) = 0.0 
      DO 47 I=1,M 
47    CU(I,I) = 1.0 
      CALL SSOLMT(C,CU,M,IJ,M,KEY) 
      IF (KEY .EQ. 1) GO TO 65 
      DO 85 I=1,M 
      DO 85 J=1,M 
85    P(I) = P(I) + CU(I,J)*V(J) 
55    DO 95 I=1,M 
95    E1(I) = SQRT(CU(I,I)) 
      IF (SWITCH) GO TO 1002
      WRITE(lun,72) NT 
      WRITE(lun,3)(P(I),I=1,M) 
3     FORMAT(1X,8G15.5,G10.3) 
      WRITE(lun,3) 
1002  S=0.0 
      DO 105 L=1,N 
      IF(WZ) 16,17,16 
16    WT=W(L) 
      GO TO 18 
17    WT=1. 
18    YF(L)=AUX(P,D,X(L), L) 
      XX=(Y(L)-YF(L))**2 
      S=XX*WT + S 
105   CONTINUE 
      PP=N-M 
      FI=SQRT(S/PP) 
      DO 115 I=1,M 
115   E2(I)=FI  *E1(I) 
      IF(SWITCH) GO TO 1003
      WRITE (lun,73) S 
1003  IF (IV .NE. 1 .AND. NII .NE.1) ND=-1
      RETURN
65    WRITE(6,2) 
      if (lun .ne. 6) WRITE(lun,2)
2     FORMAT(22H LINEAR EQUATIONS FAIL) 
      ND=0 
      RETURN 
71    FORMAT(//53H INTERMEDIATE ESTIMATES OF PARAMETERS, SUM OF SQUARES)
72    FORMAT(/30H FINAL ESTIMATES OF PARAMETERS,35X,17HNO OF ITERATIONS= 
     1 ,I5) 
73    FORMAT (1H0,14HSUM OF SQUARES,G15.5) 
200   WRITE(6,210)
210   FORMAT('THE NUMBER OF DATA POINTS MUST EXCEED THE NUMBER OF' 
     1, ' PARAMETERS')
      ND=0
      RETURN
      END 


      SUBROUTINE SSOLMT(A,B,L,M,N,KEY) 
      DIMENSION A(M), B(50,L) 
C 
C 
      IF (A(1).LE.0.) GO TO 150 
      IF (M .EQ. 1) GO TO 160 
            A(1) = 1.0/SQRT(A(1)) 
      DO 10 I=2,N 
10          A(I) = A(I)*A(1) 
C 
C 
            INC = N 
            I1  = 1 
            IN  = N 
            NM1 = N - 1 
C 
20          INC = INC - 1 
            I1 = IN + 1 
            IN = IN + INC 
            NS = N - INC 
C 
            X = 0. 
            ISUB =I1 
            DO 30 I=INC,NM1 
           ISUB = ISUB - I 
30          X = X + A(ISUB)*A(ISUB) 
      IF (A(I1) -X .LT. 0.) GO TO 150 
            A(I1) = SQRT(A(I1) - X) 
C 
C 
      IF (A(I1).EQ. 0.) GO TO 150 
            A(I1) = 1./A(I1) 
      IF (INC .EQ. 1) GO TO 90 
C 
            I11 =I1 + 1 
            L11 = I1 - INC 
      DO 50 I=I11,IN 
            X = 0. 
            L1 = L11 
               L2 = I - INC 
      DO 40 J =1,NS 
            X = X + A(L1)*A(L2) 
            L1 = L1 - INC - J 
40          L2 = L2 - INC - J 
50          A(I) =(A(I) - X)*A(I1) 
      GO TO 20 
90    DO 130 K=1,L 
            B(1,K) = B(1,K)*A(1) 
      DO 110 I=2,N 
            JM = I-1 
            ISUB = I 
            INC = N 
            X = 0. 
      DO 100 J=1,JM 
            X = A(ISUB)*B(J,K) + X 
            INC = INC - 1 
100         ISUB = ISUB + INC 
110         B(I,K) = (B(I,K) - X)*A(ISUB) 
C 
            B(N,K) = B(N,K)*A(M) 
            INC = -1 
            J1  = M+1 
      DO 125 I=2,N 
            INC = INC + 1 
            JM = J1 - 2 
            J1 = JM - INC 
            JSUB = N-INC-1 
            II = JSUB 
            X = 0. 
      DO 120 J=J1,JM 
            JSUB = JSUB + 1 
120         X = X + A(J)*B(JSUB,K) 
125         B(II,K) = (B(II,K) - X)*A(J1-1) 
130         CONTINUE 
            KEY  = 0 
            RETURN 
150         KEY = 1 
            RETURN 
160   B(1,1)=B(1,1)/A(1) 
      KEY=0 
      RETURN 
            END 
