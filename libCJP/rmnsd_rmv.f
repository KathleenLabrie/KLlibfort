       SUBROUTINE RMNSD_RMV (A,N,TOLSIG,NITERMAX,PRINT,RMEAN,
     &   STDDEV,WT)
C
C Same as RMNSD, except removes outliers iteratively - i.e. takes
c median. Iterates a maximum of NITERMAX times, removing outliers
c beyond TOLSIG*SIGMA each time. WT is an array of initial weights;
c WT(i)=1 (used in fit) or =0 (not used). 
c 
c Jan 95 - Some changes to use of double precision (DBLE(A(I)) - can
c make a big difference in some cases with small dispersions in moderately
c large numbers!
C
       LOGICAL PRINT
       BYTE WT(1)
       REAL A(1)
       REAL*8 SUM,SUMSQ
       ITER=1
       SUM=0.D0
       SUMSQ=0.D0
       NUSE=0
       DO 10 I=1,N
       IF(WT(I).EQ.1)THEN
          NUSE=NUSE+1
          SUM=SUM+DBLE(A(I))
          SUMSQ=SUMSQ+DBLE(A(I))**2
       END IF
   10  CONTINUE
       RMEAN=SUM/NUSE
       if ((SUMSQ-(SUM**2)/NUSE) .le. 0.d0) then
	  stddev=0.
       else
          STDDEV=DSQRT((SUMSQ-(SUM**2)/NUSE)/(NUSE-1))
       end if
       IF(PRINT)WRITE(6,3)ITER,RMEAN,STDDEV,NUSE
       IF(PRINT)WRITE(7,3)ITER,RMEAN,STDDEV,NUSE
    3  FORMAT(5X,'ITER #',I3,':  MEAN =',F10.3,5X,'SIGMA =',
     &   F8.4,'  (',I6,' POINTS)')
       DO 2 ITER=2,NITERMAX
       NUSEOLD=NUSE
       RLO=RMEAN-TOLSIG*STDDEV
       RHI=RMEAN+TOLSIG*STDDEV
       DO 1 I=1,N
       IF(WT(I).EQ.0)GO TO 1
       IF(A(I).LT.RLO.OR.A(I).GT.RHI)THEN
         NUSE=NUSE-1
         SUM=SUM-DBLE(A(I))
         SUMSQ=SUMSQ-DBLE(A(I))**2
         WT(I)=0
       END IF
    1  CONTINUE
       RMEAN=SUM/NUSE
       if (nuse .le. 1) return
       if ((SUMSQ-(SUM**2)/NUSE) .le. 0.d0) then
	  stddev=0.
       else
          STDDEV=DSQRT((SUMSQ-(SUM**2)/NUSE)/(NUSE-1))
       end if
       IF(PRINT)WRITE(6,3)ITER,RMEAN,STDDEV,NUSE
       IF(PRINT)WRITE(7,3)ITER,RMEAN,STDDEV,NUSE
       IF(NUSE.EQ.NUSEOLD)RETURN
    2  CONTINUE
       RETURN
       END
