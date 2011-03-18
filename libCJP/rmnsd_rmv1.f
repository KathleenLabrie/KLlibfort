       SUBROUTINE RMNSD_RMV1 (A,N,TOLSIG,NITERMAX,PRINT,RMEAN,
     &   STDDEV)
C
C Same as RMNSD, except removes outliers iteratively - i.e. takes
c median. Iterates a maximum of NITERMAX times, removing outliers
c beyond TOLSIG*SIGMA each time. This is a new version that doesn't
c need weights (all initial weights are assumed to be unity).
C
       LOGICAL PRINT
       REAL A(1)
       REAL*8 SUM,SUMSQ,SUM0,SUMSQ0
       ITER=1
       SUM0=0.D0
       SUMSQ0=0.D0
       NUSE0=0
       DO 10 I=1,N
          NUSE0=NUSE0+1
          SUM0=SUM0+DBLE(A(I))
          SUMSQ0=SUMSQ0+DBLE(A(I))**2
   10  CONTINUE
       RMEAN=SUM0/NUSE0
       if ((sumsq0-(sum0**2)/nuse0) .le. 0.d0) then
	  stddev=0.
       else
          STDDEV=DSQRT((SUMSQ0-(SUM0**2)/NUSE0)/(NUSE0-1))
       end if
       IF(PRINT)WRITE(6,3)ITER,RMEAN,STDDEV,NUSE0
       IF(PRINT)WRITE(7,3)ITER,RMEAN,STDDEV,NUSE0
    3  FORMAT(5X,'ITER #',I3,':  MEAN =',F10.3,5X,'SIGMA =',
     &   F8.4,'  (',I6,' POINTS)')
       NUSE=NUSE0
       DO 2 ITER=2,NITERMAX
       NUSEOLD=NUSE
       NUSE=NUSE0
       SUM=SUM0
       SUMSQ=SUMSQ0
       RLO=RMEAN-TOLSIG*STDDEV
       RHI=RMEAN+TOLSIG*STDDEV
       DO 1 I=1,N
       IF(A(I).LT.RLO.OR.A(I).GT.RHI)THEN
         NUSE=NUSE-1
         SUM=SUM-DBLE(A(I))
         SUMSQ=SUMSQ-DBLE(A(I))**2
       END IF
    1  CONTINUE
       if (nuse .le. 1) return
       RMEAN=SUM/NUSE
       if ((sumsq-(sum**2)/nuse) .le. 0.d0) then
	  stddev=0.
       else
          STDDEV=DSQRT((SUMSQ-(SUM**2)/NUSE)/(NUSE-1))
       end if
99     IF(PRINT)WRITE(6,3)ITER,RMEAN,STDDEV,NUSE
       IF(PRINT)WRITE(7,3)ITER,RMEAN,STDDEV,NUSE
       IF(NUSE.EQ.NUSEOLD)RETURN
    2  CONTINUE
       RETURN
       END
