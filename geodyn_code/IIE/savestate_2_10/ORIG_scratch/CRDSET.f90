      SUBROUTINE CRDSET(IDIR,HOLD)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   PURPOSE: HOLD THE COMMOM BLOCKS HAVING TO DO WITH GRAVITY
!            COMPUTATIONS IN STORAGE OR RELOAD THESE
!            BLOCKS FROM STORAGE. THIS HOLDING IS BEING DONE
!            DURING ANY GRAVITY COMPUTATIONS FOR A SECONDARY
!            ASTEROID.
!
!            ON A CALL TO LOAD THE COMMON BLOCKS (IDIR=1)
!            NMAX,NMAXP1 ABD NP OF THE GEIDEG ARRAY WILL
!            BE SET TO VALUES APPROPRIATE FOR A SECONDARY
!            ASTEROID
!
!            ON A CALL TO RELOAD THE COMMON BLOCKS (IDIR=2)
!            THE CRMB2 COMMON BLOCK WILL BE FILLED FROM THE
!            CRMB COMMON BLOCK BEFORE RGE CRMB BLOCK IS RELOADED.
!            THE CRM2 COMMON BLOCK WILL BE NEEDED BY VEVALC
!
!
!   ARGUMENTS: IDIR  - IF 1 LOAD THE COMMOM BLOCKS INTO STORAGE
!                      IF 2 LOAD THE COMMOM BLOCKS FROM STORAGE
!              HOLD  - STORAGE FOR VOMMON BLOCKS
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!
      COMMON/CEFMAT/EFMAT(9)
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/CRMB2/RMB2(9), rmb02(9)
      COMMON/CRMBI/RMBI(9)
      COMMON/CRMI/RMI(9)
      COMMON/CREFMT/REFMT(9)
      COMMON/CRM5OE/RM5OE(9)
      COMMON/GEODEG/NMAX,NMAXP1,NP,NTOLD,NTOLO,NADJC,NADJS,NADJCS,      &
     &              NPMAX,NXGDEG
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
!
      DIMENSION HOLD(73),IP(9),IL(9)
!
      DATA IP/1,10,12,30,39,48,57,66,69/
      DATA IL/9,2,18,9,9,9,9,3,5/
!
      IF(IDIR.EQ.2) GO TO 500
!
      IC=1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        HOLD(IPQ-1+I)=EFMAT(I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      HOLD(IPQ)=GM
      HOLD(IPQ+1)=AE
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        HOLD(IPQ-1+I)=RMB(I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        HOLD(IPQ-1+I)=RMBI(I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        HOLD(IPQ-1+I)=RMI(I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        HOLD(IPQ-1+I)=REFMT(I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        HOLD(IPQ-1+I)=RM5OE(I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      HOLD(IPQ)=DBLE(NMAX)
      HOLD(IPQ+1)=DBLE(NMAXP1)
      HOLD(IPQ+2)=DBLE(NP)
      ITEST=NMAX
      NMAX=6
      IF(ITEST.LT.NMAX) NMAX=ITEST
      NMAXP1=NMAX+1
      NP=NMAX*NMAXP1/2+3*NMAX
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      HOLD(IPQ)=SINPSI
      HOLD(IPQ+1)=COSPSI
      HOLD(IPQ+2)=XLAMDA
      HOLD(IPQ+3)=GMR
      HOLD(IPQ+4)=AOR
      RETURN
 500  CONTINUE
!!
!!
!
      DO I=1,18
       RMB2(I)=RMB(I)
      ENDDO
!
      IC=1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        EFMAT(I)=HOLD(IPQ-1+I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      GM=HOLD(IPQ)
      AE=HOLD(IPQ+1)
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        RMB(I)=HOLD(IPQ-1+I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        RMBI(I)=HOLD(IPQ-1+I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        RMI(I)=HOLD(IPQ-1+I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        REFMT(I)=HOLD(IPQ-1+I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      DO I=1,ILQ
        RM5OE(I)=HOLD(IPQ-1+I)
      ENDDO
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      NMAX=(HOLD(IPQ)+.001D0)
      NMAXP1=(HOLD(IPQ+1)+.001D0)
      NP=(HOLD(IPQ+2)+.001D0)
!
      IC=IC+1
      ILQ=IL(IC)
      IPQ=IP(IC)
      SINPSI=HOLD(IPQ)
      COSPSI=HOLD(IPQ+1)
      XLAMDA=HOLD(IPQ+2)
      GMR=HOLD(IPQ+3)
      AOR=HOLD(IPQ+4)
!
      RETURN
      END
