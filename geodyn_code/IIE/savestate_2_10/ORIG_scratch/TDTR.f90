!$TDTR
      SUBROUTINE TDTR(ROT1,ROT2,ROT3,ROT4,ROT5,ROT6,ROT7,ROT8,ROT9,     &
     &                X1,X2,WORK,NM,NDIM,LTDTR)
!********1*********2*********3*********4*********5*********6*********7**
! TDTR             87/09/30            8710.0    PGMR - D. ROWLANDS
!
!
! FUNCTION:  TRANFORM TRUE OF DATE EARTH EQUATOR & EQUINOX
!            (TDEEES) SYSTEM TO TRUE OF REF SATELLTE SYSTEM
!            (TRSS). THE TRSS MAY BE THE IAU SYSTEM IF THE
!            CENTER OF INTEGRATION IS NOT THE EARTH). THIS
!            ROUTINE CAN GO THE OTHER DIRECTION.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ROTN     I         THE NTH ELEMENT OF THE ROTATION MATRIX
!                      THAT TRANSFORMS FROM TDEEES TO MEAN OF
!                      2000 (50).
!                      POINT
!   X1       I         TDEEES COORDINATES OR TRSS COORDINATES
!   X2       O         TRSS COORDINATES OR TDEEES COORDINATES
!   WORK          A    NEEDS TO BE DIMENSIONED NM BY 3
!   NM       I         NUMBER OF LIGHT TIMES TO BE COMPUTED
!   NDIM     I         FIRST DIMENSION OF X1 & X2
!   LTDTR    I         .TRUE. IF GOING FOROM TDEEES TO TRSS
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CREFMT/REFMT(9)
      DIMENSION X1(NDIM,3),X2(NDIM,3),WORK(NM,3)
      DIMENSION ROT1(NM),ROT2(NM),ROT3(NM),ROT4(NM),ROT5(NM),ROT6(NM),  &
     &          ROT7(NM),ROT8(NM),ROT9(NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!
      IF(.NOT.LTDTR) GO TO 500
!
!  TRANSFORM FROM TRUE OF DATE TO MEAN OF 2000 (50)
      DO 100 N=1,NM
      WORK(N,1)=ROT1(N)*X1(N,1)+ROT2(N)*X1(N,2)+ROT3(N)*X1(N,3)
      WORK(N,2)=ROT4(N)*X1(N,1)+ROT5(N)*X1(N,2)+ROT6(N)*X1(N,3)
      WORK(N,3)=ROT7(N)*X1(N,1)+ROT8(N)*X1(N,2)+ROT9(N)*X1(N,3)
  100 END DO
!
!  TRANSFORM FROM MEAN OF 2000 (50) TO TRUE OF REF
      DO 200 N=1,NM
      X2(N,1)=REFMT(1)*WORK(N,1)+REFMT(2)*WORK(N,2)                     &
     &       +REFMT(3)*WORK(N,3)
      X2(N,2)=REFMT(4)*WORK(N,1)+REFMT(5)*WORK(N,2)                     &
     &       +REFMT(6)*WORK(N,3)
      X2(N,3)=REFMT(7)*WORK(N,1)+REFMT(8)*WORK(N,2)                     &
     &       +REFMT(9)*WORK(N,3)
  200 END DO
      RETURN
  500 CONTINUE
!
!  TRANSFORM FROM TRUE OF REF TO MEAN OF 2000 (50)
      DO 600 N=1,NM
      WORK(N,1)=REFMT(1)*X1(N,1)+REFMT(4)*X1(N,2)+REFMT(7)*X1(N,3)
      WORK(N,2)=REFMT(2)*X1(N,1)+REFMT(5)*X1(N,2)+REFMT(8)*X1(N,3)
      WORK(N,3)=REFMT(3)*X1(N,1)+REFMT(6)*X1(N,2)+REFMT(9)*X1(N,3)
  600 END DO
!
!  TRANSFORM FROM MEAN OF 2000 (50) TO TRUE OF DATE
      DO 700 N=1,NM
      X2(N,1)=ROT1(N)*WORK(N,1)+ROT4(N)*WORK(N,2)+ROT7(N)*WORK(N,3)
      X2(N,2)=ROT2(N)*WORK(N,1)+ROT5(N)*WORK(N,2)+ROT8(N)*WORK(N,3)
      X2(N,3)=ROT3(N)*WORK(N,1)+ROT6(N)*WORK(N,2)+ROT9(N)*WORK(N,3)
  700 END DO
      RETURN
      END
