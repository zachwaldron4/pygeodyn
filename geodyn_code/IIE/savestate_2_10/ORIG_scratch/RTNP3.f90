!$RTNP3
      SUBROUTINE RTNP3(ROT1,ROT2,ROT3,ROT4,ROT5,ROT6,ROT7,ROT8,ROT9,    &
     &                 X1,X2,NM,LTDTR)
!***********************************************************************
!
! VERSION           - 9106.0  DATE- 08.09.91     LUCIA TSAOUSSI
!
! FUNCTION          - TRANFORM TRUE OF DATE EARTH EQUATOR & EQUINOX
!                     (TDEEES) SYSTEM TO TRUE OF REF SATELLTE SYSTEM
!                     (TRSS). THE TRSS MAY BE THE IAU SYSTEM IF THE
!                     CENTER OF INTEGRATION IS NOT THE EARTH). THIS
!                     ROUTINE CAN GO THE OTHER DIRECTION.
!
! INPUT PARAMETERS  - ROTN= THE NTH ELEMENT OF THE ROTATION MATRIX
!                      P.N THAT TRANSFORMS FROM TDEEES TO J2000.0
!                            (OR B1950.0 )
!                     X1= TDEEES COORDINATES OR TRSS COORDINATES
!                     NM=NUMBER OF OBSERVATION  TIMES TO BE COMPUTED
!                     LTDTR=.TRUE. IF GOING FOROM TDEEES TO TRSS
!
! OUTPUT PARAMETERS - X2=TRSS COORDINATES OR TDEEES COORDINATES
!
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      COMMON/CREFMT/REFMT(9)
      DIMENSION X1(3),X2(NM,3)
      DIMENSION ROT1(NM),ROT2(NM),ROT3(NM),ROT4(NM),ROT5(NM),ROT6(NM),  &
     &          ROT7(NM),ROT8(NM),ROT9(NM)
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      IF(.NOT.LTDTR) GO TO 500
!
!  TRANSFORM FROM TRUE OF DATE TO MEAN OF J2000.0
      DO 100 N=1,NM
      X2(N,1)=ROT1(N)*X1(1)+ROT2(N)*X1(2)+ROT3(N)*X1(3)
      X2(N,2)=ROT4(N)*X1(1)+ROT5(N)*X1(2)+ROT6(N)*X1(3)
      X2(N,3)=ROT7(N)*X1(1)+ROT8(N)*X1(2)+ROT9(N)*X1(3)
  100 END DO
!
      RETURN
  500 CONTINUE
!
!  TRANSFORM FROM J2000.0 (B1950.0) TO TRUE OF DATE
      DO 700 N=1,NM
      X2(N,1)=ROT1(N)*X1(1)+ROT4(N)*X1(2)+ROT7(N)*X1(3)
      X2(N,2)=ROT2(N)*X1(1)+ROT5(N)*X1(2)+ROT8(N)*X1(3)
      X2(N,3)=ROT3(N)*X1(1)+ROT6(N)*X1(2)+ROT9(N)*X1(3)
  700 END DO
      RETURN
      END
