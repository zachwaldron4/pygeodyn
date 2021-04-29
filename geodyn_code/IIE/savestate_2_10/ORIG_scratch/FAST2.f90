      SUBROUTINE FAST2(XPS,XDDS,XDDCM2,VMATA2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  PURPOSE:  COMPUTE THE ACCELERATIONS ON THE SECONDARY ASTEROID THAT
!            ARISE FROM SOURCES OTHER THAN THOSE EXERTED BY THE PRIMARY
!            AASTEROID
!
!  ARGUMENTS  XPS    - (I) J2000 STATE OF SECONDARY ASTEROID WRT PRIMARY
!             XDDS   - (I&O) TOTAL J2000 ACCELERATION OF SECONDARY ASTEROID
!             XDDCM2 - (I&O) J2000 ACCELERATION OF SECONDARY ASTEROID WITHOUT
!                            THE GRAVITATIONAL ACCELERATION FROM THE
!                            ORIMARY
!             VMATA2 - (I&O) J2000 MATRIX OF DERIVRIVES OF ACCELERATION
!                            WRT SECONDARY ASTEROID STATE
!
!
!   NOTES:   (1) THIS ROUTINE ASSUMES SUBROUTINE F HAS BEEN CALLED JUST
!                JUST PRIOR TO THE CALL TO THIS ROUTINE
!            (2) TORQUES FOR SECOND ASTEROID COMPUTED IN FASTIN
!            (3) TORQUES FOR SECOND ASTEROID APPLIE IN A SECOND
!                CALL TO FAST
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      COMMON/CRMI/RMI(9)
      COMMON/SETOPT/LSDRG,LSSRD,LSGA,LSORB,LTHDRG,LROCK4
      DIMENSION XPS(3,2),XDDS(3),VMATA2(3,6),XDDCM2(3)
      DIMENSION XDDCMT(3)
      DIMENSION VMAT(3,3),HOLD(3,3)
      DIMENSION XPSTD(3,3),XDD(3)

!
!  SUNGRV COORDINATE SYSTEM IS IN PRIMARY ASTEROID TOD
!
!
      XPSTD(1,1)=RMI(1)*XPS(1,1)+RMI(4)*XPS(2,1)+RMI(7)*XPS(3,1)
      XPSTD(2,1)=RMI(2)*XPS(1,1)+RMI(5)*XPS(2,1)+RMI(8)*XPS(3,1)
      XPSTD(3,1)=RMI(3)*XPS(1,1)+RMI(6)*XPS(2,1)+RMI(9)*XPS(3,1)
!
      DO I=1,9
         VMAT(I,1)=0.D0
         VMATA2(I+9,1)=0.D0
      ENDDO
      C20=0.D0
!
      CALL SUNGRV(2,1,XPSTD,C20,LSORB,XDDCMT,VMAT)
!
! NEED TO ROTATE XDDCMT

!
      XDDCM2(1)=RMI(1)*XDDCMT(1)+RMI(2)*XDDCMT(2)+RMI(3)*XDDCMT(3)
      XDDCM2(2)=RMI(4)*XDDCMT(1)+RMI(5)*XDDCMT(2)+RMI(6)*XDDCMT(3)
      XDDCM2(3)=RMI(7)*XDDCMT(1)+RMI(8)*XDDCMT(2)+RMI(9)*XDDCMT(3)
!
!
      XDDS(1)=XDDS(1)+XDDCM2(1)
      XDDS(2)=XDDS(2)+XDDCM2(2)
      XDDS(3)=XDDS(3)+XDDCM2(3)
!
!   TRANSFORM VMATRX INTO TRUE OF REF
!
      IF(LSORB) GO TO 500
      HOLD(1,1)=VMAT(1,1)*RMI(1)+VMAT(1,2)*RMI(2)+VMAT(1,3)*RMI(3)
      HOLD(2,1)=VMAT(2,1)*RMI(1)+VMAT(2,2)*RMI(2)+VMAT(2,3)*RMI(3)
      HOLD(3,1)=VMAT(3,1)*RMI(1)+VMAT(3,2)*RMI(2)+VMAT(3,3)*RMI(3)
      HOLD(1,2)=VMAT(1,1)*RMI(4)+VMAT(1,2)*RMI(5)+VMAT(1,3)*RMI(6)
      HOLD(2,2)=VMAT(2,1)*RMI(4)+VMAT(2,2)*RMI(5)+VMAT(2,3)*RMI(6)
      HOLD(3,2)=VMAT(3,1)*RMI(4)+VMAT(3,2)*RMI(5)+VMAT(3,3)*RMI(6)
      HOLD(1,3)=VMAT(1,1)*RMI(7)+VMAT(1,2)*RMI(8)+VMAT(1,3)*RMI(9)
      HOLD(2,3)=VMAT(2,1)*RMI(7)+VMAT(2,2)*RMI(8)+VMAT(2,3)*RMI(9)
      HOLD(3,3)=VMAT(3,1)*RMI(7)+VMAT(3,2)*RMI(8)+VMAT(3,3)*RMI(9)
      VMATA2(1,1)=                                                      &
      VMATA2(1,1)+RMI(1)*HOLD(1,1)+RMI(2)*HOLD(2,1)+RMI(3)*HOLD(3,1)
      VMATA2(2,1)=                                                      &
      VMATA2(2,1)+RMI(4)*HOLD(1,1)+RMI(5)*HOLD(2,1)+RMI(6)*HOLD(3,1)
      VMATA2(3,1)=                                                      &
      VMATA2(3,1)+RMI(7)*HOLD(1,1)+RMI(8)*HOLD(2,1)+RMI(9)*HOLD(3,1)
      VMATA2(1,2)=                                                      &
      VMATA2(1,2)+RMI(1)*HOLD(1,2)+RMI(2)*HOLD(2,2)+RMI(3)*HOLD(3,2)
      VMATA2(2,2)=                                                      &
      VMATA2(2,2)+RMI(4)*HOLD(1,2)+RMI(5)*HOLD(2,2)+RMI(6)*HOLD(3,2)
      VMATA2(3,2)=                                                      &
      VMATA2(3,2)+RMI(7)*HOLD(1,2)+RMI(8)*HOLD(2,2)+RMI(9)*HOLD(3,2)
      VMATA2(1,3)=                                                      &
      VMATA2(1,3)+RMI(1)*HOLD(1,3)+RMI(2)*HOLD(2,3)+RMI(3)*HOLD(3,3)
      VMATA2(2,3)=                                                      &
      VMATA2(2,3)+RMI(4)*HOLD(1,3)+RMI(5)*HOLD(2,3)+RMI(6)*HOLD(3,3)
      VMATA2(3,3)=                                                      &
      VMATA2(3,3)+RMI(7)*HOLD(1,3)+RMI(8)*HOLD(2,3)+RMI(9)*HOLD(3,3)
!
  500 CONTINUE
      RETURN
      END
