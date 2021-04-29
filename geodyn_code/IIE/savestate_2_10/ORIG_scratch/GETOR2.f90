      SUBROUTINE GETOR2(XAXIS,XAXIS2,XA2B,CMAT,DNORM,DC123,DS123,DVECT, &
     &                  LONA)
!!!  &                  XMAT1,RB,DB,WB,LONA)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     CMAT IS FRM B TO A
!     CMAT, RB,DB,WB VERIFIED INDEPENDENTLY
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION XAXIS(3,3)
      DIMENSION XAXIS2(3,3)
      DIMENSION XA2B(3),DC123(3),DS123(3),DVECT(3)
      DIMENSION FSEC(1),RA(1),DEC(1),SCRTCH(4),ROTI(9),ROTJ(9)
      DIMENSION XMAT1(3,3),XMAT2(3,3)
      DIMENSION CMAT(3,3)

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DEC(1)=ASIN(XAXIS(3,3))
      IF(ABS(XAXIS(3,3)).LT.1.D0) THEN
        R=SQRT(XAXIS(1,3)*XAXIS(1,3)+XAXIS(2,3)*XAXIS(2,3))
        RA(1)=ATAN2(XAXIS(2,3)/R,XAXIS(1,3)/R)
      ELSE
        RA(1)=0.D0
      ENDIF
!
      CALL DXQN(MJDSC,FSEC,RA,DEC,ROTI(1),ROTI(2),ROTI(3),              &
     &                ROTI(4),ROTI(5),ROTI(6),ROTI(7),ROTI(8),          &
     &                ROTI(9),SCRTCH,1)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DEC(1)=ASIN(XAXIS2(3,3))
      DB=DEC(1)
      IF(ABS(XAXIS2(3,3)).LT.1.D0) THEN
        R=SQRT(XAXIS2(1,3)*XAXIS2(1,3)+XAXIS2(2,3)*XAXIS2(2,3))
        RA(1)=ATAN2(XAXIS2(2,3)/R,XAXIS2(1,3)/R)
      ELSE
        RA(1)=0.D0
      ENDIF
      RB=RA(1)
!
      CALL DXQN(MJDSC,FSEC,RA,DEC,ROTJ(1),ROTJ(2),ROTJ(3),              &
     &                ROTJ(4),ROTJ(5),ROTJ(6),ROTJ(7),ROTJ(8),          &
     &                ROTJ(9),SCRTCH,1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! INERTIAL IAU XAXIS IN J2000 1ST ROW OF ROTI
      COSTGA=XAXIS(1,1)*ROTI(1)+XAXIS(2,1)*ROTI(4)+XAXIS(3,1)*ROTI(7)
      C1=ROTI(4)*XAXIS(3,1)-ROTI(7)*XAXIS(2,1)
      C2=ROTI(7)*XAXIS(1,1)-ROTI(1)*XAXIS(3,1)
      C3=ROTI(1)*XAXIS(2,1)-ROTI(4)*XAXIS(1,1)
!
! C1,C2,C3 HAS LENGTH |SINTHG|
! AND HAS SAME ORIRNTATION (UP TO DIRECTION) AS XAXIS(*,3)
      SINTGA=XAXIS(1,3)*C1+XAXIS(2,3)*C2+XAXIS(3,3)*C3
!
! INERTIAL IAU XAXIS IN J2000 1ST ROW OF ROTJ
      COSTGB=XAXIS2(1,1)*ROTJ(1)+XAXIS2(2,1)*ROTJ(4)+XAXIS2(3,1)*ROTJ(7)
      C1=ROTJ(4)*XAXIS2(3,1)-ROTJ(7)*XAXIS2(2,1)
      C2=ROTJ(7)*XAXIS2(1,1)-ROTJ(1)*XAXIS2(3,1)
      C3=ROTJ(1)*XAXIS2(2,1)-ROTJ(4)*XAXIS2(1,1)
!
! C1,C2,C3 HAS LENGTH |SINTHG|
! AND HAS SAME ORIRNTATION (UP TO DIRECTION) AS XAXIS(*,3)
      SINTGB=XAXIS2(1,3)*C1+XAXIS2(2,3)*C2+XAXIS2(3,3)*C3
      WB=ATAN2(SINTGB,COSTGB)
!
      IF(LONA) RETURN
!
!
      XMAT1(1,1)=ROTJ(1)*COSTGB+ROTJ(2)*SINTGB
      XMAT1(2,1)=ROTJ(4)*COSTGB+ROTJ(5)*SINTGB
      XMAT1(3,1)=ROTJ(7)*COSTGB+ROTJ(8)*SINTGB
      XMAT1(1,2)=-ROTJ(1)*SINTGB+ROTJ(2)*COSTGB
      XMAT1(2,2)=-ROTJ(4)*SINTGB+ROTJ(5)*COSTGB
      XMAT1(3,2)=-ROTJ(7)*SINTGB+ROTJ(8)*COSTGB
      XMAT1(1,3)=ROTJ(3)
      XMAT1(2,3)=ROTJ(6)
      XMAT1(3,3)=ROTJ(9)
!
!
      XMAT2(1,1)=COSTGA*ROTI(1)+SINTGA*ROTI(2)
      XMAT2(2,1)=-SINTGA*ROTI(1)+COSTGA*ROTI(2)
      XMAT2(3,1)=ROTI(3)
      XMAT2(1,2)=COSTGA*ROTI(4)+SINTGA*ROTI(5)
      XMAT2(2,2)=-SINTGA*ROTI(4)+COSTGA*ROTI(5)
      XMAT2(3,2)=ROTI(6)
      XMAT2(1,3)=COSTGA*ROTI(7)+SINTGA*ROTI(8)
      XMAT2(2,3)=-SINTGA*ROTI(7)+COSTGA*ROTI(8)
      XMAT2(3,3)=ROTI(9)
!
!
      CMAT(1,1)=XMAT2(1,1)*XMAT1(1,1)+XMAT2(1,2)*XMAT1(2,1)             &
     &         +XMAT2(1,3)*XMAT1(3,1)
      CMAT(2,1)=XMAT2(2,1)*XMAT1(1,1)+XMAT2(2,2)*XMAT1(2,1)             &
     &         +XMAT2(2,3)*XMAT1(3,1)
      CMAT(3,1)=XMAT2(3,1)*XMAT1(1,1)+XMAT2(3,2)*XMAT1(2,1)             &
     &         +XMAT2(3,3)*XMAT1(3,1)
      CMAT(1,2)=XMAT2(1,1)*XMAT1(1,2)+XMAT2(1,2)*XMAT1(2,2)             &
     &         +XMAT2(1,3)*XMAT1(3,2)
      CMAT(2,2)=XMAT2(2,1)*XMAT1(1,2)+XMAT2(2,2)*XMAT1(2,2)             &
     &         +XMAT2(2,3)*XMAT1(3,2)
      CMAT(3,2)=XMAT2(3,1)*XMAT1(1,2)+XMAT2(3,2)*XMAT1(2,2)             &
     &         +XMAT2(3,3)*XMAT1(3,2)
      CMAT(1,3)=XMAT2(1,1)*XMAT1(1,3)+XMAT2(1,2)*XMAT1(2,3)             &
     &         +XMAT2(1,3)*XMAT1(3,3)
      CMAT(2,3)=XMAT2(2,1)*XMAT1(1,3)+XMAT2(2,2)*XMAT1(2,3)             &
     &         +XMAT2(2,3)*XMAT1(3,3)
      CMAT(3,3)=XMAT2(3,1)*XMAT1(1,3)+XMAT2(3,2)*XMAT1(2,3)             &
     &         +XMAT2(3,3)*XMAT1(3,3)
!
!
      DC123(1)=XMAT1(1,1)*XA2B(1)+XMAT1(2,1)*XA2B(2)+XMAT1(3,1)*XA2B(3)
      DC123(2)=XMAT1(1,2)*XA2B(1)+XMAT1(2,2)*XA2B(2)+XMAT1(3,2)*XA2B(3)
      DC123(3)=XMAT1(1,3)*XA2B(1)+XMAT1(2,3)*XA2B(2)+XMAT1(3,3)*XA2B(3)
      DNORM=SQRT(DC123(1)*DC123(1)+DC123(2)*DC123(2)+DC123(3)*DC123(3))
      DS123(1)=DC123(1)/DNORM
      DS123(2)=DC123(2)/DNORM
      DS123(3)=DC123(3)/DNORM
!
!
      DVECT(1)=DC123(1)*XAXIS2(1,1)+DC123(2)*XAXIS2(1,2)                &
     &        +DC123(3)*XAXIS2(1,3)
      DVECT(2)=DC123(1)*XAXIS2(2,1)+DC123(2)*XAXIS2(2,2)                &
     &        +DC123(3)*XAXIS2(2,3)
      DVECT(3)=DC123(1)*XAXIS2(3,1)+DC123(2)*XAXIS2(3,2)                &
     &        +DC123(3)*XAXIS2(3,3)
!
!
      RETURN
      END
