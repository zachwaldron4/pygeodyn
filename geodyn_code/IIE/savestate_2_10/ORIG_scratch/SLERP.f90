      SUBROUTINE SLERP(DT,Q0,Q1,Q)
      IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! ROUTINE TO PERFORM SPHERICAL LINEAR INTERPOLATION ON QUATERNIONS
!
!    DT    - LINEAR INTERPOLATION FRACTION BETWEEN TIME 0 and TIME 1
!    Q0    - QUATERNION AT TIME 0
!    Q1    - QUATERNION AT TIME 1
!    Q     - INTERPOLATED QUATERNION
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DOUBLE PRECISION  DT,Q0(4),Q1(4),Q(4)
      DOUBLE PRECISION W,D(4),DN,ANG,DC,DS,TEST,WW
!
!
      D(1)=Q1(1)-Q0(1)
      D(2)=Q1(2)-Q0(2)
      D(3)=Q1(3)-Q0(3)
      D(4)=Q1(4)-Q0(4)
      TEST=SQRT(D(1)*D(1)+D(2)*D(2)+D(3)*D(3)+D(4)*D(4))
      IF(TEST.LT.0.00001D0) THEN
       Q(1)=Q0(1)
       Q(2)=Q0(2)
       Q(3)=Q0(3)
       Q(4)=Q0(4)
       RETURN
      ENDIF
      WW=Q0(1)*Q1(1)+Q0(2)*Q1(2)+Q0(3)*Q1(3)+Q0(4)*Q1(4)
      IF(WW.GT.1.D0) WW=1.D0
      W=ACOS(WW)
!     W=DACOS(Q0(1)*Q1(1)+Q0(2)*Q1(2)+Q0(3)*Q1(3)+Q0(4)*Q1(4))
      D(1)=Q1(1)-Q0(1)*COS(W)
      D(2)=Q1(2)-Q0(2)*COS(W)
      D(3)=Q1(3)-Q0(3)*COS(W)
      D(4)=Q1(4)-Q0(4)*COS(W)
      DN=SQRT(D(1)*D(1)+D(2)*D(2)+D(3)*D(3)+D(4)*D(4))
      ANG=DT*W
      DC=COS(ANG)
      DS=SIN(ANG)
!
      Q(1)=Q0(1)*DC+DS*D(1)/DN
      Q(2)=Q0(2)*DC+DS*D(2)/DN
      Q(3)=Q0(3)*DC+DS*D(3)/DN
      Q(4)=Q0(4)*DC+DS*D(4)/DN
!
      RETURN
      END
