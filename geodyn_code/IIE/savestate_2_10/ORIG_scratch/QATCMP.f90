!$QATCMP
      SUBROUTINE QATCMP(XPQ,VPQ,QSBF,ROLL,PITCH,YAW)
!********1*********2*********3*********4*********5*********6*********7**
! QATCMP           96/05/14            9505.00   PGMR - SBL
!
! FUNCTION:         COMPARE LVLH WITH BODY QUATERNIONS
!
! I/O PARAMETERS:
!
! NAME      I/O   A/S   DESCRIPTION OF PARAMETERS
! _______   ___   ___   ________________________________________________
!  XPQ       I     A    J2000 sat. position
!  QSBF      I     A    SBF to J2000 Quaternion
!  ROLL      O     S    ROLL OF S/C
!  PITCH     O     S    PITCH OF S/C
!  YAW       O     S    YAW OF S/C
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION XPQ(3),VPQ(3),QSBF(4),ROT(3,3)
      DIMENSION QJ20LVLH(4),QSBFLVLH(4)
!
      DATA ONE/1.0D0/,TWO/2.0D0/,HALF/0.5D0/,FOUR/4.0D0/,ZERO/0.D0/
      data degrad/1.7453292519943296D-2/
!
!***********************************************************************
! START OF EXECUTABLE CODE**********************************************
!***********************************************************************
!
! compute j2000 to lvlh rotation matrix
!
      a=xpq(3)*xpq(3)*vpq(1) - vpq(3)*xpq(1)*xpq(3) -                   &
     &  xpq(1)*xpq(2)*vpq(2) + vpq(1)*xpq(2)*xpq(2)
      b=xpq(1)*xpq(1)*vpq(2) - vpq(1)*xpq(1)*xpq(2) -                   &
     &  xpq(2)*xpq(3)*vpq(3) + vpq(2)*xpq(3)*xpq(3)
      c=xpq(2)*xpq(2)*vpq(3) - vpq(2)*xpq(2)*xpq(3) -                   &
     &  xpq(1)*xpq(3)*vpq(1) + vpq(3)*xpq(1)*xpq(1)
      q=SQRT(a*a + b*b + c*c)
!
      d=vpq(2)*xpq(3)-xpq(2)*vpq(3)
      e=vpq(3)*xpq(1)-xpq(3)*vpq(1)
      f=vpq(1)*xpq(2)-xpq(1)*vpq(2)
      s=SQRT(d*d + e*e + f*f)
!
      r=SQRT(xpq(1)*xpq(1)+xpq(2)*xpq(2)+xpq(3)*xpq(3))
!
      rot(1,1)=a/q
      rot(1,2)=b/q
      rot(1,3)=c/q
      rot(2,1)=d/s
      rot(2,2)=e/s
      rot(2,3)=f/s
      rot(3,1)=-xpq(1)/r
      rot(3,2)=-xpq(2)/r
      rot(3,3)=-xpq(3)/r
!
      call rotqat(rot,qj20lvlh)
!
      call qcomp(qj20lvlh,qsbf,qsbflvlh)
!
      call eul123(qsbflvlh,roll,pitch,yaw)
!      call eul231(qsbflvlh,pitch,yaw,roll)
!
      roll=roll/degrad
      pitch=pitch/degrad
      yaw=yaw/degrad
!
!      write(69,69690) rmes,roll,pitch,yaw
!69690 format(1x,d24.16,3(1x,d13.6))
!
      RETURN
      END
