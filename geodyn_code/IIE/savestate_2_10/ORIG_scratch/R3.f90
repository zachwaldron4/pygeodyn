      SUBROUTINE R3(ANG,ROT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION ROT(3,3)
!
      COSA=COS(ANG)
      SINA=SIN(ANG)
!
      ROT(1,1)=COSA
      ROT(1,2)=SINA
      ROT(1,3)=0.D0
!
      ROT(2,1)=-SINA
      ROT(2,2)=COSA
      ROT(2,3)=0.D0
!
      ROT(3,1)=0.D0
      ROT(3,2)=0.D0
      ROT(3,3)=1.D0
!
      RETURN
      END
