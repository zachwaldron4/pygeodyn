      SUBROUTINE MMULT3(SL,SR,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION SL(3,3)
      DIMENSION SR(3,3)
      DIMENSION R(3,3)
!
      R(1,1)=SL(1,1)*SR(1,1)+SL(1,2)*SR(2,1)+SL(1,3)*SR(3,1)
      R(2,1)=SL(2,1)*SR(1,1)+SL(2,2)*SR(2,1)+SL(2,3)*SR(3,1)
      R(3,1)=SL(3,1)*SR(1,1)+SL(3,2)*SR(2,1)+SL(3,3)*SR(3,1)
!
      R(1,2)=SL(1,1)*SR(1,2)+SL(1,2)*SR(2,2)+SL(1,3)*SR(3,2)
      R(2,2)=SL(2,1)*SR(1,2)+SL(2,2)*SR(2,2)+SL(2,3)*SR(3,2)
      R(3,2)=SL(3,1)*SR(1,2)+SL(3,2)*SR(2,2)+SL(3,3)*SR(3,2)
!
      R(1,3)=SL(1,1)*SR(1,3)+SL(1,2)*SR(2,3)+SL(1,3)*SR(3,3)
      R(2,3)=SL(2,1)*SR(1,3)+SL(2,2)*SR(2,3)+SL(2,3)*SR(3,3)
      R(3,3)=SL(3,1)*SR(1,3)+SL(3,2)*SR(2,3)+SL(3,3)*SR(3,3)
!
      RETURN
      END
