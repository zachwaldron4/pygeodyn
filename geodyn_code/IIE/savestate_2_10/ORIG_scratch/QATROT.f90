!$QATROT
      SUBROUTINE QATROT(Q,ROT)
!********1*********2*********3*********4*********5*********6*********7**
! ROTQAT           93/06/29            9305.03   PGMR - SBL
!
! FUNCTION:         COMPUTE DIRECTION COSINE MATRIX FROM A QUATERNION
!
! I/O PARAMETERS:
!
! NAME      I/O   A/S   DESCRIPTION OF PARAMETERS
! _______   ___   ___   ________________________________________________
!  Q         I          COMPUTED QUATERNION
!  ROT       O          DIRECTION COSINE MATRIX
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION ROT(3,3),Q(4)
!
      DATA ONE/1.0D0/,TWO/2.0D0/,HALF/0.5D0/,FOUR/4.0D0/,ZERO/0.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE**********************************************
!***********************************************************************
!
      ROT(1,1)=Q(1)**2-Q(2)**2-Q(3)**2+Q(4)**2
      ROT(2,2)=-Q(1)**2+Q(2)**2-Q(3)**2+Q(4)**2
      ROT(3,3)=-Q(1)**2-Q(2)**2+Q(3)**2+Q(4)**2
      ROT(1,2)=TWO*(Q(1)*Q(2) + Q(4)*Q(3))
      ROT(1,3)=TWO*(Q(1)*Q(3) - Q(4)*Q(2))
      ROT(2,1)=TWO*(Q(2)*Q(1) - Q(4)*Q(3))
      ROT(2,3)=TWO*(Q(2)*Q(3) + Q(4)*Q(1))
      ROT(3,1)=TWO*(Q(3)*Q(1) + Q(4)*Q(2))
      ROT(3,2)=TWO*(Q(3)*Q(2) - Q(4)*Q(1))
      RETURN
      END
