!$ROTQAT
      SUBROUTINE ROTQAT(ROT,QAT)
!********1*********2*********3*********4*********5*********6*********7**
! ROTQAT           92/09/09            0000.0    PGMR - SBL
!
! FUNCTION:         COMPUTE QUATERNION FROM A DIRECTION COSINE MATRIX
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ROT      I          DIRECTION COSINE MATRIX
!   QAT      O          COMPUTED QUATERNION
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION ROT(3,3),QAT(4)
!
      DATA ONE/1.0D0/,HALF/0.5D0/,FOUR/4.0D0/,ZERO/0.0D0/
      DATA C1M3/1.0D-3/
      DATA C5M1/5.0D-1/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      TRC1=ONE+ROT(1,1)+ROT(2,2)+ROT(3,3)
      QAT(4)=ZERO
      IF(TRC1.GT.ZERO) QAT(4)=HALF*SQRT(TRC1)
!
      IF(ABS(QAT(4)).GT.C1M3) THEN
      QATD4=FOUR*QAT(4)
      QAT(1)=(ROT(2,3)-ROT(3,2))/QATD4
      QAT(2)=(ROT(3,1)-ROT(1,3))/QATD4
      QAT(3)=(ROT(1,2)-ROT(2,1))/QATD4
!
      ELSE
! debug extatt
!      print *,'rotqat: q4 is: ',QAT(4)
!      print *,'rotqat: the rotation matrix is: '
!      print *,rot(1,1),rot(1,2),rot(1,3)
!      print *,rot(2,1),rot(2,2),rot(2,3)
!      print *,rot(3,1),rot(3,2),rot(3,3)
! debug extatt
!
!     ARG1=HALF*SQRT(ONE+ROT(1,1)-ROT(2,2)-ROT(3,3))
!     ARG2=HALF*SQRT(ONA-ROT(1,1)+ROT(2,2)-ROT(3,3))
!     ARG3=HALF*SQRT(ONE-ROT(1,1)-ROT(2,2)+ROT(3,3))
      ARG1=HALF*SQRT(ABS(ONE+ROT(1,1)-ROT(2,2)-ROT(3,3)))
      ARG2=HALF*SQRT(ABS(ONE-ROT(1,1)+ROT(2,2)-ROT(3,3)))
      ARG3=HALF*SQRT(ABS(ONE-ROT(1,1)-ROT(2,2)+ROT(3,3)))
! debug extatt
!      print *,'rotqat: arg1-3: ',arg1,arg2,arg3
! debug exatt
       IF(ARG3.GE.C5M1) THEN
        QAT(3)=ARG3
        QATD4=FOUR*QAT(3)
        QAT(1)=(ROT(3,1)+ROT(1,3))/QATD4
        QAT(2)=(ROT(3,2)+ROT(2,3))/QATD4
        QAT(4)=(ROT(1,2)-ROT(2,1))/QATD4
       ELSE IF(ARG2.GE.C5M1) THEN
        QAT(2)=ARG2
        QATD4=FOUR*QAT(2)
        QAT(1)=(ROT(2,1)+ROT(1,2))/QATD4
        QAT(3)=(ROT(2,3)+ROT(3,2))/QATD4
        QAT(4)=(ROT(3,1)-ROT(1,3))/QATD4
       ELSE IF(ARG1.GE.C5M1) THEN
        QAT(1)=ARG1
        QATD4=FOUR*QAT(1)
        QAT(2)=(ROT(1,2)+ROT(2,1))/QATD4
        QAT(3)=(ROT(1,3)+ROT(3,1))/QATD4
        QAT(4)=(ROT(2,3)-ROT(3,2))/QATD4
       ELSE
        PRINT *,' ABNORMAL TERMINATION IN ROTQAT'
        PRINT *,' QUATERNION COULD NOT BE COMPUTED DUE TO SINGULARITIES'
       ENDIF
!
      ENDIF
!
        QMAG=SQRT(QAT(1)**2+QAT(2)**2+QAT(3)**2+QAT(4)**2)
        QAT(1)=QAT(1)/QMAG
        QAT(2)=QAT(2)/QMAG
        QAT(3)=QAT(3)/QMAG
        QAT(4)=QAT(4)/QMAG
      RETURN
      END
