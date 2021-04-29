!$FACT
      FUNCTION FACT(I)
!********1*********2*********3*********4*********5*********6*********7**
! FACT             83/08/15            8308.0    PGMR - DEMOS
!                                                        CHRISTODOULIDIS
!                                                       D. ROWLANDS
!                                                        (MOD FOR GII)
!
! FUNCTION:  COMPUTE A FACTOR IN DOODSON COEFFCIENT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   I
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DATA ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      FACT=ONE
      IF(I.EQ.0) RETURN
      DO 10 J=1,I
      FACT=FACT*DBLE(J)
   10 END DO
      RETURN
      END
