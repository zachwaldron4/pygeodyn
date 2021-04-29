      SUBROUTINE COEFNC(IORDER,NRAT1,CPPNC)
!********1*********2*********3*********4*********5*********6*********7**
! COEFNC                                         PGMR - D. ROWLANDS
!
!
! FUNCTION:  COMPUTE THE COEFFCIENTS NEEDED TO PREDICT THE LARGE STEP
!            STATE AT SMALL STEP TIMES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IORDER   I    S    INTEGRATION ORDER
!   NRAT1    I    S    NRAT-1 WHERE NRAT IS THE RATIO OF THE LARGE
!                      STEP TO THE SMALL STEP
!   CPPNC    O    A    COEFFICENTS NECESSARY TO PREDICT LARGE STEP STATE
!                      AT SMALL STATE EPOCHS
!
!
!   COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      PARAMETER(MRAT=500)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!
      DIMENSION CPPNC(NRAT1,IORDER,2)
      DIMENSION S(MRAT),B(MRAT)
!
      NRAT=NRAT1+1
      IF(NRAT.GT.MRAT) THEN
         WRITE(6,6000)
         WRITE(6,6001) NRAT,MRAT
         STOP
      ENDIF
!
      DO IQP=1,NRAT1
        S(IQP)=DBLE(IQP)/DBLE(NRAT)
      ENDDO
!
      CALL COEFV(S,B,NRAT1,IORDER,CPPNC(1,1,1),CPPNC(1,1,2))
!
      DO IQP=1,NRAT1
        CPPNC(IQP,IORDER,1)=S(IQP)
      ENDDO

      RETURN
 6000 FORMAT(' EXECUTION TERMINATING IN COEFNC ')
 6001 FORMAT(' NRAT=',I5,' MRAT=',I5)
      END
