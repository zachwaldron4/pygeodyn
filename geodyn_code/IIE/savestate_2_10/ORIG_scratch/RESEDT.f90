!$RESEDT
      SUBROUTINE RESEDT(EDTLVL,RESID ,RESIDU,EDTSIG,RATIO ,LEDIT ,NM)
!********1*********2*********3*********4*********5*********6*********7**
! RESEDT           86/03/25            8604.0    PGMR - TOM MARTIN
!
! FUNCTION:  EDIT RESIDUAL RATIOS AGAINST EDIT LEVEL TO ELIMINATE
!            GROSS RESIDUALS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   EDTLVL   I    S    EDIT LEVEL
!   RESID    I    A    OBSERVATION RESIDUALS
!   RESIDU  I/O   A    USED TO STORE ABS( RATIO )
!   EDTSIG   I    A    OBSERVATION EDITING SIGMA
!   RATIO    O    A    RATIO OF OBSERVATION RESIDUALS TO EDITING
!                      SIGMAS
!   LEDIT   I/O   A    SWITCHES INDICATING WHICH OBSERVATIONS HAVE
!                      BEEN PREVIOUSLY EDITED AS INPUT
!                      SWITCHES INDICATING WHICH OBSERVATIONS HAVE
!                      BEEN EDITED AS OUTPUT
!   NM       I    S    NUMBER OF OBSERVATIONS IN THIS LOGICAL BLOCK
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION RESID (NM),RATIO (NM),LEDIT(NM),RESIDU(NM),EDTSIG(NM)
      DATA ZERO/0.0D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
! CLEAR RATIO TO EDITING SIGMA ARRAY AND THEN
! COMPUTE RATIO OF RESIDUALS TO EDITING SIGMAS
!
      DO 2200 N=1,NM
         RATIO (N)=ZERO
         IF(EDTSIG(N).LE.ZERO) GO TO 2200
         RATIO (N)=RESID (N)/EDTSIG(N)
 2200 END DO
!
! COMPUTE ABSOLUTE VALUE OF RATIO TO SIGMA
!
      DO 2300 N=1,NM
         RESIDU(N)=ABS(RATIO(N))
 2300 END DO
!
! EDIT LARGE RATIOS
!
      DO 2400 N=1,NM
         IF(RESIDU(N).LE.EDTLVL) GO TO 2400
         LEDIT(N)=.TRUE.
 2400 END DO
      RETURN
      END
