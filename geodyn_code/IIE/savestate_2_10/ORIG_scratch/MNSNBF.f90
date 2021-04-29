!$MNSNBF
      SUBROUTINE MNSNBF(NGOOD,IFGO,IFGN)
!********1*********2*********3*********4*********5*********6*********7**
! MNSNBF           83/08/23            8308.0    PGMR - D. ROWLANDS
!
! FUNCTION:   TRANSFER PREVIOUSLY CALCULATED PLANET STATE VECTOR
!             INFORMATION AT THE 12 HR INTERPOLATION ENDPOINTS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NGOOD    I    S    TOTAL NUMBER OF GOOD RECORDS TO BE SAVED
!   IFGO     I    S    FIRST LOCATION IN OLD BUFFER OF GOOD INFORMATION
!   IFGN     I    S    FIRST LOCATION IN NEW BUFFER OF GOOD INFORMATION
!
! COMMENTS:
!            OUTPUT THROUGH COMMON BLOCK MNSNI
!
! RESTRICTION:  SHOULD ONLY BE CALLED BY BUFPOS
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION HOLD(12,4)
      COMMON/MNSNI/XMNSNI(6,2,4)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(NGOOD.LE.0) RETURN
      NSPOT2=IFGO-1
      NSPOT1=IFGN-1
      DO 10 I=1,NGOOD
      DO 10 J=1,12
   10 HOLD(J,I)=XMNSNI(J,1,I+NSPOT2)
      DO 20 I=1,NGOOD
      DO 20 J=1,12
   20 XMNSNI(J,1,I+NSPOT1)=HOLD(J,I)
      RETURN
      END
