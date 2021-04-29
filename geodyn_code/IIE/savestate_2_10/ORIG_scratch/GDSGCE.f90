!$GDSGCE
      SUBROUTINE GDSGCE(ZSC,XY,COSGCL,SINGCL,SCRTCH,NM)
!********1*********2*********3*********4*********5*********6*********7**
! GDSGCE           00/00/00            8711.0    PGMR - D. ROWLANDS
!
! FUNCTION:  CONVERT SOME GEODETIC QUANTITIES TO GEOCENTRIC
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ZSC      I    A    Z COMPONENT OF SPACECRAFT FROME THE CENTER OF
!                      CURV
!   XY       I    A    RADIUS OS SATELLITE IN THE XY PLANE
!   COSGCL   O    A    COSINE OF GEOCENTRIC LAT OF SUBSAT POINT
!   SINGCL   O    A    SINE OF GEOCENTRIC LAT OF SUBSAT POINT
!   SCRTCH  I/O   A    WORKING ARRAY
!   NM       I    S    NUMBER OF OBSERVATIONS IN THE BLOCK
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CCBODY/ESQ,ESQP1,OMESQ
      DIMENSION ZSC(NM),XY(NM),COSGCL(NM),SINGCL(NM),SCRTCH(NM)
      DATA ONE/1.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!  GET GEOCENTRIC TAN
      DO 100 I=1,NM
      SINGCL(I)=OMESQ*ZSC(I)/XY(I)
  100 END DO
!  GET GEOCENTRIC COSINE & SINE
      DO 200 I=1,NM
      COSGCL(I)=ONE+SINGCL(I)*SINGCL(I)
      SCRTCH(I)=SQRT(COSGCL(I))
      COSGCL(I)=ONE/SCRTCH(I)
      SINGCL(I)=SINGCL(I)*COSGCL(I)
  200 END DO
      RETURN
      END
