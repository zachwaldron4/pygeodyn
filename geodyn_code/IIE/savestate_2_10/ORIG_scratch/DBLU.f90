!$DBLU
      FUNCTION DBLU(N,M,K,MSIGN,EPSLON)
!********1*********2*********3*********4*********5*********6*********7**
! DBLU             83/08/18            8308.0    PGMR - DEMOS
!                                                       CHRISTODOULIDIS
!                                                PGMR - D. ROWLANDS
!                                                       (MOD FOR GII)
!
! FUNCTION:   CALCULATES A FACTOR GOING INTO EACH DOODSON COEFFCIENT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   N        I    S    DEGREE ASSOCIATED WITH COEFFCIENT
!   M        I    S    TIDAL EXPANSION ARGUMENT
!   K        I    S    TIDAL EXPANSION ARGUMENT
!   MSIGN    I    S    SIGN ASSOCIATED WITH TIDAL EXPANSION ARGUMENTS
!   EPSLON   I    S    ANGLE OF THE ECLIPTIC
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DATA HALF/.5D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IR=K*MSIGN
      IDM=0
      IF(M.EQ.0) IDM=1
      IDK=0
      IF(K.EQ.0) IDK=1
      IT=IDM+IDK
      IS=1-2*MOD(IT,2)
      DBLU=((ONE-HALF*IDK)*IS)*PSI(N,M,IR,EPSLON)
      RETURN
      END
