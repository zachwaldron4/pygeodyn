!$PMEQJ2
!     SUBROUTINE PMEQJ2(T,VPMEJ2,VPMEEC,AL,DE)
      SUBROUTINE PMEQJ2(T,VPMEJ2,VPMEEC)
!********1*********2*********3*********4*********5*********6*********7
! PMEQJ2           12/02/92            0000.0    PGMR - S.LUO
!
! FUNCTION: CALCULATE THE COMPONENTS OF THE POLE VECTOR OF MARS EQUATOR
!            IN STANDARD REFERENCE FRAME, THEN TRANSFORM IT TO ECLIPTIC
!            FRAME
!
! I/O PARAMETERS
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   T        I         TIME INTERVAL IN DAY FROM J2000.0
!   VPMEJ2   O    A    THE DIRECTION OF THE POLE OF MARS EQUATOR IN
!                      J2000 SYSTEM
!   ...................................................................
!   AL       O         RIGHT ASCENSION OF ASCENDING NODE OF MARS ORBIT
!                      ON ECLIPTIC IN J2000 EARTH REFERENCE FRAME
!   DE       O         DECLINATION OF ASCENDING NODE OF MARS ORBIT
!                      ON ECLIPTIC IN J2000 EARTH REFERENCE FRAME
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/ASTRO /A0,DA0,B0,DB0,V0,DV0,T01
      DIMENSION VPMEJ2(3),VPMEEC(3),A(3,3)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      D=T
      TT=D/36525.D0
      AL=A0+DA0*TT
      DE=B0+DB0*TT
!
! CONVERT THE UNIT OF ALPHA AND DELTA TO RADIAN FROM DEGREE
!
      ALPHA=AL*DEGRAD
      DELTA=DE*DEGRAD
!
      CSDE = COS(DELTA)
      SNDE = SIN(DELTA)
      CSAL = COS(ALPHA)
      SNAL = SIN(ALPHA)
!
      VPMEJ2(1) = CSDE * CSAL
      VPMEJ2(2) = CSDE * SNAL
      VPMEJ2(3) = SNDE
! DEBUG...
!     WRITE(6,100)AL,DE
!
!     ROTATE VPMEJ2 TO VPMEEC
!
      E0 = 23.4392911D0
      EE = E0*DEGRAD
      CALL ROTAT(EE,A,1)
      CALL MULTI(A,VPMEJ2,VPMEEC,3,3,1)
!
! DEBUG..
!CC   WRITE(6,200)VPMEJ2,VPMEEC
  100 FORMAT(1X, 'PMEQJ2**','ALPHA=',F20.9,'DELTA=',F20.9)
  200 FORMAT(1X, 'VPMEJ2 =', 3F10.4/                                    &
     &       1X, 'VPMEEC =', 3F10.4)
      RETURN
      END
