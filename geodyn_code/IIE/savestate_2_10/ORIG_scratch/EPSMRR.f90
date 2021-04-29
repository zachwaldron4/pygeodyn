!$EPSMR
      SUBROUTINE EPSMRR(T,ARMN,EPSMM)
!********1*********2*********3*********4*********5*********6*********7
! EPSMR          01/11/93            0000.0    PGMR - S.LUO
!
! FUNCTION:  CALCULATE MARS MEAN OBLIQUITY AT  TIME, T.
!
! I/O PARAMETERS
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   T        I         INTERVAL TIME IN DAY FROM J2000.0(JD2451545)
!   EPSMM    O         MARS MEAN OBLIQUITY AT EPOCH T
!   ARMN     O         ANGLE BETWEEN EQUINOX AND ASCENDING NODE OF MARS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DIMENSION RM(3),VPMEEC(3), VMASCE(3),VPMOBE(3),VPMEJ2(3)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
! CALL SUBROUTINE PMEQJ2 TO GET THE DIRECTION OF
!      THE NORTH POLE OF MARS EQUATOR PLANE IN ECLIPTIC
!
!     CALL PMEQJ2(T,VPMEJ2,VPMEEC,AL,DL)
      CALL PMEQJ2(T,VPMEJ2,VPMEEC)
!
! CALL SUBROUTINE PMASCE TO GET THE DIRECTIONS OF MARS ASCENDING NODE
!      AND THE POLE OF MARS ORBIT IN ECLIPLIC
!
      CALL PMASCE(T,VMASCE,VPMOBE)
!
!
! CALL ARGMA TO GET THE ANGLE BETWEEN MARS VERNAL EQUINOX AND ITS
!     PERIHELION
!
      CALL ARGMA(VPMEEC,VPMOBE,VMASCE,SLMD,ARMN,EPSMM,RM)
      ARMN = ARMN*DEGRAD
!
      RETURN
      END
