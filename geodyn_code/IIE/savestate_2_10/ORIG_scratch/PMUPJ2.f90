!$PMUPJ2
      SUBROUTINE PMUPJ2(VPMEJ2,DPSI,DEPSI,AUTMOB,OBMTAU,RR,DD)
!********1*********2*********3*********4*********5*********6*********7
! PMUPJ2           04/15/97            0000.0    PGMR - S.LUO
!
! FUNCTION: UPDATE THE RIGHT ASCENSION AND DECLINATION OF MARS POLE
!           IN EARTH J2000 BY ROTATING IT INTO MARS ORBIT + MARS VERNAL
!           SYSTEM THEN ADDING MARS NUTATION ( DR. HILTON'S MODEL)
!           AND ROTATE IT BACK TO EARTH J2000 SYSTEM
!
! I/O PARAMETERS
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   VPMEJ2   I    A    THE DIRECTION OF THE POLE OF MARS EQUATOR IN
!                      J2000 SYSTEM ( ONLY MARS PRECESSION APPLIED)
!   AUTMOB   I    A    ROTATION MATRICS FROM EARTH J2000 to MARS MEAN
!                      ORBIT+MARS VERNAL EQUINOX SYSTEM
!   OBMTAU   I    A    ROTATION MATRICS FROM MARS MEAN ORBIT+MARS
!                      VERNAL EQUINOX SYSTEM (T) TO EARTH J2000 SYSTEM
!   DPSI     I    S    MARS NUTATION IN LONGITUDE ( MARS MEAN ORBIT SYS.
!   DEPSI    I    S    MARS OBLIQUITY NUTATION  ( MARS MEAN ORBIT SYS.)
!   ...................................................................
!   RR       O    S    UPDATED RIGHT ASCENSION OF MARS POLE
!   DD       O    S    UPDATED DECLINATION OF MARS POLE
!                      ( BOTH RR AND DD ARE IN EARTH J2000)
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION VPMEJ2(3),AAA(3),BBB(3),AUTMOB(3,3),OBMTAU(3,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      CALL MULTI(AUTMOB,VPMEJ2,AAA,3,3,1)
! get ALONOB & BLATOB of MARS EQUATOR POLE in Mars orbit plane
! from AAA(3)
      ALONOB=ATAN(AAA(2)/AAA(1))
      BLATOB=ACOS(AAA(3))
      AAL=ALONOB/DEGRAD
      BBL=BLATOB/DEGRAD
!     PRINT *,'AAL, BBL**',AAL,BBL
!
! UPDATE AAA(3) by adding Mars nutatuion in
!
      ALONOB=ALONOB-DPSI
!     ALONOB=ALONOB+DPSI
!      BLATOB=BLATOB+DEPSI
       BLATOB=BLATOB-DEPSI
      AAA(1)=SIN(BLATOB)*COS(ALONOB)
      AAA(2)=SIN(BLATOB)*SIN(ALONOB)
      AAA(3)=COS(BLATOB)
      CALL MULTI(OBMTAU,AAA,BBB,3,3,1)
!
!     PRINT *,'TEST* VPMEJ2 *', VPMEJ2
!     PRINT *,'TEST* BBB *', BBB
      ARIGHJ=ATAN(BBB(2)/BBB(1))
      BDECLJ=ACOS(BBB(3))
      RR =TWOPI+ARIGHJ
      DD =PI/2.D0-BDECLJ
      AAL=RR/DEGRAD
      BBL=DD/DEGRAD
!     PRINT *,'PMUPJ2***AAL, BBL**',AAL,BBL
!
      RETURN
      END
