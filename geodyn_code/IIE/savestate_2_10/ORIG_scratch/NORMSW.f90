!$NORMSW
      INTEGER FUNCTION NORMSW(IPRE,MTYPE,IRECDH)
!********1*********2*********3*********4*********5*********6*********7**
! NORMSW           86/06/04            8606.0    PGMR - TOM MARTIN
!
! FUNCTION:   MODIFY OBSERVATION HEADER RECORD PREPROCESSING WORDS
!             TO REFLECT THE OUTPUT OF OBSERVATION NORMAL POINTS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IPRE     I    S    INPUT PREPRO WORD.
!   MTYPE    I    S    OBSERVATION TYPE INDICATOR.
!   IRECDH   I    S    INDEX OF HEADER RECORD TO WHICH PREPRO
!                      WORD IS APPLICABLE.
!   NORMSW   O    S    MODIFIED PREPRO WORD.
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      DIMENSION LPREIN(24),LPRETP(9,5),LOCEAN(9),ITYPES(99),LPRESW(24,4)
      EQUIVALENCE (LPRESW(1,1),LPRE9(1))
      DATA LPRETP/                                                      &
     &   .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,               &
     &   .FALSE.,.FALSE.,.FALSE.,                                       &
     &   .TRUE. ,.FALSE.,.TRUE. ,.FALSE.,.TRUE. ,.FALSE.,               &
     &   .TRUE. ,.FALSE.,.TRUE. ,                                       &
     &   .FALSE.,.FALSE.,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,               &
     &   .FALSE.,.FALSE.,.FALSE.,                                       &
     &   .FALSE.,.TRUE. ,.TRUE. ,.TRUE. ,.TRUE. ,.FALSE.,               &
     &   .FALSE.,.TRUE. ,.FALSE.,                                       &
     &   .FALSE.,.FALSE.,.TRUE. ,.TRUE. ,.FALSE.,.FALSE.,               &
     &   .FALSE.,.FALSE.,.TRUE. /
!...PCE              F,F,F,F,F,F,F,F,F,  F,F,F,F,F,F,F,F,F,  F,F,F,F,T,T
!...OPTICAL          T,F,T,F,T,F,T,F,T,  F,F,F,F,F,F,F,F,F,  F,F,F,F,T,T
!...ANGLES           F,F,T,T,T,T,F,F,F,  F,F,F,F,F,F,F,F,F,  F,F,F,F,F,F
!...RANGE & DOPPLER  F,T,T,T,T,F,F,T,F,  F,F,F,F,F,F,F,F,F,  F,F,F,F,F,F
!...ALTIMETER        F,F,T,T,F,F,F,F,T,  F,T,T,F,T,F,F,F,F,  F,F,F,F,F,F
      DATA LOCEAN/                                                      &
     &   .FALSE.,.TRUE. ,.TRUE. ,.FALSE.,.TRUE. ,.FALSE.,               &
     &   .FALSE.,.FALSE.,.FALSE./
      DATA ITYPES/12*1, 2*2,24*3,60*4,   5/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      INTYPE=ITYPES(MTYPE)
      CALL DECOMP(IPRE,LPREIN,24)
! SET SWITCHES FOR CORRECTIONS TO ALL DATA TYPES
      DO 1000 IPART=1,9
      IF(.NOT.LPRETP(IPART,INTYPE)) GO TO 1000
      IF(.NOT.LSWTCH(IPART,IRECDH)) GO TO 1000
      LPREIN(IPART)=.NOT.LPRESW(IPART,IRECDH)
 1000 END DO
      IF(INTYPE.GT.2) GO TO 2000
      IF(IRECDH.NE.1) GO TO 2000
! SET COORD. SYS. SWITCHES FOR OPTICAL AND PCE DATA
      LPREIN(23)=.NOT.LPRESW(23,1)
      LPREIN(24)=.NOT.LPRESW(24,1)
 2000 CONTINUE
      IF(INTYPE.LT.5) GO TO 4000
      J=IRECDH/2
      IF(IRECDH.GT.0) GO TO 4000
! SET OCEAN PARAMETERS SWITCHES FOR ALTIMETRY
      DO 3000 IPART=1,9
      IF(.NOT.LOCEAN(IPART)) GO TO 3000
      IF(.NOT.LSWTCH(IPART,5)) GO TO 3000
      LPREIN(IPART+9)=.NOT.LPRESW(IPART+9,IRECDH)
 3000 END DO
 4000 CONTINUE
      CALL RECOMP(LPREIN,IPRE,24)
      NORMSW=IPRE
      RETURN
      END
