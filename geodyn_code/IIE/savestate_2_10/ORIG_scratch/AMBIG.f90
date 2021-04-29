!$AMBIG
      SUBROUTINE AMBIG(OBS,OBSC,RAMBIG,NM,LVAMB,L2WAY,AA)
!********1*********2*********3*********4*********5*********6*********7**
! AMBIG            00/00/00            0000.0    PGMR - T. MARTIN
!
! FUNCTION:  RESOLVES OBSERVATION AMBIGUITIES.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!   OBS     I/O   A    OBSERVATION ARRAY. ON INPUT THESE VALUES MAY
!                      BE AMBIGUOUS. ON OUTPUT THE OBSERVATION HAS
!                      THE AMBIGUITIES REMOVED.
!   OBSC     I    A    ARRAY OF COMPUTED OBSERVATIONS. THIS ARRAY IS
!                      IS USED TO RESOLVE THE AMBIGUITIES IN THE
!                      OBSERVATION ARRAY.
!   RAMBIG   I    A    SINGLE AMBIGUITY OR ARRAY OF AMBIGUITY VALUES.
!   NM       I    S    NUMBER OF OBSERVATIONS IN THIS BLOCK.
!   LVAMB    I    S    LOGICAL FLAG CONTROLLING WHETHER OR NOT THE
!                      AMBIGUITY IS CONSTANT FOR THE ENTIRE BLOCK OF
!                      OBSERVATIONS. (TRUE - AMBIGUITY VARIES FOR EACH
!                      OBSERVATION; FALSE - AMBIGUITY IS CONSTANT)
!   L2WAY              TRUE IF 2 WAY MEAS
!   AA                 REAL ARRAY
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CBLOKA/FSECBL,SIGNL ,VLITEC,SECEND,BLKDAT(6,4),DOPSCL(5,4)
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
!
      DIMENSION OBS(NM),OBSC(NM),RAMBIG(1),AA(1)
!
!
      DATA HALF/0.5D0/,ONE/1.D0/,TWO/2.D0/
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
! THE AMBIGUITY VALUE IS IN THE SAME SPEED OF LIGHT AS THE OBSERVATION
      AMBIGU=RAMBIG(1)
      DO 1000 N=1,NM
      IF(LVAMB) AMBIGU=RAMBIG(N)
      IF(L2WAY) THEN
        OBSX=OBS(N)*TWO
!       OBSXC=OBSC(N)*TWO
        OBSXC=(OBSC(N)+AA(KSMCOR-1+N))*TWO
      ELSE
        OBSX=OBS(N)
!       OBSXC=OBSC(N)
        OBSXC=OBSC(N)+AA(KSMCOR-1+N)
      ENDIF
      DIFF=OBSXC-OBSX
      IAMBIG=DIFF/AMBIGU+SIGN(HALF,DIFF)
      OBSX=OBSX+DBLE(IAMBIG)*AMBIGU
      IF(L2WAY) THEN
        OBS(N)=OBSX*HALF
      ELSE
        OBS(N)=OBSX
      ENDIF
 1000 END DO
      RETURN
      END
