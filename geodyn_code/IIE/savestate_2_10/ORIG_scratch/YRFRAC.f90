!$YRFRAC
      SUBROUTINE YRFRAC(AA,DYDE,KOBCOR,LPRE,LSWTCH,NM)
!********1*********2*********3*********4*********5*********6*********7**
! YRFRAC           00/00/00            8804.0    PGMR - TVM
!
! FUNCTION: SCALE THE REFRACTION IN ELEVATION BY THE PARTIAL OF Y-ANGLE
!           W.R.T. ELEVATION TO COMPUTE THE Y-ANGLE REFRACTION AND LOAD
!           INTO APPROPRIATE LOCATIONS IN THE OBS. CORRECTIONS RECORD.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA                 BASE ADDRESS OF DYNAMIC ARRAY SPACE. WHEN USED
!                      WITH APPROPRIATE OFFSET (KOBCOR) PROVIDES
!                      ACCESS TO OBSERVATION CORRECTIONS RECORDS.
!   DYDE               PARTIAL OF Y-ANGLE W.R.T. ELEVATION
!   KOBCOR             POINTERS INTO THE OBSERVATION CORRECTIONS RECORD
!   LPRE               SWITCHES INDICATING WHICH CORRECTIONS ARE TO BE
!                      COMPUTED
!   LSWTCH             SWITCHES INDICATING PRESENCE OF SPACE IN
!                      OBSERVATION CORRECTIONS RECORD FOR THESE
!                      CORRECTIONS
!   NM                 NUMBER OF OBSERVATIONS TO BE CORRECTED.
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION AA(1)
      DIMENSION DYDE  (NM)
      DIMENSION KOBCOR(   9),LPRE(  24),LSWTCH(10)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO  2000 I1=3,7,2
      I2=I1+1
      IF(LPRE  (I1)) GO TO  2000
      IF(.NOT.LSWTCH(I2)) GO TO  2000
!...CHAIN DY/DE * DELTA E DRY, WET OR IONO AND STORE IN OBS BUFFER
      JOBCOR=KOBCOR(I2)
      DO  1000 N=1,NM
      AA(JOBCOR)=AA(JOBCOR)*DYDE (N)
      JOBCOR=JOBCOR+1
 1000 END DO
 2000 END DO
      RETURN
      END
