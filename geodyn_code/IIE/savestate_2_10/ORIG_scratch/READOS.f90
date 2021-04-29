!
!$READOS
      SUBROUTINE READOS(IUNT,INTOSP,TMOS0,TMOS,TMOSP,                  &
     & SUMXOS,SUMPOS,XDDTOS,PDDTOS,NEQNI,KNSTEPS,KNST,NN,NN2,HOS,ISAT)
!********1*********2*********3*********4*********5*********6*********7**
! READOS           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  CALLED BY ORBSET READS THE RAPID FILES
!            INTERPOLATED
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT     I    S    READING UNIT
!   INTOSP   I    S    BUFFER NUMBER
!   KNSTEPS  I    S    HOW MANY STEPS ARE ALLOWED IN A BUFFER FOR THIS SATELLITE
!   TMOS0    I    A    TIME OF THE IOL1th DATA STORAGE
!   TMOS     I    A    TIME OF THE LATEST DATA STORAGE IN ORBIT
!   TMOSP    I    A    TMOS FOR PARTIALS
!   SUMXOS,SUMPOS,XDDTOS,PDDTOS I A SUMS AND ACCELLERATIONS FOR RAPID INTEGRAT.
!   NEQNI    I    A    FORCE MODEL EQUATIONS FOR EACH SATELLITE PARTICIPATING
!                      IN RAPID INTEGRATION.
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION TMOS0(1),TMOS(1),TMOSP(1),HOS(1)
      DIMENSION SUMXOS(3,2,1)
      DIMENSION SUMPOS(NEQNI,3,2,1)
      DIMENSION XDDTOS(3,KNSTEPS,1)
      DIMENSION PDDTOS(NEQNI,3,KNSTEPS,1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      READ(IUNT,END=3000) TMOS0(1),TMOS(1),TMOSP(1), &
     &         NEQNI,HOS(1)


      READ(IUNT,END=3000) (SUMXOS(IQP,1,1),IQP=1,6)

      DO IQP=1,KNST
        READ(64,END=3000) XDDTOS(1,IQP,1),XDDTOS(2,IQP,1),  &
     &           XDDTOS(3,IQP,1)
      ENDDO

      READ(64,END=3000) (SUMPOS(IQP,1,1,1),IQP=1,NN2)

      DO IQP=1,KNST
        READ(64) (PDDTOS(JQP,1,IQP,1),JQP=1,NN)
      ENDDO

3000  CONTINUE
      RETURN
      END
