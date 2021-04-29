!$TROPMM
      FUNCTION TROPMM(MODELT,P,TDK,WVP,SINE,COSE,EDOT,FLAM,COSLT2,FPOS, &
     &   WETT)
!********1*********2*********3*********4*********5*********6*********7**
! TROPMM           00/00/00            0000.0    PGMR - ?
!
! FUNCTION:  MARINI-MURRAY TROPOSHPERE CORRECTION FORMULAE(METERS)
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MODELT
!   P
!   TDK
!   WVP
!   SINE
!   COSE
!   EDOT
!   FLAM
!   COSLT2
!   FPOS
!   WETT
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DATA ONE/1.0D0/,TWO/2.0D0/,THREE/3.0D0/
      DATA C10M2/1.0D-2/
      DATA CAP/0.002357D0/,CAWVP/0.000141D0/
      DATA CFK0/1.163D0/,CFKL2/-9.68D-3/,CFKT/-1.04D-3/,                &
     &     CFKP/1.435D-5/
      DATA CBP/1.084D-8/,CBP2/4.734D-8/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! MISC. TERMS
      D=CAP*P
      A=  D   + CAWVP*WVP
      FK=CFK0 + CFKL2*COSLT2 + CFKT*TDK + CFKP*P
      B=CBP*P*TDK*FK + CBP2*P*P*TWO / (TDK*(THREE-ONE/FK))
!*** DELAY
      AB   =A+B
      BAB  =B/AB
      DENOM=SINE+(BAB/(SINE+C10M2))
      DELR =FLAM/FPOS  *  AB/DENOM
      DB   =D+B
      BDB  =B/DB
      DENOMD=SINE+(BDB/(SINE+C10M2))
      DRYT  =FLAM/FPOS  *  DB/DENOMD
      WETT  =DELR-DRYT
      TROPMM=DRYT
      IF(MODELT.LE.1) RETURN
!***DELAY RATE
      DELRR  = -EDOT * (DELR/DENOM ) * (ONE+BAB)*COSE
      DRYT   = -EDOT * (DRYT/DENOMD) * (ONE+BDB)*COSE
      WETT   = DELRR - DRYT
      TROPMM = DRYT
      RETURN
      END
