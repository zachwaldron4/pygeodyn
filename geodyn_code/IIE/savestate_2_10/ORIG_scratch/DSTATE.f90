      SUBROUTINE DSTATE(MJDSEC,FSEC,IORDER,IOL1,IOL2,H,H2,SUMX,XDDOT,XP,&
     &               NSAT,N3,CPP,CPV,CCP,                               &
     &               NXSTAT,TXSTAT,                                     &
     &               DELXST,IPXST,HREMAL,LXSTAT,JPXST,                  &
     &               DSROT)
!********1*********2*********3*********4*********5*********6*********7**
! DSTATE
!
! FUNCTION:  NUMERICAL INTEGRATION OF SMALL STEP STATE AT "INBETWEEN"
!            STEPS AND PREDICTION OF SMALL STEP STATE AT LARGE STEP
!
! I/O PARAMETERS:
!
!   MJDSEC   I    S    CURRENT DATE AND INTEGRAL SECONDS OF TIME
!                      OF LARGE STEP ORBIT INTEGRATION
!   FSEC     I    S    CURRENT FRACTIONAL SECONDS FROM MJDSEC
!   IORDER   I    S    ORDER OF COWELL INTEGRATION FOR ORBIT
!   IOL1     I    S    IORDER-1
!   IOL2     I    S    IORDER-2
!   H        I    S    INTEGRATION LARGE STEP SIZE IN SECONDS
!   H2       I    S    H*H
!   SUMX     I&O  A    INTEGRATION SUMS FOR ORBIT
!   XDDOT    I    A    ORBIT ACCELERATIONS
!   XP       I&O  A    PREDICTRD SATELLITE STATE
!   NSAT     I    A    NUMBER OF SATELLITES BEING INTEGRATED IN THIS
!                      CALL
!   N3       I    S    NSAT*3
!   CPV      I    S    COWELL COEFFICIENTS FOR PREDICTING VELOCITY
!   CCP      I    A    COWELL COEFFICIENTS FOR CORRECTING POSITION
!   NXSTAT   I    S    NUMBER OF DSTATE EPOCHS
!   TXSTAT   I    A    DSTATE EPOCHS IN ELAPSED TIME SECONDS SINCE
!                      JD 2430000.5
!   DELXST   I&O  A    ARRAY OF DELSTATE VALUES.ID YJESE ARE INPUT
!                      IN LOCAL ORBIT PLANE, THEY ARE ROTATED TO
!                      TRUE OF REFERENCE. THIS ARRAY IS REINITIALIZED
!                      IN INITI EVERY ITERATION.
!   IPXST    I    S    STARTING LOCATION IN THE FORCE MODEL EXPLICIT
!                      PARTIAL AEEAY FOR DELTA STATES
!   HREMAL   O    S    FRACTION BASED ON WHERE DELTA STATE FALL WRT
!                      CLOSEST STEP SIZE
!   LXSTAT   O    S    SET TO FALSE, THEN TO TRUE IF A DSTATE OCCURS
!                      AT CURRENT STEP
!   JPXST    O    S    SET TO ZERO, IF THERE IS A DELTA STATE AT
!                      CURRENT STEP SET TO LOCATION OF THAT PARAMETER
!                      IN THE EXPLICIT PARTIAL ARRAY
!   DSROT   I&O   A    ON INPUT THE IDENITY MATRIX. IF THERE IS A
!                      DSTATE AT THE CURRENT STEP AND IF THAT DSTATE
!                      IS IN THE LOCAL ORBIT COORDINTE SYSTEM, A ROTATION
!                      MATRIX TO GO TO TRUE OF REFERENCE IS OUTPUT
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      COMMON/IDELTX/MXSTAT,NEVENTS,NDSTAT,IDSYST,NDELTX
      DIMENSION SUMX(N3,2),                                             &
     &   XDDOT(N3,IOL1)
      DIMENSION CPP(IORDER),CPV(IORDER)
      DIMENSION TXSTAT(MXSTAT),DELXST(6,MXSTAT,NSAT)
      DIMENSION XP(3,NSAT,2),DSROT(3,3,NSAT)
      DIMENSION ARY(3,2)
      DIMENSION DUM(3)
!
!
      JPXST=0
      LXSTAT=.FALSE.
      IF(NXSTAT.LE.0) RETURN
      IF(H.GT.1.0001D0) THEN
        WRITE(6,6000)
        WRITE(6,6001)
        WRITE(6,6002) H
        WRITE(6,6003)
        STOP
      ENDIF
!
!
!
      IF(IDSYST.EQ.2) THEN
! CALL ORBTOR TO GET THE ROTATION MATRIX BASED ON PREDICTED SAT POSITION
        CALL ORBTOR(XP(1,1,1),XP(1,1,2),DSROT,NSAT)
      ENDIF
!
!  CODE FOR DELTA STATE
!
      TIME1=DBLE(MJDSEC)+FSEC
      TIME2=TIME1
      LAT2=.FALSE.
      DO 100 IQP=1,NXSTAT
         IPDXST=IQP
         TDIF=ABS(TIME2-TXSTAT(IQP))
         IF(TDIF.LT..5D0) THEN
           LAT2=.TRUE.
!!!!       HREMAL=0.D0
           HREMAL=TIME2-TXSTAT(IQP)
           GO TO 110
         ENDIF
  100 END DO
  110 CONTINUE
      IF(LAT2.AND..NOT.LXSTAT) THEN

                  IF(IDSYST.EQ.2) THEN
!  ROTATE DELXST TO TOR BEFORE CORRECTING XP IF IDSYST=2
         DO IQP=1,NSAT
         ARY(1,1)=DSROT(1,1,IQP)*DELXST(1,IPDXST,IQP)+                  &
     &            DSROT(1,2,IQP)*DELXST(2,IPDXST,IQP)+                  &
     &            DSROT(1,3,IQP)*DELXST(3,IPDXST,IQP)
         ARY(2,1)=DSROT(2,1,IQP)*DELXST(1,IPDXST,IQP)+                  &
     &            DSROT(2,2,IQP)*DELXST(2,IPDXST,IQP)+                  &
     &            DSROT(2,3,IQP)*DELXST(3,IPDXST,IQP)
         ARY(3,1)=DSROT(3,1,IQP)*DELXST(1,IPDXST,IQP)+                  &
     &            DSROT(3,2,IQP)*DELXST(2,IPDXST,IQP)+                  &
     &            DSROT(3,3,IQP)*DELXST(3,IPDXST,IQP)
         ARY(1,2)=DSROT(1,1,IQP)*DELXST(4,IPDXST,IQP)+                  &
     &            DSROT(1,2,IQP)*DELXST(5,IPDXST,IQP)+                  &
     &            DSROT(1,3,IQP)*DELXST(6,IPDXST,IQP)
         ARY(2,2)=DSROT(2,1,IQP)*DELXST(4,IPDXST,IQP)+                  &
     &            DSROT(2,2,IQP)*DELXST(5,IPDXST,IQP)+                  &
     &            DSROT(2,3,IQP)*DELXST(6,IPDXST,IQP)
         ARY(3,2)=DSROT(3,1,IQP)*DELXST(4,IPDXST,IQP)+                  &
     &            DSROT(3,2,IQP)*DELXST(5,IPDXST,IQP)+                  &
     &            DSROT(3,3,IQP)*DELXST(6,IPDXST,IQP)
         DELXST(1,IPDXST,IQP)=ARY(1,1)
         DELXST(2,IPDXST,IQP)=ARY(2,1)
         DELXST(3,IPDXST,IQP)=ARY(3,1)
         DELXST(4,IPDXST,IQP)=ARY(1,2)
         DELXST(5,IPDXST,IQP)=ARY(2,2)
         DELXST(6,IPDXST,IQP)=ARY(3,2)
         ENDDO
                  ENDIF

         JPXST=IPXST+(IPDXST-1)*6
         DO IQP=1,NSAT
         MQP=0
         DO JQP=1,2
         DO KQP=1,3
           MQP=MQP+1
           XP(KQP,IQP,JQP)=XP(KQP,IQP,JQP)+DELXST(MQP,IPDXST,IQP)
           IF(JQP.EQ.1) THEN
             XP(KQP,IQP,JQP)=XP(KQP,IQP,JQP)                            &
     &      +HREMAL*DELXST(MQP+3,IPDXST,IQP)
           ENDIF
         ENDDO
         ENDDO
         ENDDO
         CALL UCSSS(XP,N3,IORDER,IOL1,IOL2,CPP,CPV,H,H2,                &
     &              SUMX,XDDOT)
      ENDIF
!
!
!
      LXSTAT=LXSTAT.OR.LAT2
!
      RETURN
 6000 FORMAT(' EXECUTION TERMIATING IN ROUTINE DSTATE')
 6001 FORMAT(' DSTATE OPTION BEING USED WITH INTGCB')
 6002 FORMAT(' WHEN STEPSIZE: ',F20.3)
 6003 FORMAT(' IS LARGER THAN 1.0 ')
      END
