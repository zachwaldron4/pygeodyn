!$INTRPS
      SUBROUTINE INTRPS(MJDSM,FSECM,S,NM,                               &
     &   INDH,NMH,MINTVL,NINTVL,MJDSEC,FSEC,H,                          &
     &   IBACK)
!********1*********2*********3*********4*********5*********6*********7**
! INTRPS           08/27/82            8208.0    PGMR- TOM MARTIN
!
! FUNCTION:  COMPUTE INTERPOLATION FRACTIONS AND TIME INTERVAL
!            GROUP PARAMETERS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSM    I    S    DATES&INTEGRAL SECONDS OF MEASUREMENT TIMES
!   FSECM    I    A    FRACTIONAL SECONDS OF MEASUREMENT TIMES
!   S        O    A   VECTOR OF INTERPOLATION FRACTIONS
!   NM       I    S    NUMBER OF MEASUREMENT TIMES TO BE INTERPOLATED
!   INDH     O    A   INTEGRATION ARRAY BACK VALUE INDICES FOR
!                     EACH INTERPOLATION GROUP INTERVAL
!   NMH      O    S   NUMBER OF LAST MEASUREMENT IN EACH GROUP
!   MINTVL   I    S    MAX. NUMBER OF ALLOWABLE INTERPOLATION
!                      GROUP INTERVALS
!   NINTVL   O    S   NUMBER OF INTERPOLATION GROUPS
!   MJDSEC   I    S   CURRENT DATE&INTEGRAL SECS. OF INTEG. TIME
!   FSEC     I    S   CURRENT FRACTIONAL SECONDS OF INTEG. TIME
!   H        I    S   INTEGRATION STEPSIZE IN SECONDS
!   IBACK    I    S   NUMBER OF EXTRA BACK VALUES OF
!                     COWELL SUMS
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION FSECM(NM),S(NM),                                        &
     &   INDH(MINTVL),NMH(MINTVL)
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! COMPUTE INTERPOLATION FRACTIONS BASED ON CURRENT INTEGRATOR TIME
!
      OFFSET=DBLE(MJDSM-MJDSEC)
      DO 1000 N=1,NM
      S(N)=(OFFSET+(FSECM(N)-FSEC))/H
 1000 END DO
!
!
! SET INDICES AND INTERPOLATION FRACTION FOR FIRST FRACTION
      IBACK1=IBACK+1
      IBACK3=IBACK+3
      IS=S(1)
      JBACK=IS+IBACK3
      JBACK=MIN(MAX(1,JBACK),IBACK1)
      NINTVL=1
      INDH(1)=JBACK
      SBACK=IBACK1-JBACK
      S(1)=S(1)+SBACK
      NMI=1
      IF(NM.LT.2)GO TO 4000
! LOOP THRU REMAINING MEASUREMENTS
      DO 3000 N=2,NM
      ISN=S(N)
      JBACKN=ISN+IBACK3
      JBACKN=MIN(MAX(1,JBACKN),IBACK1)
      IF(JBACKN.EQ.JBACK)GO TO 2000
! EACH TIME INDICE CHANGES, STORE MEAS. COUNT, INCREMENT INTERVAL COUNT,
!     STORE NEW INDICE, AND RECOMPUTE SHIFT IN INTERPOLATION FRACTION
      NMH(NINTVL)=NMI
      NINTVL=NINTVL+1
      IF(NINTVL.GT.MINTVL)GO TO 5000
      INDH(NINTVL)=JBACKN
      JBACK=JBACKN
      SBACK=IBACK1-JBACK
 2000 CONTINUE
! SHIFT BACK INTERPOLATION FRACTION TO APPROPRIATE BACK VALUE
      S(N)=S(N)+SBACK
      NMI=NMI+1
 3000 END DO
 4000 CONTINUE
! STORE MEAS. COUNT FOR LAST INTERVAL
      NMH(NINTVL)=NMI
      RETURN
 5000 CONTINUE
! PRINT ERROR MESSAGE AND STOP
      NMI=NMI+1
      WRITE(IOUT6,10000)NINTVL,MINTVL,NMI,NM
      WRITE(IOUT6,11000)MJDSEC,FSEC,H,IBACK,JBACKN
      WRITE(IOUT6,12000)(MJDSM,FSECM(I),S(I),I=1,NM)
      WRITE(IOUT6,13000)(INDH(I),NMH(I),I=1,MINTVL)
      STOP 16
10000 FORMAT('0INTRPS--THE NUMBER OF MEASUREMENT',                      &
     &   ' INTERPOLATION INTERVALS ',I10/'0HAS EXCEEDED',               &
     &   ' THE MAXIMUM OF ',I10/                                        &
     &   '0THE ERROR OCCURRED FOR',                                     &
     &   ' MEASUREMENT ',I10,' OF ',I10/1X )
11000 FORMAT('0THE INTERPOLATION CONDITIONS ARE:'/'0',                  &
     &   2X,'MJDSEC',13X,'FSEC',21X,'H',15X,'IBACK',                    &
     &   7X,'JBACKN'/                                                   &
     &   1X,I10,2(2X,G22.14),2(2X,I10))
12000 FORMAT('0MEASUREMENT TIMES AND INTERPOLATION',                    &
     &   ' FRACTIONS ARE:'/                                             &
     &   2(3X,'MJDSM',13X,'FSECM',21X,'S',12X)/                         &
     &   (1X,I10,2(2X,G22.14),2X,I10,2(2X,G22.14)))
13000 FORMAT('0INTERPOLATION INTERVAL PARAMETERS',                      &
     &   ' ARE:'/                                                       &
     &   5(4X,'INDH',8X,'NMH',5X)/                                      &
     &   (10(1X,I10,1X)))
      END
