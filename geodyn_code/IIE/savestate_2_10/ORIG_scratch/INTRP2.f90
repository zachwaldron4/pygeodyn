!$INTRP2
      SUBROUTINE INTRP2(S,B,NM,CIPV,CIVV,IORDER,H,INDH,NMH,NINTVL,      &
     &    SUMX,XDDOT,N3,NH,NSTEPS,XI,MAXM,M3,I3)
!********1*********2*********3*********4*********5*********6*********7**
! INTRP2           82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  INTERPOLATE FOR THE S/C HIGH RATE PORTION OF ORBIT
!            AND ADD TO MAIN PORTION OF ORBIT
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   S        I    A    VECTOR OF INTERPOLATION FRACTIONS
!   B       I/O   A    SCRATCH USED FOR INTERMEDIATE SUMS
!   NM       I    S    NUMBER OF TIMES FOR WHICH INTERPOLATION NEEDED
!   CIPV     I    A    COWELL COEFFICIENTS FOR INTERPOLATING A
!                      VECTOR OF POSITIONS
!   CIVV     I    A    COWELL COEFFICIENTS FOR INTERPOLATING A
!                      VECTOR OF VELOCITIES
!   IORDER   I    S    ORDER OF COWELL INTEGRATION
!   H        I    S    INTEGRATION STEPSIZE
!   INDH     I    A    INDICES OF THE BACK VALUES USED FOR
!                      INTERPOLATING A GROUP OF TIMES
!   NMH      I    A    NUMBER OF THE LAST MEASUREMENT IN EACH
!                      INTERPOLATION GROUP
!   NINTVL   I    S    NUMBER OF INTERPOLATION GROUPS
!   SUMX     I    A    INTEGRATION SUMS
!   XDDOT    I    A    ACCELERATION BACK VALUES
!   N3       I    S    NUMBER OF SATELLITES TIMES 3
!   NH       I    S    NUMBER OF ACCELERATION BACK VALUES
!   NSTEPS   I    S    NUMBER OF COWELL SUM BACK VALUES
!   XI       O    A    INTERPOLATED ORBIT POSITIONS AND VELOCITIES
!   MAXM     I    S    MAX # OF SAT. TO BE INTERPOLATED TIMES 3
!   M3       I    S    NUMBER OF SATELLITES TIMES 3 TO BE STORED
!                      IN THE OUTPUT ARRAY
!   I3       I    S    NUMBER OF SATELLITES TIMES 3 TO BE INTERPOLATED
!                      ON THIS CALL
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CEGREL/LGRELE,LRLCLK,LGRELR,LXPCLK,LBPCLK,NXEGRL
      DIMENSION S(NM),B(NM),CIPV(NM,IORDER),CIVV(NM,IORDER),            &
     &          INDH(NINTVL),NMH(NINTVL),SUMX(N3,2,NH),                 &
     &          XDDOT(N3,NSTEPS),XI(MAXM,M3,2)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      CALL COEFV(S,B,NM,IORDER,CIPV,CIVV)
!
      H2=H**2
      IOL2=IORDER-2
      IOL1=IOL2+1
      NM1=1
      DO 1000 INTVL=1,NINTVL
      NM2=NMH(INTVL)
      INDEX=INDH(INTVL)
!
      DO 900 N=1,I3
!
      DO 100 NMI=NM1,NM2
      B(NMI)=SUMX(N,2,INDEX)
  100 END DO
!
!
      DO 300 K=1,IOL1
      KK=IOL1-K+INDEX
!
      DO 200 NMI=NM1,NM2
      B(NMI)=B(NMI)+CIVV(NMI,K)*XDDOT(N,KK)
  200 END DO
!
!
  300 END DO
!
      DO 400 NMI=NM1,NM2
      XI(NMI,N,2)=XI(NMI,N,2)+B(NMI)*H
  400 END DO
!
!
      DO 500 NMI=NM1,NM2
      B(NMI)=SUMX(N,1,INDEX)-SUMX(N,2,INDEX)                            &
     &      +S(NMI)*SUMX(N,2,INDEX)
  500 END DO
!
!
      DO 700 K=1,IOL2
      KK=IOL1-K+INDEX
!
      DO 600 NMI=NM1,NM2
      B(NMI)=B(NMI)+CIPV(NMI,K)*XDDOT(N,KK)
  600 END DO
!
!
  700 END DO
!
      DO 800 NMI=NM1,NM2
      XI(NMI,N,1)=XI(NMI,N,1)+B(NMI)*H**2
  800 END DO
!
!
  900 END DO
      NM1=NM2+1
 1000 END DO
      RETURN
      END
