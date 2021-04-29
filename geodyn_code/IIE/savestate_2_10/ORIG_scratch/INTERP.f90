!
!$INTERP
      SUBROUTINE INTERP(IWINDO,DMIN,H,PHIS,DLAW,DDFI,DDLA,NPHI,NDLA,    &
     &                  IPDIM,ILDIM,PHI,DLA,VALINT)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!     SUBROUTINE FOR INTERPOLATION OF VALUES FROM A STANDARD DTM-GRID  C
!     TO INDIVIDUAL STATION LOCATIONS.                                 C
!                                                                      C
!                                                                      C
!     INPUT PARAMETERS...                                              C
!     ===================                                              C
!     IWINDO...    A SPLINE WINDOW OF SIZE 'IWINDO' X 'IWINDO' WILL BE C
!                  USED AROUND EACH STATION. IF 'IWINDO' IS 0 OR 1,    C
!                  BILINEAR INTERPOLATION WILL BE USED.                C
!     DMIN...      MINIMUM ACCEPTABLE DISTANCE FROM THE GRID EDGE IN   C
!                  KM (USEFUL FOR FFT GRIDS).                          C
!     H...         2D DATA ARRAY (ELEMENT (1,1) IN SW CORNER).         C
!     PHIS,DLAW... LATITUDE AND LONGITUDE OF SW GRID POINT.            C
!     DDFI,DDLA... GRID SPACING IN LATITUDE AND LONGITUDE DIRECTION.   C
!     NPHI,NDLA... NUMBER OF GRID POINTS IN LATITUDE AND LONGITUDE     C
!                  DIRECTION.                                          C
!     IPDIM,ILDIM..DIMENSIONS OF 2D DATA ARRAY 'H' AS DECLARED IN THE  C
!                  CALLING PROGRAM.                                    C
!     PHI,DLA...   LATITUDE AND LONGITUDE OF INTERPOLATION POINT.      C
!                                                                      C
!                                                                      C
!     OUTPUT PARAMETERS...                                             C
!     ====================                                             C
!     VALINT...    INTERPOLATED VALUE.                                 C
!                                                                      C
!                                                                      C
!     EXECUTION TIME ON CDC 990 IS...                                  C
!     ===============================                                  C
!     +------------------+-------------------+-------------------+     C
!     I  INTERPOLATION   I  OPT=LOW          I  OPT=HIGH         I     C
!     I------------------I-------------------I-------------------I     C
!     I  BILINEAR        I  1.44 MSEC/STAT.  I  1.44 MSEC/STAT.  I     C
!     I  3 X 3 SPLINE    I  1.53 MSEC/STAT.  I  1.51 MSEC/STAT.  I     C
!     I  5 X 5 SPLINE    I  1.70 MSEC/STAT.  I  1.67 MSEC/STAT.  I     C
!     I  7 X 7 SPLINE    I  2.02 MSEC/STAT.  I  1.74 MSEC/STAT.  I     C
!     I  9 X 9 SPLINE    I  2.31 MSEC/STAT.  I  2.00 MSEC/STAT.  I     C
!     +------------------+-------------------+-------------------+     C
!                                                                      C
!                                                                      C
!     PROGRAM CREATION BY...   H. DENKER          MAY 30, 1987         C
!                              H. DENKER          MARCH 13, 1989       C
!                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER(IPA1=20)
      LOGICAL LODD
      REAL H(IPDIM,ILDIM)
      DIMENSION A(IPA1),R(IPA1),Q(IPA1),HC(IPA1)
      IDIM1=IPA1
      RHO=57.29577951D0
      REARTH=6374000.D0
      IF(IWINDO.LT.2) IWINDO=2
      IF(IWINDO.GT.IDIM1) IWINDO=IDIM1
      ILIM=DMIN*1000.*RHO/(REARTH*DDFI)
      JLIM=DMIN*1000.*RHO/(REARTH*DDLA*COS((PHIS+DDFI*NPHI/2.)/RHO))
      LODD=(IWINDO/2)*2.NE.IWINDO
!     print *,'INTERP*PHI,PHIS,DLA,DALW',PHI,PHIS,DLA,DLAW,DDFI,DDLA
!     print *,'INTERP*ILIM,JLIM',ILIM,JLIM,NPHI,NDLA
      RI=(PHI-PHIS)/DDFI
      RJ=(DLA-DLAW)/DDLA
      IF(LODD) THEN
        I0=RI-0.5
        J0=RJ-0.5
      ELSE
        I0=RI
        J0=RJ
      ENDIF
      I0=I0-IWINDO/2+1
      J0=J0-IWINDO/2+1
      II=I0+IWINDO-1
      JJ=J0+IWINDO-1
!     print *,'*INTERP*I0,J0,II,JJ*',I0,J0,II,JJ
      IF(I0.LT.0 .OR. II.GE.NPHI .OR. J0.LT.0 .OR. JJ.GE.NDLA) THEN
!       WRITE(6,7008) PHI,DLA
        VALINT=999999.
        RETURN
      ELSEIF(I0.LT.ILIM .OR. II.GT.NPHI-ILIM .OR. J0.LT.JLIM .OR.       &
     &  JJ.GT.NDLA-JLIM) THEN

!       ....NPOINT and ILIST only appear in the statement below
!       ....and are not defined
!!        IF(NPOINT.LE.ILIST) WRITE(6,7009) PHI,DLA     ! jjm 9/98

        VALINT=999999.
        RETURN
      ENDIF
 7008 FORMAT(' ',2F10.6,' STATION TOO NEAR GRID BOUNDARY  - NO INT.'    &
     &,' POSSIBLE|')
 7009 FORMAT(' ',2F10.6,' STATION OUTSIDE ACCEPTABLE AREA - NO INT.'    &
     &,' PERFORMED|')
      IF(IWINDO.GT.2) THEN
        DO 110 I=1,IWINDO
          DO 111 J=1,IWINDO
            A(J)=H(I0+I,J0+J)
  111     CONTINUE
          CALL INITSP(A,IWINDO,R,Q)
          HC(I)=SPLINE(RJ-J0+1.,A,IWINDO,R)
  110   CONTINUE
        CALL INITSP(HC,IWINDO,R,Q)
        VALINT=SPLINE(RI-I0+1.,HC,IWINDO,R)
      ELSE
        VALINT=BILIN(RI+1.,RJ+1.,H,NPHI,NDLA,IPDIM,ILDIM)
      ENDIF
      RETURN
      END
