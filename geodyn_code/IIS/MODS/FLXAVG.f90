!$FLXAVG
      SUBROUTINE FLXAVG(FLXS,INTVAL,NUMFLX,FLXVG)
!********1*********2*********3*********4*********5*********6*********7**
! FLXAVG           07/18/83            8307.01   PGMR - DIANE SEVITSKI
!
! FUNCTION:  MAKE A MOVING AVERAGE OF FLXS ARRAY
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   FLXS     I         FLUX ARRAY
!   INTVAL   I         AVERAGING INTERVAL
!   NUMFLX   I         NUMBER OF FLUXES
!   FLXVG    O         ARRAY OF MOVING AVERAGES
!
!*********1*********2*********3*********4*********5*********6*********7*
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/BIHTBL/IA1UTC,JA1UTC,IPOLA1,JPOLA1,IFLUXS,JFLUXS,IFLXAP,   &
     &              JFLXAP,IFLXKP,JFLXKP,IIONO,JIONO,                   &
     &              NVR(6),NUTC  ,IBIH  ,JBIH  ,                        &
     &              IFLUX ,JFLUX ,IBHSYS,IBHDAT,IBHTIM,IFLXSF,          &
     &              IBIHTL(2),IBIHHR,JBIHHR,NXBIHT
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY,XCONST
      COMMON/UNITS /IUNT01,IUNT02,IUNT03,IUNT05,IUNT06,IUNT07,IUNT11,   &
     &              IUNT12,IUNT15,IUNT16,                               &
     &              IUNT30,IUNT31,IUNT39,IUNT40,IUNT41,IUNT42,IUNT43,   &
     &              IUNT50,IUNT52,IUNT53,IUNT60,IUNT90,IUNT91,IUNT99,   &
     &              ILINE(2),IPAGE(2),MLINEP(2),MAXCOL(2),IUNT17,       &
     &              IUNT70,IUNT71,                                      &
     &              IUNT88,NXUNIT
      DIMENSION FLXS(1),FLXVG(1)
      DATA C0P0/0.0D0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
! IF THE FIRST VALUE IN THE AVERAGE FLUX ARRAY IS GREATER THEN ZERO,
! THEN A USER SPECIFIED AVERAGE WAS INPUT, THUS DO NOT COMPUTE AVERAGES
      IF(FLXVG(1).GT.C0P0) GOTO 490
      XINTVL=INTVAL
      IMID=(INTVAL/2)+1
      F0=C0P0
      IF((IATDEN.EQ.42).OR.(IATDEN.EQ.52).OR.(IATDEN.EQ.62)) GOTO 420
      DO 100 I=1,INTVAL
      F0=F0+FLXS(I)
  100 END DO
      FAVG=F0/XINTVL
      DO 200 I=1,IMID
      FLXVG(I)=FAVG
  200 END DO
!
      JX=NUMFLX-(IMID-1)
      IX=IMID+1
      DO 300 I=IX,JX
      J=I-IMID
      F0=F0-FLXS(J)
      J=J+INTVAL
      F0=F0+FLXS(J)
      FLXVG(I)=F0/XINTVL
  300 END DO
      FAVG=FLXVG(JX)
      JX=JX+1
      DO 400 I=JX,NUMFLX
      FLXVG(I)=FAVG
  400 END DO
      GOTO 490
  420 CONTINUE
! DETERMINE END OF INTERVAL AVERAGE
      FLXSUM=C0P0
      DO 430 I2=1,INTVAL
      FLXSUM=FLXSUM+FLXS(I2)
  430 END DO
      FAVG=FLXSUM/XINTVL
      DO 440 I2=1,INTVAL
      FLXVG(I2)=FAVG
  440 END DO
      INTV1=INTVAL+1
      DO 450 I2=INTV1,NUMFLX
      FLXSUM=FLXSUM-FLXS(I2-INTVAL)+FLXS(I2)
      FLXVG(I2)=FLXSUM/XINTVL
  450 END DO
!
!     ** SCALE **
!
  490 CONTINUE
      CALL MJDYMD(MJDX,IFLUX,IDUM,2)
      T=MJDX-15020.D0-1.D0
      DO 700 I=1,NUMFLX
      T=T+1.0D0
      TJ=T/365.25D2
      XM=358.47583D0+.9856002670D0*T-1.5D-4*TJ**2-3.0D-6*TJ
      E=1.675104D-2-4.180D-5*TJ-1.26D-7*TJ**2
      XM=MOD(XM,360.0D0)*DEGRAD
      EC1=XM
      DO 600 J=1,5
      EC=XM+E*SIN(EC1)
      IF(ABS(EC-EC1).LE.1.0D-5) GO TO 500
  500 EC1=EC
  600 AU=1.0D0-E*COS(EC)
      FLXS(I)=FLXS(I)/AU**2
  700 FLXVG(I)=FLXVG(I)/AU**2
  800 CONTINUE
      RETURN
      END
