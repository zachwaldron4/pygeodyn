!$OLOAD
      SUBROUTINE OLOAD(MJDSEC,FSEC,NM,ABCOF,ANGFAC,SPEED,XLOCV,         &
     &                 XYZC,TRIGOL,SCR2,LADJOL,PXSPAB,NEAJ,NNAJ,        &
     &                 NVAJ,IPAJE,IPAJN,IPAJV ,ABCCM ,PXSPA1,NXCJ,NYCJ, &
     &                 NZCJ,IPAXC,IPAYC,IPAZC ,ABCEO ,PXSPA2,NXPJ,NYPJ, &
     &                 NUPJ,IPAXP,IPAYP,IPAUP ,LOLD,LXYC,LEOP,          &
     &                 isnumb,LNEG,NDXYZC,LALOAD,ALCOEF,ISITE,ISITA,    &
     &                 XENV)
!********1*********2*********3*********4*********5*********6*********7**
! OLOAD            00/00/00            8604.0    PGMR - ?
!
! FUNCTION:  COMPUTES OCEAN LOADING DISPLACEMENT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I    S    START BLOCK SATELLITE INTEGER TIME
!   FSEC     I    A    FRACTIONAL SECONDS OF BLOCK MEASUREMENT TIMES
!   NM       I    S    NUMBER OF OBSERVATIONS IN THE BLOCK
!   ABCOF    O    S    FREQUENCIES FOR OCEAN LOADING
!   ANGFAC   I    A    FACTORS TO MULTIPLY EACH OF THE 5 ARGUMENTS BY
!                      PLUS A -/+ PHASE FOR CERTAIN DOODSON NUMBERS
!   SPEED    I    A    RATE OF ANGULAR ARGUMENT FOR EACH FREQUENCY
!   XLOCV    I    A    OCEAN LOADING PARAMETERS COVARIANCE MATRIX
!   XYZC     O    A    X,Y,Z FOR OCEAN LOADING
!   TRIGOL  I/O   A    COSINES AND SINES OF OCEAN LOADING FREQURNCIES
!                      AT BLOCK START TIME (IN SOME CASES MODIFIED
!                      FOR SIDE BANDS). ALSO DERIVATIVES WRT OF
!                      THE COSINE AND SINES. FIRST TWO SLOTS ARE
!                      DERIVATIVES. COSINES AND SINES IN SKOTS 3&4
!                      IF LNEG=.TRUE., THIS ROUTINE FLIPS SIGN
!                      OF TRIGOL
!   SCR2    I/O   A    SCRATCH ARRAY FOR INTERMEDIATE CALCULATION
!   LADJOL   I    S    .TRUE. IF OCEAN LOADING IS TO BE ADJUSTED
!   PXSPAB   O    A    THE PART OF PMPA FOR OCEAN LOADING
!   NEAJ     I    S    POINTER TO OCEAN LOADING EAST INFO FROM
!                      COMMON/OLDADJ/
!   NNAJ     I    S    POINTER TO OCEAN LOADING NORTH INFO FROM
!                      COMMON/OLDADJ/
!   NVAJ     I    S    POINTER TO OCEAN LOADING VERTICAL INFO FROM
!                      COMMON/OLDADJ/
!   IPAJE    I    A    POINTER TO EAST INFO OF IOLDPA ARRAY
!   IPAJN    I    A    POINTER TO NORTH INFO OF IOLDPA ARRAY
!   IPAJV    I    A    POINTER TO VERTICAL INFO OF IOLDLPA ARRAY
!   LALOAD   I    S    .TRUE. IF THIS SITE MODELS ATMOSPHERIC PRESSURE L
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION ANGFAC(NTOLFR,6),SPEED(NTOLFR),XLOCV(3,3)
      DIMENSION TRIGOL(NTOLFR,4),ABCOF(NTOLFR,2,3)
      DIMENSION ABCCM(NTOLFR,2,3)
      DIMENSION ABCEO(NTOLFR,2,3)
      DIMENSION IPAJE(*),IPAJN(*),IPAJV(*)
      DIMENSION IPAXC(*),IPAYC(*),IPAZC(*)
      DIMENSION IPAXP(*),IPAYP(*),IPAUP(*)
      DIMENSION SCR2(NM),XYZC(NDXYZC,3),FSEC(NM),PXSPAB(NM,3,*)
      DIMENSION XENV(3,3)
!>>>>>>>>>>>
!
      PARAMETER ( NMOLOD = 100 )
!c      DIMENSION stacor(NMOLOD,3)
      dimension xtnoet(3,NMOLOD), xtet(3,NMOLOD), dummy(3,3)
!
      DIMENSION PXSPA1(NM,3,*)
      DIMENSION PXSPA2(NM,3,*)
      DIMENSION ALCOEF(ITOTAL,3,MAXAPL),ISITE(ITOTAL)
!
      COMMON/LOLOAD/LOLMD,LOLAJ,L2FOLT,LAPLOD
      COMMON/NALOAD/NSITES,NALFIL,NALSIT,MAXSIT,ITOTAL,MAXAPL,ITAPLB,   &
     &              ITAPLE,IAPLDT,NXLOAD
      COMMON/OLDMOD/NTOLFR,NTOLF2,NTOLMD,NSTAOL,NSITOL,NTOLAJ,MXOLSA,   &
     &              NPOLSH,                                             &
     &              NXOLMD
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
!
      DATA ZERO/0.D0/
!
      DATA kentry /0/
      DATA iprint /2/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      I0OFF=ABS(FSEC(1))
      IF(FSEC(1).LT.0.D0) I0OFF=-I0OFF
      FSCO=-I0OFF

! FOR NEG CALLS FLIP THE SIGNS ON TRIG FUNCTIONS
!
      IF(LNEG) THEN
         DO 130 N=1,NTOLF2
         TRIGOL(N,1)=-TRIGOL(N,1)
         TRIGOL(N,3)=-TRIGOL(N,3)
  130    CONTINUE
      ENDIF
!
! FIGURE DELTX,DELTY,DELTZ AT START OF BLOCK
      DELTX0=ZERO
      DELTY0=ZERO
      DELTZ0=ZERO
      IF(LOLD.AND..NOT.LXYC) THEN
         DO 135 N=1,NTOLF2
         DELTX0=DELTX0+ABCOF(N,1,1)*TRIGOL(N,3)
         DELTY0=DELTY0+ABCOF(N,1,2)*TRIGOL(N,3)
         DELTZ0=DELTZ0+ABCOF(N,1,3)*TRIGOL(N,3)
  135    CONTINUE
      ENDIF
      IF(.NOT.LOLD.AND.LXYC) THEN
         DO 140 N=1,NTOLF2
         DELTX0=DELTX0+ABCCM(N,1,1)*TRIGOL(N,3)
         DELTY0=DELTY0+ABCCM(N,1,2)*TRIGOL(N,3)
         DELTZ0=DELTZ0+ABCCM(N,1,3)*TRIGOL(N,3)
  140    CONTINUE
      ENDIF
      IF(LOLD.AND.LXYC) THEN
         DO 145 N=1,NTOLF2
         DELTX0=DELTX0+(ABCOF(N,1,1)+ABCCM(N,1,1))*TRIGOL(N,3)
         DELTY0=DELTY0+(ABCOF(N,1,2)+ABCCM(N,1,2))*TRIGOL(N,3)
         DELTZ0=DELTZ0+(ABCOF(N,1,3)+ABCCM(N,1,3))*TRIGOL(N,3)
  145    CONTINUE
      ENDIF
      DXDT=ZERO
      DYDT=ZERO
      DZDT=ZERO
      IF(LOLD.AND..NOT.LXYC) THEN
         DO 155 N=1,NTOLF2
         DXDT=DXDT+ABCOF(N,1,1)*TRIGOL(N,1)
         DYDT=DYDT+ABCOF(N,1,2)*TRIGOL(N,1)
         DZDT=DZDT+ABCOF(N,1,3)*TRIGOL(N,1)
  155    CONTINUE
      ENDIF
      IF(.NOT.LOLD.AND.LXYC) THEN
         DO 160 N=1,NTOLF2
         DXDT=DXDT+ABCCM(N,1,1)*TRIGOL(N,1)
         DYDT=DYDT+ABCCM(N,1,2)*TRIGOL(N,1)
         DZDT=DZDT+ABCCM(N,1,3)*TRIGOL(N,1)
  160 END DO
      ENDIF
      IF(LOLD.AND.LXYC) THEN
         DO 165 N=1,NTOLF2
         DXDT=DXDT+(ABCOF(N,1,1)+ABCCM(N,1,1))*TRIGOL(N,1)
         DYDT=DYDT+(ABCOF(N,1,2)+ABCCM(N,1,2))*TRIGOL(N,1)
         DZDT=DZDT+(ABCOF(N,1,3)+ABCCM(N,1,3))*TRIGOL(N,1)
  165    CONTINUE
      ENDIF
!
! CORRECT XYZ FOR OCEAN LOADING
!
!      ....set flag for printing station height oload corrections
       lolshp = NPOLSH .gt. 0
!
       if( lolshp ) then
         do 3001 iii=1,3
            do 3011 jjj=1,nm
               xtnoet(iii,jjj) = XYZC(jjj,iii)
 3011       continue
 3001    continue
      endif
!
      DO 170 N=1,NM
         FSCN=FSEC(N)+FSCO
         SCR2(N)=DELTX0+DXDT*FSCN
!       if( lolshp ) then
!         stacor(N,1) = scr2(N)
!      endif
         XYZC(N,1)=XYZC(N,1)+SCR2(N)
  170 END DO
      DO 180 N=1,NM
         FSCN=FSEC(N)+FSCO
         SCR2(N)=DELTY0+DYDT*FSCN
!       if( lolshp ) then
!         stacor(N,2) = scr2(N)
!      endif
         XYZC(N,2)=XYZC(N,2)+SCR2(N)
  180 END DO
      DO 190 N=1,NM
         FSCN=FSEC(N)+FSCO
         SCR2(N)=DELTZ0+DZDT*FSCN
!       if( lolshp ) then
!         stacor(N,3) = scr2(N)
!      endif
         XYZC(N,3)=XYZC(N,3)+SCR2(N)
  190 END DO
!
       if( lolshp ) then
         do 3002 iii=1,3
            do 3012 jjj=1,nm
               xtet(iii,jjj) = XYZC(jjj,iii)
 3012       continue
 3002    continue
!--------------------------------------------------------------
!>>>>>>>>>>>>>>
         kentry = kentry + 1
         if(kentry .eq. 1) then
            OPEN(96,FILE='fort.96',STATUS='NEW',FORM='FORMATTED',       &
     &         BLANK='ZERO')
            write(96,5000)
 5000       format(1x,'OCEAN LOADING FILE'/                             &
     &             1x,'station ',4x, 'MJD', 5x,'NM',1x, 'YYMMDD ',      &
     &             'HHMMSS ',3x, 'del phi', 6x, 'del lam',              &
     &             6x, 'del height')
            mjdse0 = mjdsec
         endif
!>>>>>>>>>>>>>>
         do 3050 jjj=1,NM
!
!         if( kentry .le. iprint .and. jjj .le. 2 ) then
!         write(6,*) 'oload: mjdsec ', mjdsec
!         write(6,*) 'oload: jjj ', jjj
!         write(6,*) 'oload:  fsec(jjj) ',fsec(jjj)
!         write(6,*) 'oload: xtnoet ', (xtnoet(iii,jjj),iii=1,3)
!         write(6,*) 'oload: xtet ', (xtet(iii,jjj),iii=1,3)
!         endif
!
            call plhout( xtnoet(1,jjj), phino, cosphi, sinphi, xlamno,  &
     &                coslam, sinlam, hnoet, dummy, .true., .false. )
            call plhout( xtet(1,jjj), phiet, cosphi, sinphi, xlamet,    &
     &                coslam, sinlam, het,   dummy, .true., .false. )
            delphi = phiet - phino
            dellam = xlamet - xlamno
            del_h  = het   -  hnoet
!            write(6,*) 'oload: PLH het, hnoet, del_h ',het,hnoet,del_h
            mjddif = mjdsec - mjdse0
            mjdsex = mjdsec+fsec(jjj)
            xmjddy = (DBLE(mjdsec)+fsec(jjj))/86400.D0 + TMGDN2
            call MJDYMD(mjdsex,IYMD,IHMS, 4)
            if( MOD(kentry,2) .eq. 1) then
               write(96,'(1x,i8,1x,f10.3,i3,i7,i7,3d13.3 )')            &
     &         isnumb, xmjddy, jjj, IYMD, IHMS, delphi, dellam, del_h
            endif
 3050    continue
      endif
!--------------------------------------------------------------
!
!
      IF(.NOT.LADJOL) GOTO 2000
!
! FORM THE PARTIALS FOR EAST
      IF(NEAJ.LE.0) GO TO 550
      JCT=0
      DO 500 I=1,NEAJ
      JCT=JCT+1
      JFREQ=IPAJE(I)
      DO 410 N=1,NM
      FSCN=FSEC(N)+FSCO
      SCR2(N)=TRIGOL(JFREQ,3)+TRIGOL(JFREQ,1)*FSCN
  410 END DO
! TERMS FROM  DXSDAX
      DO 420 N=1,NM
      PXSPAB(N,1,JCT)=SCR2(N)*XLOCV(1,1)
      PXSPAB(N,2,JCT)=SCR2(N)*XLOCV(2,1)
      PXSPAB(N,3,JCT)=SCR2(N)*XLOCV(3,1)
  420 END DO
! TERMS FROM  DXSDBX
      DO 430 N=1,NM
      FSCN=FSEC(N)+FSCO
      SCR2(N)=TRIGOL(JFREQ,4)+TRIGOL(JFREQ,2)*FSCN
  430 END DO
      DO 440 N=1,NM
      PXSPAB(N,1,JCT+NEAJ)=SCR2(N)*XLOCV(1,1)
      PXSPAB(N,2,JCT+NEAJ)=SCR2(N)*XLOCV(2,1)
      PXSPAB(N,3,JCT+NEAJ)=SCR2(N)*XLOCV(3,1)
  440 END DO
  500 END DO
  550 CONTINUE
! FORM THE PARTIALS FOR NORTH
      IF(NNAJ.LE.0) GO TO 650
      JCT=2*NEAJ
      DO 600 I=1,NNAJ
      JCT=JCT+1
      JFREQ=IPAJN(I)
      DO 560 N=1,NM
      FSCN=FSEC(N)+FSCO
      SCR2(N)=TRIGOL(JFREQ,3)+TRIGOL(JFREQ,1)*FSCN
  560 END DO
! TERMS FROM  DXSDAX
      DO 570 N=1,NM
      PXSPAB(N,1,JCT)=SCR2(N)*XLOCV(1,2)
      PXSPAB(N,2,JCT)=SCR2(N)*XLOCV(2,2)
      PXSPAB(N,3,JCT)=SCR2(N)*XLOCV(3,2)
  570 END DO
! TERMS FROM  DXSDBX
      DO 580 N=1,NM
      FSCN=FSEC(N)+FSCO
      SCR2(N)=TRIGOL(JFREQ,4)+TRIGOL(JFREQ,2)*FSCN
  580 END DO
      DO 590 N=1,NM
      PXSPAB(N,1,JCT+NNAJ)=SCR2(N)*XLOCV(1,2)
      PXSPAB(N,2,JCT+NNAJ)=SCR2(N)*XLOCV(2,2)
      PXSPAB(N,3,JCT+NNAJ)=SCR2(N)*XLOCV(3,2)
  590 END DO
  600 END DO
  650 CONTINUE
! FORM THER PARTTIALS FOR VERT
      IF(NVAJ.LE.0) GO TO 1000
      JCT=2*(NEAJ+NNAJ)
      DO 700 I=1,NVAJ
      JCT=JCT+1
      JFREQ=IPAJV(I)
      DO 660 N=1,NM
      FSCN=FSEC(N)+FSCO
      SCR2(N)=TRIGOL(JFREQ,3)+TRIGOL(JFREQ,1)*FSCN
  660 END DO
! TERMS FROM  DXSDAX
      DO 670 N=1,NM
      PXSPAB(N,1,JCT)=SCR2(N)*XLOCV(1,3)
      PXSPAB(N,2,JCT)=SCR2(N)*XLOCV(2,3)
      PXSPAB(N,3,JCT)=SCR2(N)*XLOCV(3,3)
  670 END DO
! TERMS FROM  DXSDBX
      DO 680 N=1,NM
      FSCN=FSEC(N)+FSCO
      SCR2(N)=TRIGOL(JFREQ,4)+TRIGOL(JFREQ,2)*FSCN
  680 END DO
      DO 690 N=1,NM
      PXSPAB(N,1,JCT+NVAJ)=SCR2(N)*XLOCV(1,3)
      PXSPAB(N,2,JCT+NVAJ)=SCR2(N)*XLOCV(2,3)
      PXSPAB(N,3,JCT+NVAJ)=SCR2(N)*XLOCV(3,3)
  690 END DO
  700 END DO
 1000 CONTINUE
!
! FORM THE PARTIALS FOR EAST
      IF(NXCJ.LE.0) GO TO 1550
      JCT=0
      DO 1500 I=1,NXCJ
      JCT=JCT+1
      JFREQ=IPAXC(I)
      DO 1410 N=1,NM
      FSCN=FSEC(N)+FSCO
      PXSPA1(N,1,JCT)=TRIGOL(JFREQ,3)+TRIGOL(JFREQ,1)*FSCN
      PXSPA1(N,2,JCT)=ZERO
      PXSPA1(N,3,JCT)=ZERO
      PXSPA1(N,1,JCT+NXCJ)=TRIGOL(JFREQ,4)+TRIGOL(JFREQ,2)*FSCN
      PXSPA1(N,2,JCT+NXCJ)=ZERO
      PXSPA1(N,3,JCT+NXCJ)=ZERO
 1410 END DO
 1500 END DO
 1550 CONTINUE
! FORM THE PARTIALS FOR NORTH
      IF(NYCJ.LE.0) GO TO 1650
      JCT=2*NXCJ
      DO 1600 I=1,NYCJ
      JCT=JCT+1
      JFREQ=IPAYC(I)
      DO 1560 N=1,NM
      FSCN=FSEC(N)+FSCO
      PXSPA1(N,1,JCT)=ZERO
      PXSPA1(N,2,JCT)=TRIGOL(JFREQ,3)+TRIGOL(JFREQ,1)*FSCN
      PXSPA1(N,3,JCT)=ZERO
      PXSPA1(N,1,JCT+NYCJ)=ZERO
      PXSPA1(N,2,JCT+NYCJ)=TRIGOL(JFREQ,4)+TRIGOL(JFREQ,2)*FSCN
      PXSPA1(N,3,JCT+NYCJ)=ZERO
 1560 END DO
 1600 END DO
 1650 CONTINUE
! FORM THER PARTTIALS FOR VERT
      IF(NZCJ.LE.0) GOTO 2000
      JCT=2*(NXCJ+NYCJ)
      DO 1700 I=1,NZCJ
      JCT=JCT+1
      JFREQ=IPAZC(I)
      DO 1660 N=1,NM
      FSCN=FSEC(N)+FSCO
      PXSPA1(N,1,JCT)=ZERO
      PXSPA1(N,2,JCT)=ZERO
      PXSPA1(N,3,JCT)=TRIGOL(JFREQ,3)+TRIGOL(JFREQ,1)*FSCN
      PXSPA1(N,1,JCT+NZCJ)=ZERO
      PXSPA1(N,2,JCT+NZCJ)=ZERO
      PXSPA1(N,3,JCT+NZCJ)=TRIGOL(JFREQ,4)+TRIGOL(JFREQ,2)*FSCN
 1660 END DO
 1700 END DO
 2000 CONTINUE

!     write(6,*)' dbg LALOAD ',LALOAD
      IF(LALOAD) THEN
! THIS SITE MODELS ATMOSPHERIC PRESSURE LOADING
!     write(6,*)' dbg DEBUG APL IN OLOAD ISITA ',ISITA
!     write(6,*)' dbg APL INFO LALOAD  TIME ',LALOAD,MJDSEC,FSEC(1)
!     write(6,*)' dbg APL INFO TIMES ',ITAPLB,ITAPLE,IAPLDT
!     write(6,*)' dbg ITOTAL MAXAPL ',ITOTAL,MAXAPL
!     do icount=1,ITOTAL
!     write(6,*)' dbg new'
!      do j=1,MAXAPL
!      write(6,*)'dbgalc',alcoef(icount,1,j),alcoef(icount,2,j),    &
!    & alcoef(icount,3,j),ISITE(ICOUNT),icount,j
!      enddo
!     enddo
!     CURRENT TIME
      TIMEB=DBLE(ITAPLB)
      TIMEE=DBLE(ITAPLE)
      DT=DBLE(IAPLDT)

!     write(6,*)' dbg REAL TIMES ',TIMEB,TIMEE,DT
      DO I=1,NM
      ISEC=FSEC(I)
      REM=FSEC(I)-ISEC
      TIME=DBLE(MJDSEC+ISEC)+REM
       IF (TIME.LT.TIMEB) THEN
       write(6,*)'CURRENT TIME LESS THAN START OF APLOAD'
       write(6,*)'CHECK APLOAD FILE'
       END IF
!     write(6,*)' DBG CURRENT TIME ',TIME,I,NM
! DIFF TIMEB-CURRENT TIME
      DIFF=TIME-TIMEB
!     write(6,*)' DBG DIFF ,DT ',DIFF,DT
!     IF DT NEGATIVE ERROR
!     FIND THE TWO RECORDS NEEDED TO INTERPOLATE FOR CURRENT TIME.

        IF(DIFF.GT.DT) THEN
         IG=DIFF/DT
!        write(6,*)' dbg diff is greater than dt so.. ',IG
         TNEW=TIMEB+IG*DT
         DIFFNEW= TIME-TNEW
         T1=TNEW
         T3=TNEW+DT
         T2=TIME
         IG=IG+1
!IG needs plus 1 for reading records
!records were previously 3 hours off
        ELSE IF (DIFF.LT.DT) THEN
         IG=1
         T1=TIMEB
         T3=TIMEB+DT
         T2=TIME
        END IF

        FACTOR=(T2-T1)/(T3-T1)
        VE1=ALCOEF(ISITA,1,IG)
        VE3=ALCOEF(ISITA,1,IG+1)
        VN1=ALCOEF(ISITA,2,IG)
        VN3=ALCOEF(ISITA,2,IG+1)
        VV1=ALCOEF(ISITA,3,IG)
        VV3=ALCOEF(ISITA,3,IG+1)
        VE=FACTOR*(VE3-VE1)+VE1
        VN=FACTOR*(VN3-VN1)+VN1
        VV=FACTOR*(VV3-VV1)+VV1

! RECORDS TO BE USED FOR INTERPOLATION
!      write(6,*)'rec int',alcoef(isita,1,ig),alcoef(isita,2,ig),     &
!    &           alcoef(isita,3,ig)
!      write(6,*)'rec int',alcoef(isita,1,ig+1),alcoef(isita,2,ig+1), &
!    &            alcoef(isita,3,ig+1)
!      write(6,*)' NEW INT VAL ',VE,VN,VV,FACTOR
!       VMAG=SQRT(VE**2+VN**2+VV**2)

! Convert to DX DY DZ
!      write(6,*)' dbg XLOCV '
!      write(6,*)XENV(1,1),XENV(1,2),XENV(1,3)
!      write(6,*)XENV(2,1),XENV(2,2),XENV(2,3)
!      write(6,*)XENV(3,1),XENV(3,2),XENV(3,3)
       DX=VE*XENV(1,1) + VN*XENV(1,2) + VV*XENV(1,3)
       DY=VE*XENV(2,1) + VN*XENV(2,2) + VV*XENV(2,3)
       DZ=VE*XENV(3,1) + VN*XENV(3,2) + VV*XENV(3,3)
!      write(6,*)' DBG DISPLACEMENTS ',DX,DY,DZ
!        ENDIF

! temporarilly comment out
      XYZC(I,1)=XYZC(I,1)+DX
      XYZC(I,2)=XYZC(I,2)+DY
      XYZC(I,3)=XYZC(I,3)+DZ
      ENDDO
      ENDIF
      RETURN
      END
