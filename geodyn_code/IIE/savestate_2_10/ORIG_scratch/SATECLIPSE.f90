!
      SUBROUTINE SATECLIPSE(IPRN,TTAG,helios,state,   &
     &iblock,xyz,outxyz,yangle,phi,night,noon,nophi,YBIAS)
!
! January 2014 Version.
!
!       NAME                SATECLIPSE
!                       (based on Jan Kouba's 2008 paper in GPS Solutions and 20
!                        and on his subroutine "eclips", September  2014 version
!                        First version of "sateclipse" written by OLC in January
!                        with some changes made by OLC in March 2014.
!
!        PURPOSE         DETECT ECLIPSING & YAW ROTATE ECLIP. SATELLITES
!                       (THE INPUT BODY-X UNIT VECTOR - xyz IS YAW-ROTATED
!                        BY PHI-YANGLE (THE ECL-NOMINAL) YAW ANGLE DIFFERENCE)
!
!       CHANGES: Except where noted that minor changes have been made, the part
!                calculates the yaw is the same as in the original "eclips".
!                The original code is in all-capitals. The parts in low case are
!                (with comments) added to calculate angles and vectors that defi
!                and the sun-earth geometry (beta, mu, epsilon, etc.) This code
!                added to make the subroutine more self-standing.
!                Also some arguments have been removed from the original calling
!                statements, as those were meant for passing information require
!                a recursive (with a Kalman filter and smoother) solution, and a
!                in a batch least squares solution.
!                One way to use the results of this subroutine to calculate the
!                is using the the output arguments "phi" and "nophi" as explaine
!                end of these front-end comments.
!
!       NOTE: The numbering of the GPS Blocks here is different from that in oth
!              (see description of "IBLK", below).
!
!        PARAMETERS        DESCRIPTION
!
! INPUT:
!        IPRN           SV PRN NUMBER (.LE.32 FOR GPS)
!        TTAG           OBSERVATION EPOCH TIME TAG IN SECONDS SINCE START OF SOL
!        HELIOS         GEOCENTER TO CENTER OF SUN DISK VECTOR X, Y, Z  (m) (IN
!        STATE          SATELITE POSITION+VELOCITY STATE VECTOR (m and m/s) (IN
!        IBLK           SV BLOCK  1=I, 2=II, 3=IIA, IIR=(4, 5) IIF=6
!        XYZ            UNIT VECTOR ALIGNED WITH BODY-FRAME X AXIS AND AS CLOSEL
!                       WITH THE SUN  (IN INERTIAL FAME).
! OUTPUT:
!        OUTXYZ         VECTOR "XYZ" ROTATED BY THE ANGLE (PHI-YAWNGLE).
!        YANGLE         NOMINAL YAW ANGLE (DEGREES).
!        PHI            ACTUAL YAW ANGLE (DEGREES) DURING ECLIPSE MANEUVERS (SEE
!        NOPHI          SIGNALS THAT THE NOMINAL YAW FROM MAIN PROGRAM SHOULD BE
!                       OR THE TRUE MANEUVERING YAW (NOPHI=1).
!        NOON           LOGICAL FLAG SET TO ".TRUE." DURING A NOON MANEUVER.
!        NIGHT          LOGICAL FLAG SET TO ".TRUE." DURING A MIDNIGHT MANEUVER.
!
! MAIN INTERNAL PARAMETERS:
!        AMU            MU ORBIT ANGLE, FROM MIDNIGHT POINT TO CURRENT SATELLITE
!        BETAA          THE SUN ANGLE WITH THE ORBITAL PLANE(RADIANS).
!        BETADG         SAME AS BETAA, BUT IN DEGREES.
!        SVBCOS         SV BETA ANGLE (BETWEEN SV AND SUN RADIUS): -COSINE OF EP
!        ANOON          SV BETA ANGLE LIMIT (DEG) FOR A NOON TURN MANEUVRE .
!        ANIGHT         SV BETA ANGLE LIMIT (DEG) FOR A NIGHT SHADOW CROSSING.
!        NECLIPS        NUMBER OF ECLIPSING FOR THE PRN SATELLITE.
!        ECLSTM         ECLIPSE START TIME(IN SECONDS, FROM THE FIRST EPOCH IN T
!        ECLETM         ECLIPSE END TIME  ( "         "         ").
!        IECLIPS        SV ECLIPSING (0=NO,1, 2=YES(1=night, 2=noon))
!        XSV(3)         SV POSITION VECTOR  X, Y, Z (m)   -- FROM "STATE" (IN IN
!        VSVC(3)        SV VELOCITY VECTOR VX,VY,VZ (m/s) -- FROM "STATE" (IN IN
!        PI             = PI=3.1415926536D0
!
! SUBROUTINES CALLED:
!        VPROD, VNORM, SPROD, ECLIPSOL (in the added code)
!
!                       NOTE: THE IIA BODY-FRAME X AXIS ORIENTATION
!                       IS REVERSED FOR IIR, BUT THE CORRECTION
!                       PHI-YANGLE TO THE ACTUAL YAW IS THE SAME IF THE YAW
!                       IS DEFINED IN TERMS OF THE SATELLITE SUN-FACING "X" SIDE
!                       THE USER COMPUTES THE YAW ANGLE OF THE SATELLITE AS:
!
!                       YAW = (PHI-YANGLE+YAWNU)*NOPHI+(1-NOPHI)*YAWNU
!
!                       HERE "PHI", "YANGLE" AND "NOPHI" ARE OUTPUT VARIABLES
!                       OF THIS SUBROUTINE, (NOPHI IS EITHER "1" OR "0")
!                       AND "YAWNU" IS THE USER'S CHOSEN NOMINAL YAW FOR THE IIR
!                       WHICH MAY OR MAY NOT BE A REVERSED X VECTOR.
!
!
! *********************************************************************
!
!  ************** Oscar L. Colombo, March 2014 **************
!
       implicit DOUBLE PRECISION (a-h,o-z)
       save
!
!     MAXSAT - MAXIMUM NUMBER OF SATELLITES, CHANGED IF REQUIRED
!
      INTEGER MAXSAT
      PARAMETER (MAXSAT=64)
!
      INTEGER*4 IDIR, IPRN
      INTEGER*4 IECLIPS, NECLIPS(100)
!
      DOUBLE PRECISION TTAG, TWOHR, HALFHR,helios(3),state(6)
      DOUBLE PRECISION SVBCOS,PI, damu,amu,eeek
      DOUBLE PRECISION     ECLSTM(MAXSAT,100),outxyz(3)
      DOUBLE PRECISION     ECLETM(MAXSAT,100)
      DOUBLE PRECISION ANOON, ANIGHT
      DOUBLE PRECISION CNOON, CNIGHT,sunbetav(3),across(3),acrossn(3)
      DOUBLE PRECISION     DTR, DTTAG,sun(3),xn(3),helion(3),satn(3),sunx(3)
      DOUBLE PRECISION XSV(3), xyz(3), VSVC(3), BETAA, MURATE, YANGLE, DET, &
     &          YRATE(maxsat), BETADG, PHI, SANTX, SANTY,v(3),r(3)
      DOUBLE PRECISION YAWEND,sbperp(3),YBIAS
      DOUBLE PRECISION amul(32),tmuin(32),amuin(32)
      INTEGER*4  IBLK(maxsat), J, I,muin(32)
!
      LOGICAL   NOON, NIGHT
!
! MAX YAW RATES OF CURRENT&PAST BLOCK II/IIA's,(AVER'D 1996-2008 JPL's SOLUTIONS
! CHANGE IF REQUIRED OR INPUT IF ESTIMATED
! PRN                 01     02      03      04     05       06     07
      DATA YRATE/.1211D0,.1339D0,.123D0,.1233D0,.1180D0,.1266D0,.1269D0 &
!             08      09     10      11      12      13      14      15
     &,.1033D0,.1278D0,.0978D0,0.200D0,0.199D0,0.200D0,0.0815D0,.1303D0 &
! PRN          16     17     18      19      20      21      22     23
     &,.0838D0,.1401D0,.1069D0, .098D0, .103D0,0.1366D0,.1025D0,.1140D0 &
! PRN          24     25      26      27     28      29      30     31
     &,.1089D0,.1001D0,.1227D0,.1194D0,.1260D0,.1228D0,.1165D0,.0969D0 &
! PRN         32  33-64: GLONASS RATES (DILSSNER 2010)
     &,.1152D0, 32*0.250D0/, &
     &istart/1/,idir/1/,neclips/100*0/
!
      IBLK(IPRN) = iblock
!
!  CHECK FOR BLOCK IIR AND FIX TO NOMINAL YAW RATE
      IF( IPRN .LE. 32 .AND. IBLK(IPRN).GE.4 ) YRATE(IPRN)=0.2D0
! THE NEW GPS BLK IIF YAW RATES ( DILSSNER (2010) INSIDE GNSS)
      IF( IPRN .LE. 32 .AND. IBLK(IPRN).GE.6 ) YRATE(IPRN)=0.11D0
! Dec 12, 2013
!  YBIAS=-0.5 FOR IIA (IBLK<4) PRN23 (SVN23 UP TO FEB2004,AFTER  IIR SVN60
!   AND NOT USED), IIF (IBLK=6)=-0.5, USED FOR SMALL NEG BETA NOON TURNS ONLY!
      YBIAS=0.0D0
      IF(IBLK(IPRN).LE.3) YBIAS= 0.5D0
      IF(IPRN.EQ.23.OR.IBLK(IPRN).EQ.6) YBIAS=-0.5D0
      if(istart.eq.1) then
      istart = 0
      pi = 4.D0*ATAN(1.D0)
      pion2 = 0.5D0*pi
      twopi = 2.D0*pi
      IECLIPS=0
      TWOHR = 7200.D0
      HALFHR= 1800.D0
      DTR=PI/180.D0
      rtd = 1.D0/dtr
      do i=1,32
      muin(i) = 0.D0
      enddo
      endif
!
      nophi = 1
      phi = 0.D0
      do i = 1,3
      xsv(i)  = state(i)
      vsvc(i) = state(i+3)
      enddo
      call vnorm(helios,helion,sunmod)
      do i = 1,3
      sun(i) = helios(i)-xsv(i)
      enddo
      sumsvb = 0.D0
      do i = 1,3
      sumsvb = sumsvb+sun(i)*xsv(i)
      enddo
      sunmod = 0.D0
      xsvmod = 0.D0
      do i = 1,3
      sunmod = sunmod+sun(i)**2
      xsvmod = xsvmod+xsv(i)**2
      enddo
      rsat = SQRT(xsvmod)
      dsun = SQRT(sunmod)
      svbcos = sumsvb/(rsat*dsun)
      if(svbcos.gt.1.D0-1.D-9) svbcos = 1.D0-1.D-9
      if(svbcos.lt.-1.D0+1.D-9) svbcos= -1.D0+1.D-9

      call vnorm(xsv,satn,satposmod)
!
! Find angle Beta between sun vector and orbit plane:
!
! But first, find, in inertial space, the vectors in the along
! and across directions, completing, with satn, the orbit-fixed triad.
!
      call vprod(satn,vsvc,across)
      call vnorm(across,acrossn,sacross)
      call sprod(helion,acrossn,cosalpha0)
      betaa = pion2-ACOS(cosalpha0)
!
! Find the mu orbit angle from sat. position to orbital midnight.
!
       MURATE= SQRT((VSVC(1)**2+VSVC(2)**2+VSVC(3)**2)/  &
     & (xsv(1)**2+xsv(2)**2+xsv(3)**2))*rtd
      do i = 1,3
      sunx(i) = -(helion(i)-cosalpha0*acrossn(i))
      enddo
      call vnorm(sunx,sunbetav,sunxmod) ! Sunbetav = unit "midnight" orbi
      call sprod(satn,sunbetav,cosmu)
!
! OLC - 14sep2015
      if(ABS(ttag-tmuin(iprn)).gt.7200.D0) muin(iprn)=0
!
      if(ABS(betaa*rtd).lt.5.D0.and.(ABS(amul(iprn)*rtd).lt.3.D0.or. &
     & ABS(ABS(amul(iprn)*rtd)-180.D0).lt.3.D0).and. &
     & muin(iprn).eq.1) then ! Within the fastest part
!                                            of a maneuver, but not the first ep
      amu = amuin(iprn)+(ttag-tmuin(iprn))*murate*dtr
      if(amu.gt.pi) amu = amu-twopi

      amul(iprn) = amu

      else  ! First epoch with data (muin = 0) or not near the middle of
      call sprod(satn,sunbetav,cosmu)
      call vprod(sunbetav,acrossn,sbperp)
      call sprod(satn,sbperp,signmu)
      amu = ACOS(cosmu)*(-SIGN(1.D0,signmu))

      muin(iprn) = 1
      amuin(iprn) = amu
      tmuin(iprn) = ttag

      amul(iprn) = amu
      endif
!
! compute the noon beta angle limit (beta zero) FOR A NOON TURN from YRATEs
! & THE ACTUAL SAT ORBIT ANGLE RATE (MURATE) (~0.00836 FOR GPS)
!
      damu = amu*rtd
      ANOON=ATAN(MURATE/YRATE(IPRN))*rtd
      CNOON=COS(ANOON*DTR)
      anight = 13.5D0+180
      CNIGHT=COS(ANIGHT*DTR)
!
      NOON=.FALSE.
      NIGHT=.FALSE.
      BETADG = betaa*rtd
!
!     yawrate = murate*dtan(betaa)*dcos(amu)/(dsin(amu)**2
!    .+dtan(betaa)**2)
!
      epsilonr = ACOS(-svbcos)
      cosepsilon = -svbcos
      if(ABS(cosepsilon).gt.1.D0.and.ABS(betaa).lt.1.D-6)  &
     &cosepsilon = 1.D0*SIGN(1.D0,cosepsilon)
!
      call eclipsol(rsat,dsun,epsilonr,area,areafrac)
!
!
       IF(IPRN.GT.32.AND.ABS(BETADG).LT.ANOON) THEN
! GLONASS NOON TURN MODE ACORDING TO DILSSNER 2010
         YAWEND=75.D0
!  ITERATION FOR YAWEND OF THE GLONASS  NOON TURN
         DO J=1,3
           YAWEND=ABS(ATAN2(-TAN(BETADG*DTR),SIN(PI-   &
     &       DTR*MURATE*YAWEND/YRATE(IPRN)))/DTR -     &
     &       ATAN2(-TAN(BETADG*DTR),SIN(PI+            &
     &       DTR*MURATE*YAWEND/YRATE(IPRN)))/DTR)/2.D0
         END DO
! UPDATE ANOON, CNOON FOR NEW GLONASS NOON TURN LIMITS
          ANOON= MURATE*YAWEND/YRATE(IPRN)
          CNOON= COS(ANOON*DTR)
       ENDIF
! BLK IIR'S
        IF(IBLK(IPRN).EQ.4 .OR. IBLK(IPRN).EQ.5) THEN
         CNIGHT=COS((ANOON+180.D0)*DTR)
        ENDIF
!      IF (SVBCOS .LT. CNIGHT) THEN ! Figuring out if satellite is in eclipse sh
!        NIGHT=.TRUE.               ! This is now done with subroutine "eclipsol
!      END IF
       IF (SVBCOS .GT. CNOON) THEN
         NOON=.TRUE.
       END IF
      if(areafrac.eq.0.D0) night=.true. ! "areafrac" = fraction of sun di
!
!     IF SV IN NIGHT SHADOW OR NOON TURN DURING FORWARD PASS
!     STORE START AND END TIME OF YAW MANEUVER
!
! CALCULATE NOMINAL YAW ANGLE:
!
      yangle = rtd*ATAN2(-TAN(betaa),SIN(amu)) ! (Bar-Sever, 1995)
      if(iblk(IPRN).eq.4.or.iblk(IPRN).eq.5)  &
     &yangle = rtd*ATAN2(TAN(betaa),-SIN(amu)) ! (Bar-Sever, 1995)
!
      IF ( (NIGHT .OR. NOON)) THEN
       if(ABS(amu*rtd).gt.1.D0) then ! Mod. made to prevent
!      fatal numerical error when mu is very small. (OLC Jan. 2013).
!
      if((180.D0-ACOS(svbcos)*rtd)**2-BETADG**2.gt.0.D0) then
!
!     DET=dSQRT((180.d0-dacos(svbcos)*rtd)**2-BETADG**2)
      DET=SQRT(ABS((180.D0-ACOS(svbcos)*rtd)**2-BETADG**2))
!
      else
      det = amu*rtd
      endif
!
      else
      det = amu*rtd
      endif
       PHI = PI/2.D0
! Check if already after a midnight or noon
       IF(NIGHT) THEN
         IF(IBLK(IPRN).EQ.4.OR.IBLK(IPRN).EQ.5) THEN
          IF(ABS(YANGLE).GT.90.D0) DET=-DET
         IF(DET.NE.0.D0) &
     &PHI=ATAN2( TAN(BETADG*DTR),-SIN(-DET*DTR))*rtd
         ELSE
! BLK IIA & GLONASS TOO:
          IF(ABS(YANGLE).LT.90.D0) DET=-DET
         IF(DET.NE.0.D0)  &
     &PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))*rtd
         END IF
       END IF
       IF(NOON) THEN
!      DET=dSQRT((dacos(svbcos)*180.d0/pi)**2-BETADG**2)
       DET=SQRT(ABS((ACOS(svbcos)*180.D0/pi)**2-BETADG**2))
         IF(IBLK(IPRN).EQ.4.OR.IBLK(IPRN).EQ.5) THEN
          IF(ABS(YANGLE).LT.90.D0) DET=-DET
       IF(DET.NE.0.D0)  &
     &PHI=ATAN2(TAN(BETADG*DTR),-SIN(PI-DET*DTR))*rtd
         ELSE
! BLK IIA & GLONASS
          IF(ABS(YANGLE).GT.90.D0) DET=-DET
       IF(DET.NE.0.D0)&
     &PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))*rtd
         END IF
       END IF
!
       IF (IDIR .GT. 0 ) THEN
!
!       INITIALIZE ECLIPSE START AND TIME TAG ARRAYS
!
        IF ( NECLIPS(IPRN) .EQ. 0 ) THEN
          NECLIPS(IPRN)=NECLIPS(IPRN)+1
          ECLSTM(IPRN,NECLIPS(IPRN))=TTAG+DET/MURATE
! IIR MIDNIGHT/NOON TURN or II/IIA NOON TURN START
! for IIR NIGHT (turn) only makes sense when BETADG < ANOON!
! For IIA it gets here only when NOON is true and that happens  only when BETADG
          YAWEND=ATAN(MURATE/YRATE(IPRN))*rtd
          IF((IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5.OR.NOON).AND.  &
     &       ABS(BETADG).LT.YAWEND) THEN
! GLONASS
            IF(     IPRN .GT.32) THEN
! GLONASS NOON TURN MODE ACORDING TO DILSSNER ET AL 2010
             ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-  &
     &        ANOON/MURATE
             ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+  &
     &        2.D0*ANOON/MURATE
            ELSE
! GPS IIA/IIR/IIF NOON OR IIR MIDNIGHT TURNs
             ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-  &
!    &        dABS(BETADG)*dsqrt(ANOON/dABS(BETADG)-1.d0)/MURATE
     &     ABS(BETADG)*SQRT(ABS(ANOON/ABS(BETADG)-1.D0))/MURATE
             ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+  &
!    &        2*dABS(BETADG)*dsqrt(ANOON/dABS(BETADG)-1.d0)/MURATE
     &    2*ABS(BETADG)*SQRT(ABS(ANOON/ABS(BETADG)-1.D0))/MURATE
          END IF
        ENDIF
!     II/IIA SHADOW START & END TIMES
          IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5).AND.NIGHT) THEN
           ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-  &
!    &      dSQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
     &      SQRT(ABS((ANIGHT-180.D0)**2-BETADG**2))/MURATE
           ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+  &
!    &      2.d0*dSQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
     &      2.D0*SQRT(ABS((ANIGHT-180.D0)**2-BETADG**2))/MURATE
        END IF
       ENDIF
!
!       UPDATE SV COSINE AND TIME TAG ARRAYS
!
!
        IF( (NIGHT .AND. SVBCOS .LT. CNIGHT)  &
     &       .OR. (NOON .AND. SVBCOS .GT. CNOON) ) THEN
          DTTAG= ABS(TTAG-ECLSTM(IPRN,NECLIPS(IPRN)))
!
!         LAST ECLIPSE TIME WAS MORE THAN 2 HOURS AGO, THIS IS A NEW ECLIPSE!
!
          IF( DTTAG .GT. TWOHR ) THEN
            NECLIPS(IPRN)=NECLIPS(IPRN)+1
            ECLSTM(IPRN,NECLIPS(IPRN))=TTAG+DET/MURATE
! IIR MIDNIGHT/NOON TURN  or II/IIA NOON TURN START
!                                  AND GLONASS NOON
            IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5.OR.NOON) THEN
! GLONASS
             IF(IPRN.GT.32) THEN
! GLONASS NOON TURN MODE ACORDING TO DILSSNER ET AL 2010
              ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-  &
     &          ANOON/MURATE
              ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+  &
     &         2.D0*ANOON/MURATE
             ELSE
! GPS TURNS ONLY
              ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-  &
!    &         dABS(BETADG)*dsqrt(ANOON/dABS(BETADG)-1.d0)/MURATE
     &      ABS(BETADG)*SQRT(ABS(ANOON/ABS(BETADG)-1.D0))/MURATE
              ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+  &
!    &         2.d0*dABS(BETADG)*dsqrt(ANOON/dABS(BETADG)-1.d0)/MURATE
     &    2.D0*ABS(BETADG)*SQRT(ABS(ANOON/ABS(BETADG)-1.D0))/MURATE
            END IF
           ENDIF

!     II/IIA SHADOW START & END TIMES
!   & IIF AS WELL !
            IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5).AND.NIGHT) THEN
             ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-  &
!    &       dSQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
     &       SQRT(ABS((ANIGHT-180.D0)**2-BETADG**2))/MURATE
             ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+  &
!    &       2.d0*dSQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
     &       2.D0*SQRT(ABS((ANIGHT-180.D0)**2-BETADG**2))/MURATE
            END IF
          END IF
        ENDIF
!  END OF FINDING ECLIPSE START AND END TIMES.
       ENDIF
      ENDIF
!
!     SET ECLIPSE FLAG (1=NIGHT SHADOW, 2=NOON TURN)
!
      IF ( NECLIPS(IPRN) .NE. 0 ) THEN
! CHECK IF IPRN IS ECLIPSING AND WHICH SEQ NO (I)
        I=0
        DO J=1, NECLIPS(IPRN)
        IF( TTAG.GE.ECLSTM(IPRN,J).AND.TTAG.LE.(ECLETM(iprn,J)+HALFHR)) &
     &   I= J
        END DO
! CURRENTLY NOT ECLIPSING (i=0)
        IF(I.EQ.0) GO TO 1
        IF ( TTAG .GE. ECLSTM(IPRN,I) .AND. &
     &            TTAG .LE.(ECLETM(IPRN,I)+HALFHR) ) THEN
! velocity & radius unit vectors V & R
             DO J=1,3
              V(J)=VSVC(J)/SQRT(VSVC(1)**2+VSVC(2)**2+VSVC(3)**2)
              R(J)=XSV(J)/SQRT(XSV(1)**2+XSV(2)**2+XSV(3)**2)
             END DO
      if(ABS(r(3)).lt.1.D-9) r(3) = 1.D-9
! ORBIT ANGLE MU AT ECLIPSE/TURN START
             DET= MURATE*(ECLETM(IPRN,I)-   &
     &                        ECLSTM(IPRN,I))/2.D0
! Dec 12, 2013 - start
! YAWEND HERE - IIF SHADOW YAW RATE
                  YAWEND=(ATAN2(-TAN(BETADG*DTR), SIN( DET*DTR))-   &
     &             ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR)))/DTR/     &
     &                  (ECLETM(IPRN,I)-ECLSTM(IPRN,I))
! Dec 12, 2013 - end
             IF (SVBCOS .LT. 0) THEN
! SHADOW CROSSING
! BLK IIA/IIF SHADOW CROSSING
                IF(IPRN.LE.32.AND.(IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5)) &
     &             THEN
                 IF(TTAG.LE.ECLETM(IPRN,I)) THEN
! IIA NIGHT TURN
                  IF(IBLK(IPRN).LE.3)  &
     &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))*rtd  &
! Dec 12, 2013
!    &           +SIGN(YRATE(IPRN),0.5d0)*(TTAG-ECLSTM(IPRN,I))
     &           +SIGN(YRATE(IPRN),YBIAS)*(TTAG-ECLSTM(IPRN,I))
! IIF NIGHT TURN (DILSSNER  2010)
                   IF(IBLK(IPRN).GT.5)  &
     &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR  &
! Dec 12, 2013- start
! Correct IIF  NIGHT CROSSING USING COMPUTING YAW RATE (YAWEND)
! ACCORDING TO THE USAF IIF DOCUMENT
!    &           +SIGN(0.06D0, BETADG)*(TTAG-ECLSTM(IPRN,I))
     &           +  YAWEND            *(TTAG-ECLSTM(IPRN,I))
! Dec 12, 2013 - end
                 ELSE
! **** WARNING
! IIA/IIF SHADOW EXIT RECOVERY: USING THE IIA DATA  DURING
! THE IIA RECOVERY (UP TO 30 MIN) IS NOT RECOMMENDED!
! **** WARNING
! GPS IIA  AT SHADOW EXIT
                   IF(IBLK(IPRN).LE.3)  &
     &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR &
! Dec 12, 2013
!    &        +SIGN(YRATE(IPRN),0.5d0)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))
     &        +SIGN(YRATE(IPRN),YBIAS)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))
! GPS IIF AT SHADOW EXIT
! Dec 12, 2013
! NO NEED FOR IIF RECOVERY ALREADY AT THE EXIT YAW!
                   IF(IBLK(IPRN).GT.5)GO TO 1
!                  IF(IBLK.GT.5)
!    &             PHI=dATAN2(-dTAN(BETADG*DTR), dSIN(-DET*DTR))/DTR
!    &          +SIGN(0.06D0, BETADG)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))
! YAWEND- HERE THE ACTUAL YAW DIFFERENCE  AT THE SHADOW EXIT
                   YAWEND= YANGLE- PHI
                   YAWEND=MOD(YAWEND, 360.D0)
! NOTE: To get the yaw with 360 degree jumps, uncomment the next two statements:
!     IF(dabs(YAWEND).GT.180.D0) YAWEND= YAWEND-360.D0* &
!    &               YAWEND/dabs(YAWEND)
!         IF(dabs(YAWEND).GT.180.D0) YAWEND= YAWEND-360.D0*  &
!    &               YAWEND/dabs(YAWEND)
! NOTE: When the two statements above are commented, the yaw can be corrected fo
! NOTE: When the two statements above are commented, the yaw can be corrected fo
! k*180 jumps in the calling program.
                   PHI=PHI &
     &                  +SIGN(YRATE(IPRN),YAWEND)*(TTAG-ECLETM(IPRN,I))
! SANTX- THE CURRENT ANGLE DIFF, CONSISTENT WITH YAWEND
                   SANTX= YANGLE-PHI
                   SANTX =MOD(SANTX , 360.D0)
!                  IF(dabs(SANTX).GT.180.D0) SANTX = SANTX -360.D0* &
!    &               SANTX /dabs(SANTX )
! STOP! THE NOMINAL YAW (YANGLE) REACHED!
                IF(ABS(SANTX).GT.ABS(YAWEND)) GOTO 1
                IF(YAWEND.NE.0.D0.AND.((SANTX)/YAWEND).LT.0.D0) GOTO 1
! SET PHI <-180,+180>
                   PHI= MOD(PHI, 360.D0)
                IF(ABS(PHI).GT.180.D0) PHI= PHI-360.D0*PHI/ABS(PHI)
                 ENDIF
                ENDIF
! GLONASS
                IF(IPRN.GT.32) THEN
! GLONASS      NIGHT TURN (DILSSNER AT AL 2010 )
                 IF(TTAG.GT.ECLETM(IPRN,I)) GOTO 1
                 YAWEND=YRATE(IPRN)
                 PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR  &
     &      +SIGN(YAWEND     ,BETADG)*(TTAG-ECLSTM(IPRN,I))
! YAWEND -YAW ANGLE AT THE (GLONASS) SHADOW EXIT
                  YAWEND=ATAN2(-TAN(BETADG*DTR), SIN( DET*DTR))/DTR
                 IF((YAWEND/PHI).GE.1.D0.OR.(PHI/YAWEND).LT.0.D0) THEN
                  PHI = YAWEND
                 ENDIF
                ENDIF
                   IF(IPRN.LE.32.AND.IBLK(IPRN).GT.5) THEN
! GPS BLK IIF: DURING A NIGHT CROSSING THE YAW IS NOMINAL FOR BETAA > 8 (DILSSNE
                    IF(ABS(BETADG).GT.8.D0) GO TO 1
                   ENDIF
               IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5) THEN
! BLK IIR SHADOW (MIDNIGHT TURN) CROSSING
                PHI=ATAN2( TAN(BETADG*DTR),-SIN(-DET*DTR))*rtd  &
     &      +SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))
                IF((PHI/YANGLE).GE.1.D0.OR.(PHI/YANGLE).LT.0.D0)GO TO 1 !
               END IF
!             write(*,*)"R",IPRN-32,TTAG,YANGLE, PHI,DET,
!    & BETADG, ECLETM(IPRN,I),I
               IECLIPS=1
             ELSE
! NOON TURNS
              PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))*rtd  &
     &      -SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))
! Dec 12, 2013 -start
! SMALL NEGATIVE BETA IIF OR SMALL POSIT. IIA NOON TURN PROBLEM
! Jan 24, 2014
!               IF((IBLK.LE.3.OR.IBLK.EQ.6).AND.
                IF(IPRN.LE.32.AND.   &
! CHANGE THE EMPIRICAL BETA LIMIT 0.9 DEG, IF REQ'D
     &             (BETADG*SIGN(1.D0,YBIAS)).LE.0.9D0  .AND.  &
! IN THEORY THE ABOVE LIMIT OF 0.9 DEG SHOULD BE ABS(YBIAS)!
!    &             (BETADG*SIGN(1.D0,YBIAS)).GT.0.0D0)       THEN
     &             (BETADG*YBIAS).GT.0.0D0)                  THEN
                    PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))/DTR &
     &              +SIGN(YRATE(IPRN),YBIAS )*(TTAG-ECLSTM(IPRN,I))
                ENDIF
! Dec 12, 2013 - end
                IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5) THEN
! BLK IIR NOON TURNS ONLY
                 PHI=ATAN2( TAN(BETADG*DTR),-SIN(PI-DET*DTR))*rtd  &
     &      -SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))
! IIR END TURN CHECK
                 IF((YANGLE/PHI).GE.1.D0.OR.(PHI/YANGLE).LT.0.D0)GOTO 1
                ELSE
! GLONASS END TURN CHECK
                 IF(     IPRN .GT.32.AND.TTAG.GT.ECLETM(IPRN,I)) GOTO 1
! IIA OR IIF END TURN CHECK
! Dec 12, 2013 -start
                 IF(IPRN.LE.32.AND.BETADG*SIGN(1.D0,YBIAS).LE.0.9D0.AND.&
! Jan 24, 2014
!    &             BETADG*SIGN(1.D0,YBIAS).GT.0.0D0.AND.
     &             BETADG*YBIAS.GT.0.0D0.AND.  &
     &            (((PHI-SIGN(1.D0,YBIAS)*360.D0)/YANGLE).LE.1.D0.OR. &
     &            ((PHI-SIGN(1.D0,YBIAS)*360.D0)/YANGLE).LT.0.D0))GOTO 1
!                IF(IPRN.LE.32.AND.
                 IF(IPRN.LE.32.AND.(BETADG*SIGN(1.D0,YBIAS).GT.0.9D0.OR.&
! Jan 24, 2014
!    &              BETADG*SIGN(1.D0,YBIAS).LE.0.0D0).AND.
     &              BETADG*YBIAS.LE.0.0D0).AND. &
     &            ((PHI/YANGLE).GE.1.D0.OR.(PHI/YANGLE).LT.0.D0)) GOTO 1
                ENDIF
!             write(*,*)"S",IPRN,TTAG,YANGLE, PHI,DET,
!    & BETADG, ECLSTM(IPRN,I)
               IECLIPS=2
             END IF
! ROTATE X-VECTOR TO ECLIPSING YAW ANGLE PHI
! ECLIPSING (II/IIA) NOT TO BE USED  A HALF HR AFTER SHADOW !
        phi = MOD(phi,360.D0)
          SANTX=(COS((PHI-YANGLE)*DTR)*(V(2)-V(3)*R(2)/R(3))-COS(PHI* &
     & DTR)*  &
     &(XYZ(2)-XYZ(3)*R(2)/R(3)))/(XYZ(1)*V(2)-XYZ(2)*v(1  &
     &)+((XYZ(2)*V(3)-XYZ(3)*V(2))*R(1)+(XYZ(3)*V(1)-XYZ  &
     &(1)*V(3))*R(2))/R(3))
       SANTY = (COS(PHI*DTR) - (V(1)-V(3)*R(1)/R(3))*SANTX)/  &
     & (V(2)-V(3)*R(2)/R(3))
! THE BODY-X UNIT VECTOR ROTATED BY (PHI-YANGLE) RETURNED
          outXYZ(1)= SANTX
          outXYZ(2)= SANTY
          outXYZ(3)= (-R(1)*SANTX-R(2)*SANTY)/R(3)
       nophi = 0
        END IF
      ENDIF
!
   1  RETURN
!
      END
