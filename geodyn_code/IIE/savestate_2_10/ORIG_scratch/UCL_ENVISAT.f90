!$ UCL_ENVISAT
      SUBROUTINE UCL_ENVISAT(MJDSEC,FSEC,slon,slat,ratio,xdata, &
     &           ydata,zdata,ROT,SUN_PROBE,dist,SOLACC,GRPAR,   &
     &           KPART,NEQN,PARMVC,SCALEL,LSASRD)
!**********************************************************************
!   I/O PARAMETERS
!
!   NAME    I/O    A/S   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -------------------------------------------------------------------
!   MJDSEC   I      S    INTEGRER MODIFIED JULIAN SECONDS
!   FSEC     I      S    FRACTIONAL MODIFIED JULIAN SECOND
!   slon     I      S    SON LONGITUDE
!   slat     I      S    SON LATITUDE
!   ratio    I      S    SUN ECLIPSE FACTOR
!   xdata    I      A    GRID DATA OF X ACCELERATION
!   ydata    I      A    GRID DATA OF Y ACCELERATION
!   zdata    I      A    GRID DATA OF Z ACCELERATION
!   ROT      I      A    ROTATION MATRIX FROM SBF TO TOD
!   SUN_PROBE I     A    SUN-SATELLITE UNIT VECTOR IN TOD
!   dist     I      S    SUN-SATELLITE DISTANCE
!   SOLACC   O      A    SOLAR RADIATION PRESSURE ACCELERATIONS IN TOD
!   GRPAR    O      A
!   KPART    I      S
!   NEQN     I      S
!   PARMVC   I      S
!   SCALEL   O      S
!**********************************************************************
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
!
      INCLUDE 'COMMON_DECL.inc'
      COMMON/BWREAL/SHADOW(2),SUNPCT,SCAREA,SCMASS,BWMEAN
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CRMI/RMI(9)
      COMMON/DTMGDN/TGTYMD,TMGDN1,TMGDN2,REPDIF,XTMGN
      COMMON/NPCOM /NPNAME,NPVAL(92),NPVAL0(92),IPVAL(92),IPVAL0(92),   &
     &              MPVAL(28),MPVAL0(28),NXNPCM
      COMMON/NPCOMX/IXARC ,IXSATP,IXDRAG,IXSLRD,IXACCL,IXGPCA,IXGPSA,   &
     &              IXAREA,IXSPRF,IXDFRF,IXEMIS,IXTMPA,IXTMPC,IXTIMD,   &
     &              IXTIMF,IXTHTX,IXTHDR,IXOFFS,IXBISA,IXFAGM,IXFAFM,   &
     &              IXATUD,IXRSEP,IXACCB,IXDXYZ,IXGPSBW,IXCAME,IXBURN,  &
     &              IXGLBL,IXGPC ,IXGPS, IXTGPC,IXTGPS,IXGPCT,IXGPST,   &
     &              IXTIDE,IXETDE,IXOTDE,IXOTPC,IXOTPS,IXLOCG,IXKF  ,   &
     &              IXGM  ,IXSMA ,IXFLTP,IXFLTE,IXPLTP,IXPLTV,IXPMGM,   &
     &              IXPMJ2,IXVLIT,IXEPHC,IXEPHT,IXH2LV,IXL2LV,IXOLOD,   &
     &              IXPOLX,IXPOLY,IXUT1 ,IXPXDT,IXPYDT,IXUTDT,IXVLBI,   &
     &              IXVLBV,IXXTRO,IXBISG,IXSSTF,IXFGGM,IXFGFM,IXLNTM,   &
     &              IXLNTA,IX2CCO,IX2SCO,IX2GM ,IX2BDA,IXRELP,IXJ2SN,   &
     &              IXGMSN,IXPLNF,IXPSRF,IXANTD,IXTARG,                 &
     &              IXSTAP,IXSSTC,IXSSTS,IXSTAV,IXSTL2,                 &
     &              IXSTH2,IXDPSI,IXEPST,IXCOFF,IXTOTL,NXNPCX
!
      INTEGER MJDSEC
      DOUBLE PRECISION FSEC,ratio,slon,slat,ROT(9)
      DOUBLE PRECISION xdata(73,144), ydata(73,144), zdata(73,144)
      DOUBLE PRECISION SUN_PROBE(3),SOLACC(3)
      DOUBLE PRECISION GRPAR(NEQN,3), PARMVC(1), SCALEL
      INTEGER KPART
! Local Variables
      DOUBLE PRECISION xacc, yacc, zacc, theta
      DOUBLE PRECISION npl(3), duml, q_array_rot(0:3)
      DOUBLE PRECISION array_axis(3),array_norm(3),arraySRP(3)
      DOUBLE PRECISION probe_sun(3),arrayTRR(3),busQRATE(3)
      DOUBLE PRECISION busASAR(3), nom_mass, k_mass
      DOUBLE PRECISION FACT(3)
      PARAMETER(nom_mass=8040.8D0)
!     PARAMETER(oneAU=149597870691.D0)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      DMJD=(TMGDN1+(DBLE(MJDSEC)/86400.D0)+FSEC)-2400000.5D0
      k_mass = nom_mass/SCMASS
      SOLACC(1) = 0.D0
      SOLACC(2) = 0.D0
      SOLACC(3) = 0.D0
      IF(ratio .EQ. 0.D0) GOTO 100
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! Compute the bus SRP acceleration
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! xacc,yacc, and zacc are the SRP acclerations due to the bus along
! SBF axes
      CALL interpolateX(slon,slat,xdata,xacc)
      CALL interpolateX(slon,slat,ydata,yacc)
      CALL interpolateX(slon,slat,zdata,zacc)
! Debug
!     print*,'UCL_ENVI: slon=',slon
!     print*,'UCL_ENVI: slat=',slat
!     print*,'UCL_ENVI: xacc=',xacc
!     print*,'UCL_ENVI: yacc=',yacc
!     print*,'UCL_ENVI: zacc=',zacc
! Tranform the bus acclerations from SBF to TOD
      SOLACC(1)=ROT(1)*xacc + ROT(4)*yacc + ROT(7)*zacc
      SOLACC(2)=ROT(2)*xacc + ROT(5)*yacc + ROT(8)*zacc
      SOLACC(3)=ROT(3)*xacc + ROT(6)*yacc + ROT(9)*zacc
! Debug
!     print*,'SOLACC in UCL_ENVI: '
!     print*, SOLACC
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! Compute the solar array BFS SRO accelerations
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! Compute optimal solar array normal.
! Compute normal to the plane containing the sun-probe vector and
! the BFS X axis (cross product).
! Note BFS X axis vector in TOD is (ROT(1),ROT(2),ROT(3)).
      CALL CROPDT(ROT(1),SUN_PROBE,npl)
! Normalize npl vector
      duml = SQRT(npl(1)*npl(1)+npl(2)*npl(2)+npl(3)*npl(3))
      npl(1) = npl(1)/duml
      npl(2) = npl(2)/duml
      npl(3) = npl(3)/duml
! Rotate BFS X axis by 22 degrees CCW around npl to give the array's
! logitudinal vector.
! Create the quaternion rotator
      theta = 22.D0*DEGRAD
      CALL CONVERT_FROM_ANGLE_AXIS(theta,npl,q_array_rot)
! Rotate the X axis
      CALL QUATERNION_ROTATE(ROT(1),q_array_rot,array_axis)
! Compute the solar array normal. This is the cross product of
! array_long_axis and npl vectors.
      CALL CROPDT(array_axis,npl,array_norm)
! Compute solar array SRP acceleration
      CALL ENVISAT_ARRAY_SRP(sun_probe,array_axis,                      &
     &                       array_norm,npl,arraySRP)
!
! Sum to get the total accelerations so far along the BFS frame though
! note that these are still represented in TOD frame.
! Perform distance scaling on this total
      duml = AU*AU/(dist*dist)
      SOLACC(1) = (SOLACC(1) + arraySRP(1))*duml
      SOLACC(2) = (SOLACC(2) + arraySRP(2))*duml
      SOLACC(3) = (SOLACC(3) + arraySRP(3))*duml
!
! Compute the BFS TRR accelerations due to the solar array
! array TRR is already distance scaled!!!
! sun_probe is the Sun-Probe vector, ARRAY_TRR needs Probe-Sun vector
      probe_sun(1) = -sun_probe(1)
      probe_sun(2) = -sun_probe(2)
      probe_sun(3) = -sun_probe(3)
      CALL ENVISAT_ARRAY_TRR(dist,probe_sun,array_axis,array_norm, &
     &                        arrayTRR)
!
! Sum total with array TRR to get final total
! Do mass scaling
      SOLACC(1) = (SOLACC(1) + arrayTRR(1))*k_mass
      SOLACC(2) = (SOLACC(2) + arrayTRR(2))*k_mass
      SOLACC(3) = (SOLACC(3) + arrayTRR(3))*k_mass
!
! Need to account for the sun eclipse factor
      SOLACC(1) = SOLACC(1)*ratio
      SOLACC(2) = SOLACC(2)*ratio
      SOLACC(3) = SOLACC(3)*ratio
!
 100  CONTINUE
!&&&&&&&&&&&&&&&&&&&&&
! qrates
!&&&&&&&&&&&&&&&&&&&&&
      CALL ENVISAT_QRATE(ROT(1),ROT(4),ROT(7),busQRATE,DMJD)
      SOLACC(1) = SOLACC(1) + busQRATE(1)*k_mass
      SOLACC(2) = SOLACC(2) + busQRATE(2)*k_mass
      SOLACC(3) = SOLACC(3) + busQRATE(3)*k_mass
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
! asar - this is mass scaled inside the function!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      CALL ENVISAT_ASAR(SCMASS,ROT(1),ROT(4),ROT(7),busASAR)
      SOLACC(1) = SOLACC(1) + busASAR(1)
      SOLACC(2) = SOLACC(2) + busASAR(2)
      SOLACC(3) = SOLACC(3) + busASAR(3)
!
! Debug
!     print*, SOLACC
!     stop
!
      IF(.NOT.LSASRD) RETURN
!
! Fill up partials
! Goto TOR from TOD
      FACT(1) = SOLACC(1)*RMI(1)+SOLACC(2)*RMI(2)+SOLACC(3)*RMI(3)
      FACT(2) = SOLACC(1)*RMI(4)+SOLACC(2)*RMI(5)+SOLACC(3)*RMI(6)
      FACT(3) = SOLACC(1)*RMI(7)+SOLACC(2)*RMI(8)+SOLACC(3)*RMI(9)
      !IND     = IPVAL0(IXSLRD)
      !SCALEL  = PARMVC(IND)
      GRPAR(KPART,1) = FACT(1)
      GRPAR(KPART,2) = FACT(2)
      GRPAR(KPART,3) = FACT(3)
! Denug
!     write(6,*)' dbg all GRPAR ',GRPAR(KPART,1),GRPAR(KPART,2),   &
!    &                              GRPAR(KPART,3),NEQN,KPART

      END SUBROUTINE UCL_ENVISAT
