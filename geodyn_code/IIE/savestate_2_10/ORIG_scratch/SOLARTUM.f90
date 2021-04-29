!$SOLARTUM
      SUBROUTINE SOLARTUM(MJDSBL,FSEC,HELIOS,ELEM,IFLAG1,NINNER,IBLOCK,&
     & NPRN,PARMV0,PARMVC,ACCSOLRAD,PSOLRAD,IFLAG3,ISATID,AA,LADJ,IADJ,&
     & ISEQSC,IPRN,LBOXWING_PRESENT)
!
!********1*********2*********3*********4*********5*********6*********7**
! SOLARTUM              02/20/13            1304.0    Oscar Colombo
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   HELIOS   I  Geocenter -> heliocenter vector, in meters.
!   ELEM     I  satellite position/velocity vector, in meters.
!   IFLAG1   I =1 if both acceleration and variational forcing
!               terms (referred to here as "partials") are to be
!               calculated and returned to "solwrap";
!              = 0 if only the acceleration due to a slight
!               departure of the solar panels orientation from
!               nominal, and corresponding partial, are to be returned,
!   IFLAG3   I = 1 if te y-bias acceleration and corresponding
!             partial are to be calculated and returned;
!              = 0 otherwise.
!   NINNER      Number of current iteration (it is 1 for 1st iteration).
!   IBLOCK   I  Satellite block number (II=1, IIA=2, IIR=3, IIF=4, I=5).
!   IPRN     I  Satellite PRN number.
!
! ACCSOLRAD  O  Array with 3-vector solar radiation presure acceleration
!               and partials, except for those of the solar panels.
! PSOLRAD    O  Solar panel solrad acceleration and partials
!               (the only output when IFLAG1 = 0).
! Subroutines called:
!
! solrad_tum: Implements the adjustable box-wing solrad model
!             calculation of the satellite solrad acceleration
!             and of the partials of the adjustable parameters.
! VPROD:      Returns the vector product of two input vectors.
! SPROD:      Returns the scalar product of two input vectors.
! VNORM:      Returns a unit vector pointing in the direction of the
!             input vector, as well as the input vector's magnitude.
! GROTATE:    Returns the rotated version of an input vector, with
!             the rotation angle specified in the subroutine call.
! ECLIPSOL:   Returns the extent of sun disk visible from
!             the satellite. It is 1 in full sunlight and 0 in shadows,
!             during eclipses (other than astronomical solar eclipses).

! subroutine "solrad_tum", that returns the values of the solar
! radiation pressure acceleration and corresponding "partials"
! (actually the forcing terms of the variational equations)
! in arrays "ACCSOLRAD" and "PSOLRAD", respectively.
! If IFLAG1 = 0, only the acceleration component from the solar
! panels and the two solar panel partials are returned in those
! arrays, with the unused elements set to zero.
!
!********1*********2*********3*********4*********5*********6*********7**
!
      IMPLICIT DOUBLE PRECISION   (A-H,O-Z),LOGICAL (L)
      SAVE
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/LSTRT/LSTART
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

! OPARTIALS: as above, but for the parameters of the "box".
! SPPARTIALS: solar panel "wings" parameter partials (forcing terms
!          of the corresponding variational equations).
! OPARTIALS: as above, but for the parameters of the "box".
      DIMENSION SPPARTIALS(3,2),OPARTIALS(3,7),PSOLRAD(3,9)

      DIMENSION PARAMSP(2),PARAMXP(2),PARAMZP(2),PARAMZM(2),PARAMYB(1)
      DIMENSION SATPOS(3),SATVEL(3),HELIOS(3),SUN(3),SATN(3)

      DIMENSION RM(3,3),ACCELTOT(3),SUNY(3),SUNVN(3),BVNN(3),y(3),&
     &          BVN(3),ACCSOLRAD(3),SATNXT(3),SATNXTN(3),XAX(3),  &
     &          XN(3),YN(3),ZN(3),ACROSS(3),ACROSSN(3),ALONGN(3), &
     &          SUNX(3),HELION(3),xnr(3),ynr(3),ELEM(6)
      DIMENSION ISATID(1)
      DIMENSION AA(1)
      DIMENSION PARMV0(1),PARMVC(1)
      DIMENSION SBPERP(3),SUNBETAV(3)
!
      DATA DTIME/10.D0/,ISTART/1/,SINEPSDOTOLD/1.D0/,AU/149.D9/,  &
     &          YAWMIN/1.D0/,IKIND/0/
!
! Define general constants
!
      IF(ISTART.EQ.1) THEN
      HPI = PI/2.D0
      RADEG = 1.D0/DEGRAD
      CLOSEANGLE = 15.D0*DEGRAD

      ISTART = 0
      ENDIF
!
! Find:
! the sun-pointing unit vector in the satellite's body-frame,
! the sine and cosine of the Earth-satellite-Sun angle "epsilon",
! the sign of the differential change in angle epsilon over a
! differential increment of time "dt",
! and the body-frame-to-inertial-frame rotation matrix "RM".
!
! "HELIOS" is the vector pointing from the geocenter to the sun (center);
! "SATPOS" is the vector from the geocenter to the satellite;
! "SATVEL" is the satellite velocity vector in inertial coordinates.
!
! Unpaking "elem":
!
      DO I = 1,6
      IF(i.le.3) THEN
      SATPOS(I) = elem(I)
      ELSE
      SATVEL(i-3) = elem(I)
      ENDIF
      ENDDO
!
      DO I = 1,3
      SUNY(I) = HELIOS(I)-SATPOS(I)
      ENDDO

      CALL VNORM(SUNY,SUN,XHELIOSMOD)
      CALL VNORM(SATPOS,SATN,SATPOSMOD)
      CALL VNORM(HELIOS,HELION,HELIOSMOD)
!
      DO I = 1,3
      zn(I) = -SATN(I)
! Body-frame "z" axis is positive down
!                          towards the Earth's center.
      ENDDO
!
!----------------------------------------------------------------------
! Find angle Beta0 between sun vector and orbit plane:
!
! But first, find, in inertial space, the vectors in the along
! and across directions, completing, with SATN, the orbit-fixed triad.
!
!
      CALL VPROD(SATN,SATVEL,ACROSS)
      CALL VNORM(ACROSS,ACROSSN,SACROSS)
      CALL VPROD(ACROSSN,SATN,ALONGN)
      CALL SPROD(HELION,ACROSSN,COSALPHA0)
      BETA0 = HPI-ACOS(COSALPHA0)
      COSBETA = COS(BETA0)
      SINBETA = SIN(BETA0)
      BETA0ABS = ABS(BETA0)
      BETA0DEG = RADEG*BETA0
!
!----------------------------------------------------------------------
!
!     INITIALIZE IYAWER TO 1 = USE NOMINAL IYAWER FLAG IS OUTPUT FROM
!     YAWGPS. IT WILL BE 0 IF WE NEED TO CORRECT THE YAW DUE TO ECLIPSES
      IYAWER = 1
!     INITIALIZE NOMYAW TO 1 = USE NOMINAL YAW
      NOMYAW = 1
      IF(ABS(BETA0DEG).LT.14.D0) THEN
      NOMYAW = 0
      ENDIF
!
!----------------------------------------------------------------------
!
! Find the mu orbit angle from sat. position to orbital midnight.
!
      DO I = 1,3
      SUNX(I) = -(HELION(I)-COSALPHA0*ACROSSN(I))
      ENDDO

      CALL VNORM(SUNX,SUNBETAV,SUNXMOD)
      CALL SPROD(SATN,SUNBETAV,COSMU)
      call vprod(sunbetav,acrossn,sbperp)
      call sprod(satn,sbperp,sgnmu)

      ORBMU = ACOS(COSMU)*(-SIGN(1.D0,SGNMU))
      SINMU = SIN(ORBMU)

!----------------------------------------------------------------------
!
!     Finding the sine and cosine of epsilon
      CALL SPROD(ZN,SUN,COSEPSILON)
      IF(ABS(COSEPSILON).GT.1.D0.and.ABS(BETA0).LT.1.D-6) &
     &COSEPSILON = 1.D0*SIGN(1.D0,COSEPSILON)
      SINEPSILON = SQRT(1.D0-COSEPSILON**2)
      DO I = 1,3                      ! Finding the SIGN of d/dt(epsilon)
      SATNXT(I) = -(SATPOS(I)+SATVEL(I)*dtime)
      ENDDO
      CALL VNORM(SATNXT,SATNXTN,SATNXTMOD)
      CALL SPROD(SATNXTN,SUN,COSEPSILONXT)
      DCOSEPSILON = COSEPSILONXT-COSEPSILON
      IF(ABS(DCOSEPSILON).GT.1.D-6) THEN
      SINEPSDOT = -SIGN(1.D0,DCOSEPSILON)
      ELSE
      SINEPSDOT = SINEPSDOOLDT
      ENDIF
      SINEPSDOTOLD = SINEPSDOT
!
!----------------------------------------------------------------------
!
! Finding the body axes xn, yn, zn in the inertial frame:
!
      DO I = 1,3
      XAX(I) = SUN(I)-ZN(I)*COSEPSILON
      ENDDO
      CALL VNORM(XAX,XN,XAXNORM)
      CALL VPROD(ZN,XN,YN)
      IF(ABS(COSMU).GT.0.9999999D0.and. &
     &ABS(COSBETA).GT.0.9999999D0.and.IYAWER.EQ.0) THEN
! Satellite is too close to a singular
! point of nominal yaw, either noon or midnight.
      DO I = 1,3
      XN(I) = ACROSSN(I) ! Define nominal X body axis as normal to orbit
      ENDDO
      CALL VPROD(ZN,XN,YN)
      ENDIF
!
!----------------------------------------------------------------------
!
! At this point, we will either apply nominal yaw OR we will CALL YAWGPS
! to compute the yaw during eclipses and maneuvers.
      yawnominal = ATAN2(-TAN(BETA0),SIN(ORBMU)) ! (Bar-Sever, 1995)
      YAW=yawnominal*radeg


       IF(NOMYAW.EQ.0) THEN  ! that is beta angle< 14 deg
       CALL YAWGPS(MJDSBL,FSEC,ISATID,ELEM,YAWN,YAW,IYAWER,ISEQSC,AA)
!
      YAWNOM = YAWN*RADEG

      IF(IYAWER.GT.0) THEN
!     WRITE(6,*)'dbg USE NOM YAW',BETA0DEG,IYAWER,IBLOCK,IPRN,YAWNOM
      ELSE
!     WRITE(6,*)'dbg USE COR YAW',BETA0DEG,IYAWER,IBLOCK,IPRN,YAWNOM,YAW
      ENDIF

      IF(IYAWER.EQ.1) GOTO 1000 !That is if we still need to apply nomina

      CALL SPROD(XN,ALONGN,COSYAWN)
      YAWN = -ACOS(COSYAWN)

!      IF(iblock.EQ.3) yawn = yawn-pi
!Block IIR yaw is 180 degrees off. But in Geodyn it is not,
!so uncomment this statement when the calling programs follows usual convention.
! Debugging statements:
!     yawnominal = atan2(-dtan(BETA0),dsin(ORBMU)) ! (Bar-Sever, 1995)
!     print*,' yawn, yawnominal (deg) ',yawn*RADEG,yawnominal*RADEG
!     print*,' BETA0, ORBMU (deg) ',BETA0*RADEG,ORBMU*RADEG
!

!     YAWNOM = YAWN*RADEG
!        write(6,*)' dbg NOMINAL YAW IN SOLARTUM ',yawnom
      YAWNMYAWNG = yawnom-yaw
      IF(ABS(ABS(YAWNMYAWNG)-360.D0).LT.1.D-3) YAWNMYAWNG = 360.D0
      YAWNDEGDIFF = MOD(YAWNMYAWNG,360.D0)

      IF(ABS(YAWNDEGDIFF).GT.YAWMIN) THEN
      YAWNDIFF = YAWNDEGDIFF*DEGRAD
! If "YAWNDIFF" exceeds a minimum threshold, switch from nominal
! to external (i.e. Geodyn's) yaw by rotating about zn (still working
! in inertial space) the xn, yn, zn body-fixed triad accordingly:
!
      CALL GROTATE(YAWNDIFF,XN,XNR,ZN,IERR,IKIND)
      CALL GROTATE(YAWNDIFF,YN,YNR,ZN,IERR,IKIND)
      DO I  = 1,3
      XN(I) = XNR(I)
      YN(I) = YNR(I)
      ENDDO
      ENDIF

 1000 CONTINUE

      ENDIF   ! IF(NOMYAW.EQ.0)
!
      DO I = 1,3         ! Finding the rotation matrix "RM"
!                          to convert results from body-fixed back to inertial.
      RM(i,1) = XN(I)
      RM(i,2) = YN(I)
      RM(i,3) = ZN(I)
      ENDDO
!
! Finding the sun vector in body-fixed coordinates:
!
!     IF(IYAWER.EQ.0) THEN  ! Assuming a nominal yaw,
      IF(IYAWER.EQ.1) THEN  ! Assuming a nominal yaw,
      SUNVN(1) = SINEPSILON  ! Find the unit satellite-sun pointing
!                              vector "SUNVN" in the body-fixed frame.
      SUNVN(2) = 0.D0
      SUNVN(3) = COSEPSILON
      ELSE  ! In actual non-nominal yaw mode.
      CALL SPROD (XN,SUN,SUN1)
      CALL SPROD (YN,SUN,SUN2)
      SUNVN(1) = SUN1
      SUNVN(2) = SUN2
      SUNVN(3) = COSEPSILON
      ENDIF

!
! Find, in the inertial-frame, the unit vector "BVNN",
! parallel to solar panels,
! but perpendicular to their common axis:
!
      CALL VPROD(yn,sun,BVNN)
      IF(IYAWER.EQ.0.D0) THEN
      IF(ABS(COSMU).GT.0.9999999D0.and. &
     &ABS(COSBETA).GT.0.9999999D0) THEN
      DO I = 1,3
      BVNN(I) = ACROSSN(I)*SINEPSDOT
      ENDDO
      ENDIF
      ELSEIF(IYAWER.EQ.1) THEN
!
! Put here, when the actual solar panels' orientation,
! with a non-nominal yaw becomes known, the necessary code
! to obtain the corresponding orientation of BVNN, which will also
! depend on the Block of the satellite (I, II, IIA, IIR, IIF).
!
! ...
!
      ENDIF
!
! Find the body-fixed vector BVN that corresponds to the BVNN
! in the inertial frame:
!
      CALL SPROD(XN,BVNN,BVN(1))
      CALL SPROD(ZN,BVNN,BVN(3))
      BVN(2) = 0.D0
!
! Find if the satellite is in the penumbra or umbra region
! and modify the solar irradiance constant by a variable
! scale factor "S0FACT" accordingly:
!
      S0FACT = 1.D0
      EPSILONR = ACOS(COSEPSILON)
      RSAT =SATPOSMOD
      IF(ABS(EPSILONR).LT.CLOSEANGLE.and.COSMU.GT.0.D0) THEN
!
      CALL ECLIPSOL(RSAT,XHELIOSMOD,EPSILONR,AREA,S0FACT)
!
      ENDIF
      ARFR=S0FACT
      S0FACT = S0FACT*au**2/XHELIOSMOD**2 ! Taking into account
!                                the true distance satellite to sun "satsunmod".
!
! Reset variables at the start of a new GEODYN iteration.
!
!     if the number of adjusted parameters for the GPS box-wing model
!     is  not zero
!     IFLAG2=NINNER-1
! IFLAG2 = 0, use default parameters in data statements;
!        > 0, use parameter values estimated in a previous
!             iteration of the solution.
!
        IF(IADJ.GT.0)  THEN

      INDX   = IPVAL0(IXGPSBW)+9*(IADJ-1)
!     IF(.NOT.LSTART) write(6,*)' dbg INDX ',IPVAL0(IXGPSBW), INDX
!
!     IF THERE IS ADJUSTMENT IN THE SECOND ITERATION WE SHOULD DO THIS
      PARAMSP(1) = PARMVC(INDX)
      PARAMSP(2) = PARMVC(INDX+1)
      PARAMXP(1) = PARMVC(INDX+2)
      PARAMXP(2) = PARMVC(INDX+3)
      PARAMZP(1) = PARMVC(INDX+4)
      PARAMZP(2) = PARMVC(INDX+5)
      PARAMZM(1) = PARMVC(INDX+6)
      PARAMZM(2) = PARMVC(INDX+7)
      PARAMYB(1) = PARMVC(INDX+8)
!     IF(.NOT.LSTART) THEN
!     write(6,*)'dbg INITIALIZE PRMS'
!     write(6,*)'dbg',PARMVC(INDX),INDX
!     write(6,*)'dbg',PARMVC(INDX+1),INDX+1
!     write(6,*)'dbg',PARMVC(INDX+2),INDX+2
!     write(6,*)'dbg',PARMVC(INDX+3),INDX+3
!     write(6,*)'dbg',PARMVC(INDX+4),INDX+4
!     write(6,*)'dbg',PARMVC(INDX+5),INDX+5
!     write(6,*)'dbg',PARMVC(INDX+6),INDX+6
!     write(6,*)'dbg',PARMVC(INDX+7),INDX+7
!     write(6,*)'dbg',PARMVC(INDX+8),INDX+8
!     write(6,*)'dbg',PARMV0(INDX),INDX
!     write(6,*)'dbg',PARMV0(INDX+1),INDX+1
!     write(6,*)'dbg',PARMV0(INDX+2),INDX+2
!     write(6,*)'dbg',PARMV0(INDX+3),INDX+3
!     write(6,*)'dbg',PARMV0(INDX+4),INDX+4
!     write(6,*)'dbg',PARMV0(INDX+5),INDX+5
!     write(6,*)'dbg',PARMV0(INDX+6),INDX+6
!     write(6,*)'dbg',PARMV0(INDX+7),INDX+7
!     write(6,*)'dbg',PARMV0(INDX+8),INDX+8
!     ENDIF

      ENDIF

!
      DO I = 1,3
      ACCELTOT(I) = 0.D0
      ENDDO
!
! Call the subroutine that calculates the solrad acceleration and partials:
!
!     IF(IYAWER.EQ.0) THEN

      CALL SOLRAD_TUM(IFLAG1,SUNVN,BVN,ACCELTOT,                       &
     & SPPARTIALS,OPARTIALS,PARAMSP,PARAMXP,PARAMZP,PARAMZM,PARAMYB,   &
     & IBLOCK,NPRN,SINEPSDOT,SINEPSILON,COSEPSILON,                    &
     & S0FACT,IFLAG3,LADJ,LBOXWING_PRESENT)
!
! Here should go a CALL to another subroutine for calculating the solrad.
! pressure acceleration, once the yaw is not nominal during eclipse
! maneuvers -- whenever such a feature is implemented.
!
! .....
!
!     ENDIF
!
! Convert acceleration and partial vectors from body-fixed to inertial:
!
! (1) Total acceleration due to solrad:
!     (Only the contribution from solar panels if IFLAG1 = 0)
!
      DO I = 1,3
      ACCSOLRAD(I) = 0.D0
      DO J = 1,3
      ACCSOLRAD(I) = ACCSOLRAD(I)+RM(I,J)*ACCELTOT(J) ! Calculating total
      ENDDO
      ENDDO

      IF(.NOT.LADJ) RETURN
!
! (b) Partials:
!     (Only those for the solar panels' scale factor
!      and a small orientation lag angle, if IFLAG1 = 0)
!
      DO K = 1,2
      DO I = 1,3
      PSOLRAD(I,K) = 0.D0
      ENDDO
      ENDDO
      DO K = 1,2
      DO I = 1,3
      DO J = 1,3
      PSOLRAD(I,K) = PSOLRAD(I,K)+RM(I,J)*SPPARTIALS(J,K)
      ENDDO
      ENDDO
      ENDDO
      IF(IFLAG1.EQ.1) THEN
      DO K = 3,9
      DO I = 1,3
      PSOLRAD(I,K) = 0.D0
      ENDDO
      ENDDO
      DO K = 1,7
      DO I = 1,3
      DO J = 1,3
      PSOLRAD(I,K+2) = PSOLRAD(I,K+2)+RM(I,J)*OPARTIALS(J,K)
      ENDDO
      ENDDO
      ENDDO
      ENDIF
!
      RETURN
      END
