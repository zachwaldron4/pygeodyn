!$ECFSET
      SUBROUTINE ECFSET(LSET, xp, indcr, indvr, r, rsq, isat, nsat,     &
     &                  gmr3, AA, II, LL)
!********1*********2*********3*********4*********5*********6*********7**
! ECFSET           04/18/91            9105.0    PGMR - J. McCarthy
!
! FUNCTION         set ECF quantities when the GEOPOL option and
!                  dynamic polar motion are used
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LSET     I         .TRUE.  - use RMB matrix with GEOPOL rotation
!                      .FALSE. - use RMB0 matrix without GEOPOL rotation
!   XP       I         inertial Cartesian satellite coordinates
!   INDCR    I         array pointer
!   INDVR    I         array pointer
!   R        I         length of central body - sat vector
!   RSQ      I         R**2
!   ISAT     i         ordinal number of satellite in the current set
!   NSAT     i         total number of satellites  in the current set
!   GMR3
!   AA      I/O   A    DYNAMIC ARRAY FOR REAL VARIABLES
!   II      I/O   A    DYNAMIC ARRAY FOR INTEGER VARIABLES
!   LL      I/O   A    DYNAMIC ARRAY FOR LOGICAL VARIABLES
!
! COMMENTS
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      PARAMETER (  ZERO = 0.D0 )
!
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/CRMBI/RMBI(9)
      COMMON/CRMI/RMI(9)
      COMMON/VRBLOK/SINPSI,COSPSI,XLAMDA,GMR,AOR
!
      DIMENSION XP(3,NSAT,2)
      DIMENSION AA(1),II(1),LL(1)
!
      data kentry/0/
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!
!      kentry = kentry + 1
!      lentry  = kentry .lt. 20  .or. mod(kentry,100) .eq. 0
!      if( lentry ) write(6,*) 'ecfset: kentry ', kentry
!      if( lentry ) write(6,*) 'ecfset: lset ', lset
!      if( lentry ) write(6,*) 'ecfset: indcr ', indcr
!      if( lentry ) write(6,*) 'ecfset: indvr ', indvr
!
!
!   GET VARIOUS BODY FIXED QUANTITIES ASSOCIATED WITH SATELLITE
!
!      GET BODY FIXED XYZ OF SATELLITE
!
      if ( LSET ) then
!
         AA(INDVR+8)=RMB(1)*XP(1,ISAT,1)+RMB(4)*XP(2,ISAT,1)+           &
     &                   RMB(7)*XP(3,ISAT,1)
         AA(INDVR+9)=RMB(2)*XP(1,ISAT,1)+RMB(5)*XP(2,ISAT,1)+           &
     &                   RMB(8)*XP(3,ISAT,1)
         AA(INDVR+10)=RMB(3)*XP(1,ISAT,1)+RMB(6)*XP(2,ISAT,1)+          &
     &                    RMB(9)*XP(3,ISAT,1)
!
!
!        if( lentry ) write(6,*) 'ecfset: ecf xyz for grav ',
!    1         aa(indvr+8), aa(indvr+9), aa(indvr+10)
!
      else
!
         AA(INDVR+8)=rmb0(1)*XP(1,ISAT,1)+rmb0(4)*XP(2,ISAT,1)+         &
     &                   rmb0(7)*XP(3,ISAT,1)
         AA(INDVR+9)=rmb0(2)*XP(1,ISAT,1)+rmb0(5)*XP(2,ISAT,1)+         &
     &                   rmb0(8)*XP(3,ISAT,1)
         AA(INDVR+10)=rmb0(3)*XP(1,ISAT,1)+rmb0(6)*XP(2,ISAT,1)+        &
     &                    rmb0(9)*XP(3,ISAT,1)
!
!
!        if( lentry ) write(6,*) 'ecfset: ecf xyz for tides ',
!    1         aa(indvr+8), aa(indvr+9), aa(indvr+10)
!
      endif
!
!          FILL VRBLOK & VRARAY
!
!-----------------------------------------------------------------------
!
!          VRARAY = BODY FIXED INFORMATION ABOUT SATELLITE
!                   INCLUDING R AND R**2
!
!     ....AA(INDVR+3)  = R
!     ....AA(INDVR+4)  = RSQ
!     ....AA(INDVR+5)  = Xecf**2 + Yecf**2
!     ....AA(INDVR+6)  = SQRT( Xecf**2 + Yecf**2 )
!     ....AA(INDVR+7)  = GMR
!     ....AA(INDVR+8)  = Xecf
!     ....AA(INDVR+9)  = Yecf
!     ....AA(INDVR+10) = Zecf
!-----------------------------------------------------------------------
!
      XLAMDA=ATAN2(AA(INDVR+9),AA(INDVR+8))
!
!     ....AA(INDVR+5)= Xecf**2 + Yecf**2
      AA(INDVR+5)=AA(INDVR+8)*AA(INDVR+8)+                              &
     &                   AA(INDVR+9)*AA(INDVR+9)
!
!     ....AA(INDVR+6)= SQRT( Xecf**2 + Yecf**2 )
      AA(INDVR+6)=SQRT(AA(INDVR+5))
!
!     ....RSQ=AA(INDVR+5)+AA(INDVR+10)*AA(INDVR+10)
!     ....R=SQRT(RSQ)
!
      AA(INDVR+4)=RSQ
      AA(INDVR+3)=R
!
!     ....SINPSI, COSPSI are sine and cosine of geocentric latitude
      SINPSI=AA(INDVR+10)/R
      COSPSI=AA(INDVR+6)/R
      AOR=AE/R
      GMR=GM/R
      GMR3=GMR/RSQ
      AA(INDVR+7)=GMR
!
!     if( lentry ) write(6,*) 'ecfset: AA(INDVR+3--10) ',
!    1                        ( aa(indvr+kkk),kkk=3,10)
!
!     if( lentry ) write(6,*) 'ecfset: sinpsi, cospsi ', sinpsi, cospsi
!     if( lentry ) write(6,*) 'ecfset: aor, gmr,gmr3 ', aor, gmr, gmr3
!
!-----------------------------------------------------------------------
!
!           CORPAR = TRANSFORMATION MATRIX FOR BODY FIXED R,PSI,LAMDA
!                    TO BODY FIXED XYZ (CORPAR(7)=SINPSI)
!
!     ....AA(INDCR)   = Xecf / R
!     ....AA(INDCR+1) = -Xecf * Zecf
!     ....                 / ( RSQ * SQRT( Xecf**2 + Yecf**2 ) )
!     ....AA(INDCR+2) = -Yecf / ( Xecf**2 + Yecf**2 )
!     ....AA(INDCR+3) = Yecf / R
!     ....AA(INDCR+4) = -Yecf * Zecf
!     ....                 / ( RSQ * SQRT( Xecf**2 + Yecf**2 ) )
!     ....AA(INDCR+5) = Xecf / ( Xecf**2 + Yecf**2 )
!     ....AA(INDCR+6) = SINPSI
!     ....AA(INDCR+7) = SQRT( Xecf**2 + Yecf**2 ) /RSQ
!     ....AA(INDCR+8) = ZERO
!-----------------------------------------------------------------------
!
!     ....AA(INDCR) = Xecf / R
      AA(INDCR)=AA(INDVR+8)/R
!
!     ....AA(INDCR+3) = Yecf / R
      AA(INDCR+3)=AA(INDVR+9)/R
      AA(INDCR+6)=SINPSI
!
!     ....TEMP = -Zecf / ( RSQ * SQRT( Xecf**2 + Yecf**2 ) )
      TEMP=-AA(INDVR+10)/(RSQ*AA(INDVR+6))
!
!     ....AA(INDCR+1) =  -Xecf * Zecf
!     ....                 / ( RSQ * SQRT( Xecf**2 + Yecf**2 ) )
      AA(INDCR+1)=AA(INDVR+8)*TEMP
!
!     ....AA(INDCR+4) =  -Yecf * Zecf
!     ....                 / ( RSQ * SQRT( Xecf**2 + Yecf**2 ) )
      AA(INDCR+4)=AA(INDVR+9)*TEMP
!
!     ....AA(INDCR+7) = SQRT( Xecf**2 + Yecf**2 ) /RSQ
      AA(INDCR+7)=AA(INDVR+6)/RSQ
!
!     ....AA(INDCR+2)=-Yecf / ( Xecf**2 + Yecf**2 )
      AA(INDCR+2)=-AA(INDVR+9)/AA(INDVR+5)
!
!     ....AA(INDCR+5)= Xecf / ( Xecf**2 + Yecf**2 )
      AA(INDCR+5)=AA(INDVR+8)/AA(INDVR+5)
      AA(INDCR+8)=ZERO
!     if( lentry ) write(6,*) 'ecfset: AA(INDCR+0 -- +10) ',
!    1             (aa(indcr+kkk), kkk=0,10)
!
!
!     RMBI MATRIX TO GO FROM RFL = (RADIUS, LAT(PHI), LON(LAMBDA))
!                 TO TRUE OF REF
!
      if ( LSET ) then
!
!     ....reset the coordinate system rotation matrix
!     ....to be with the geopol rotation
!     ....use matrix rmb instead of rmb0
!
      RMBI(1)=RMB(1)*AA(INDCR  )+RMB(2)*AA(INDCR+3)+RMB(3)*AA(INDCR+6)
      RMBI(2)=RMB(4)*AA(INDCR  )+RMB(5)*AA(INDCR+3)+RMB(6)*AA(INDCR+6)
      RMBI(3)=RMB(7)*AA(INDCR  )+RMB(8)*AA(INDCR+3)+RMB(9)*AA(INDCR+6)
      RMBI(4)=RMB(1)*AA(INDCR+1)+RMB(2)*AA(INDCR+4)+RMB(3)*AA(INDCR+7)
      RMBI(5)=RMB(4)*AA(INDCR+1)+RMB(5)*AA(INDCR+4)+RMB(6)*AA(INDCR+7)
      RMBI(6)=RMB(7)*AA(INDCR+1)+RMB(8)*AA(INDCR+4)+RMB(9)*AA(INDCR+7)
      RMBI(7)=RMB(1)*AA(INDCR+2)+RMB(2)*AA(INDCR+5)+RMB(3)*AA(INDCR+8)
      RMBI(8)=RMB(4)*AA(INDCR+2)+RMB(5)*AA(INDCR+5)+RMB(6)*AA(INDCR+8)
      RMBI(9)=RMB(7)*AA(INDCR+2)+RMB(8)*AA(INDCR+5)+RMB(9)*AA(INDCR+8)
!
!      if( lentry ) write(6,*) 'ecfset: rmb ', rmb
!
      else
!
!     ....reset the coordinate system rotation matrix
!     ....to be without the geopol rotation
!     ....use matrix rmb0 instead of rmb
!
      RMBI(1)=rmb0(1)*AA(INDCR )+rmb0(2)*AA(INDCR+3)+rmb0(3)*AA(INDCR+6)
      RMBI(2)=rmb0(4)*AA(INDCR )+rmb0(5)*AA(INDCR+3)+rmb0(6)*AA(INDCR+6)
      RMBI(3)=rmb0(7)*AA(INDCR )+rmb0(8)*AA(INDCR+3)+rmb0(9)*AA(INDCR+6)
      RMBI(4)=rmb0(1)*AA(INDCR+1)+rmb0(2)*AA(INDCR+4)                   &
     &       +rmb0(3)*AA(INDCR+7)
      RMBI(5)=rmb0(4)*AA(INDCR+1)+rmb0(5)*AA(INDCR+4)                   &
     &       +rmb0(6)*AA(INDCR+7)
      RMBI(6)=rmb0(7)*AA(INDCR+1)+rmb0(8)*AA(INDCR+4)                   &
     &       +rmb0(9)*AA(INDCR+7)
      RMBI(7)=rmb0(1)*AA(INDCR+2)+rmb0(2)*AA(INDCR+5)                   &
     &       +rmb0(3)*AA(INDCR+8)
      RMBI(8)=rmb0(4)*AA(INDCR+2)+rmb0(5)*AA(INDCR+5)                   &
     &       +rmb0(6)*AA(INDCR+8)
      RMBI(9)=rmb0(7)*AA(INDCR+2)+rmb0(8)*AA(INDCR+5)                   &
     &       +rmb0(9)*AA(INDCR+8)
!
!      if( lentry ) write(6,*) 'ecfset: rmb0 ', rmb0
!
      endif
!
!      if( lentry ) write(6,*) 'ecfset: rmbi ', rmbi
!
      RETURN
      END
