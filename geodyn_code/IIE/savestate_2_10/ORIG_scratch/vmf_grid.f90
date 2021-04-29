      subroutine vmf_grid( DMJD, RLAT, RLON, HTG,HTS,HT_DIFF, &
     &                     ZD, VMF1H, VMF1W,zdh,zdw,AH,AW )

!********1*********2*********3*********4*********5*********6*********7**
! vmf_grid         00/00/00            0000.0    PGMR - ?


! FUNCTION:  access the vmf grid and call vmf1_ht

! I/O PARAMETERS:

!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------

!   DMJD     I    S    MJD (incl. fraction of day )
!   RLAT     I    S    latitude  in radians
!   RLON     I    S    longitude in radians
!   HTG      I    S    height in meters (MEAN HEIGHT OF GRID)
!   HTS      I    S    GEODYN station height from STAINF
!   HT_DIFF  I    S    Difference between station height and GRID height
!   ZD       I    S    zenith distance (radians)
!   VMF1H    O    S    hydrostatic mapping function
!   VMF1W    O    S    wet         mapping function
!   zdh      O    S    hydrostatic zenith delay
!   zdw      O    S    wet zenith delay


! COMMENTS:


!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE

      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY

      DATA ZERO/0.0D0/,ONE/1.0D0/,HUNDRD/100.0D0/,C1500/1.5D3/


!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************

!      write(6,*) "VMFGRD: HTG,HTS, :", HTG,HTS


!   VMF

!     AH      d      Hydrostatic coefficient a (Note 1)
!     AW      d      Wet coefficient a (Note 1)
!     DMJD    d      Modified Julian Date
!     DLAT    d      Latitude given in radians (North Latitude)
!     HTS     d      Ellipsoidal height given in meters (OF STATION)
!     ZD      d      Zenith distance in radians
!  Returned:
!     VMF1H   d      Hydrostatic mapping function (Note 2)
!     VMF1W   d      Wet mapping function (Note 2)



      DLAT = RLAT / degrad
      DLON = RLON / degrad


!write(6,'(//A,1x,F20.2)') 'vmfgrd: dmjd  ', dmjd
!write(6,'(A,2(1x,F10.5))') &
!           'vmfgrd: rlon, rlat ', rlon, rlat
!      write(6,'(A,2(1x,F10.5))') &
!           'vmfgrd: dlon, dlat ', dlon, dlat

!write(6,'(A,2(1x,F10.5))') &
!           'vmfgrd: HTG', HTG
!write(6,'(A,2(1x,F10.5))') &
!           'vmfgrd: ZD        ', ZD
!write(6,'(A,2(1x,F10.5))') &
!           'vmfgrd: ZD (deg)  ', ZD / degrad

!------------------------------------------

! calculate date and time

      imjd = INT( dmjd )
      fmjd = dmjd - REAL( imjd, kind = 8 )

!      write(6,'(A,1x, I12, 1x, F10.6)') &
!      'vmfgrd: imjd, fmjd  ', imjd, fmjd

!------------------------------------------

! access vmf grid file to get AH and AW

!write(6,'(/A/)') 'vmfgrd: call read_vmf1_grid_file '

      call read_vmf1_grid_file( imjd, fmjd, &
     &       dlon, dlat, ah, aw, zdh, zdw)


!      write(6,*) "VMFGRD: ah,ZDH is:", ah,ZDH
!      write(6,*) "VMFGRD: aw,ZDW is:", aw,ZDW
!------------------------------------------------------------------
!******************************************************************************
!
! compute the VMF mapping functions, VMF1H and VMF1W
! from coefficients AH and AW
!
!******************************************************************************


      DMJD = INT(DMJD/86400.0D0)+30000


      call vmf1_ht( AH, AW, DMJD, RLAT,HTS, ZD, VMF1H, VMF1W )

!      write(6,'(A,2(1x,E15.7))') 'vmfgrd: VMF1H, VMF1W  ',        &
!     &                                    VMF1H, VMF1W


!******************************************************************************
! DETERMINE PRESSURE AT MEAN GRID HEIGHT
!
!   zhd(h) = 0.0022768*(P/(1-0.00266*cos(2*lat)-(0.28*10(-6))*h))     eq.(1)
!
!   p(h) = 1013.25(1-0.0000226*h)^5.225            eq. (2)
!
!   zdw = zdwg*e^(-(hs-hg)/2000)               eq.  (3)
!
! EQUATIONS CORRESPOND TO PAPER:
! Implementation and testing of the gridded Vienna Mapping Function 1 (VMF1)
!          by J. Kouba ; (Journal of Geodesy) J GEOD (2008) 82:193-205
!******************************************************************************

      DENOM = (1-0.00266D0*COS(2*RLAT)-(0.28D-6)*HTG)

      PRESSURE_G=(ZDH*DENOM)/0.0022768D0


!******************************************************************************
! DETERMINE THE DERIVATIVE OF EQUATION 2 WRT HEIGHT
!    dp
!    -- =-a*b*c*(1-bh)^(c-1)
!    dh
!
! DETERMINE THE STATION PRESSURE WRT GRID HEIGHT AND STATION HEIGHT
!    p(hs) = p(hg) + (dp/dh)*(hs-hg)
!
!******************************************************************************

      CONST = -(1013.25)*(.0000226)*(5.225)

      DPRESS = CONST*(1-.0000226*HTG)**(5.225-1)

      PRESSURE_S = PRESSURE_G + DPRESS*(HTS - HTG)

!ZDH using equation 1
      ZDH = 0.0022768*(PRESSURE_S/                                   &
     &           (1-0.00266D0*COS(2*RLAT)-(0.28D-6)*HTS))


!ZDW using equation 3
       EXP_ZDW = EXP(-(HT_DIFF)/2000)

       ZDW = ZDW*EXP_ZDW


      RETURN

      end subroutine vmf_grid
