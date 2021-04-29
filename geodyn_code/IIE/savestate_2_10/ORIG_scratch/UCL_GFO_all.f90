      SUBROUTINE  UCL_GFO_all(xbus,ybus,zbus,ratiop,dist,ACC,           &
     & GRPAR,ROT,KPART,NEQN,PARMVC,SCALE )
!-----------------------------------

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!
      COMMON/CRMI/RMI(9)
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
      DOUBLE PRECISION k_dist,NOM_mass,k_mass
      DIMENSION ACC(3),ROT(9),FACT(3)
      DIMENSION GRPAR(NEQN,3)
      DIMENSION PARMVC(1)

!     value of one Astronomical Unit in m
      PARAMETER(oneAU=149597870.691D0)

!     Nominal mass used to compute SRP/TRR model in Kg
      PARAMETER(NOM_mass = 369.048D0)
!     GFO_mass
      PARAMETER(GFO_mass = 369.048D0)

      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!     DIMENSION acc(3)

!     Compute and apply final mass, distance and flux scale terms
!     Compute the mass scale term
      k_mass = NOM_mass / GFO_mass
!     Compute the distance scale term
      k_dist = (oneAU*oneAU)/(dist*dist)
!     Apply the scalars
      ACC(1) = xbus * k_mass * k_dist * RATIOP
      ACC(2) = ybus * k_mass * k_dist * RATIOP
      ACC(3) = xbus * k_mass * k_dist * RATIOP

!
!     FILL UP PARTIALS
! GOTO TOD
       X1=ROT(1)*ACC(1) + ROT(4)*ACC(2) + ROT(7)*ACC(3)
       Y1=ROT(2)*ACC(1) + ROT(5)*ACC(2) + ROT(8)*ACC(3)
       Z1=ROT(3)*ACC(1) + ROT(6)*ACC(2) + ROT(9)*ACC(3)
!      write(6,*)'dbg TOTAL TOD ACCEL all ',X1,Y1,Z1

! GOTO TOR
      FACT(1)=X1*RMI(1)+Y1*RMI(2)+Z1*RMI(3)
      FACT(2)=X1*RMI(4)+Y1*RMI(5)+Z1*RMI(6)
      FACT(3)=X1*RMI(7)+Y1*RMI(8)+Z1*RMI(9)
!
      IND=IPVAL0(IXSLRD)
      SCALE=PARMVC(IND)
      GRPAR(KPART,1)=SCALE*FACT(1)
      GRPAR(KPART,2)=SCALE*FACT(2)
      GRPAR(KPART,3)=SCALE*FACT(3)
      write(6,*)' dbg all GRPAR ',GRPAR(KPART,1),GRPAR(KPART,2),   &
     &                              GRPAR(KPART,3),NEQN,KPART




      RETURN
      END
