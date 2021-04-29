!$ ENVISAT_ARRAY_TRR
      SUBROUTINE ENVISAT_ARRAY_TRR(distance,psun_BFS,array_axis_BFS, &
     &                             array_norm_BFS,arrayTRR)
      IMPLICIT NONE
      INTEGER I
      DOUBLE PRECISION psun_BFS(0:2), distance
      DOUBLE PRECISION array_axis_BFS(0:2),array_norm_BFS(0:2)
      DOUBLE PRECISION arrayTRR(0:2)
      DOUBLE PRECISION costheta_1,costheta_2
      DOUBLE PRECISION accn1,accn2,flux,LENGTH,DOT_NN
      DOUBLE PRECISION oneAU,solarI,mass,JASON_AREA,ENVISAT_AREA
! Main panel norms
      DOUBLE PRECISION pn_temp(0:2),pn1(0:2),pn2(0:2)
      DOUBLE PRECISION acc_pn1(0:2),acc_pn2(0:2)
      DOUBLE PRECISION E2J_area,K1,r
      PARAMETER(K1=0.069926811943510) !used in main panel normal calc =
! Value of one astronomical unit in Km
      PARAMETER(oneAU=149597870691.D0)
! Solar irradiance at one AU
      PARAMETER(solarI=1368.2D0)
! Nominal mass of ENVISAT spacecraft
      PARAMETER(mass=8040.8D0)
! Area of the solar panels
      PARAMETER(JASON_AREA=9.536D0)
!      PARAMETER(ENVISAT_AREA=35.0D0) !area of 1/2 total panel
      PARAMETER(ENVISAT_AREA=71.12D0)  !total area
!
! Calculate ENVISAT:JASON panel area ratio
      E2J_area=ENVISAT_AREA/JASON_AREA
!
! Create panel normals
      DO I=0,2
        pn_temp(I)=array_norm_BFS(I)+(K1*array_axis_BFS(I))
      END DO
      CALL VNORM(pn_temp,pn1,r)
      DO I=0,2
        pn_temp(I)=array_norm_BFS(I)-(K1*array_axis_BFS(I))
      END DO
      CALL VNORM(pn_temp,pn2,r)
!
! Calculate the Sun-Probe distance
!     temp(0)=pos(0)-sun(0)
!     temp(1)=pos(1)-sun(1)
!     temp(2)=pos(2)-sun(2)
!     distance=LENGTH(temp)
!
! Calculate the probe sun angle for panel section 1
!     costheta_1=DOT_NN(psun_BFS,pn1)
      costheta_1 = psun_BFS(0)*pn1(0) + psun_BFS(1)*pn1(1) + &
     &             psun_BFS(2)*pn1(2)
! Calculate the probe sun angle for panel section 2
!     costheta_2=DOT_NN(psun_BFS,pn2)
      costheta_2 = psun_BFS(0)*pn2(0) + psun_BFS(1)*pn2(1) + &
     &             psun_BFS(2)*pn2(2)
! Calculate acceleration due to panel segments with normal pn1
      flux=(oneAU**2/distance**2)*solarI*costheta_1
      accn1=-4.23387D-13*flux**2-1.44701D-10*flux+2.98084*10D-10
      acc_pn1(0)=E2J_area*accn1*pn1(0)
      acc_pn1(1)=E2J_area*accn1*pn1(1)
      acc_pn1(2)=E2J_area*accn1*pn1(2)
!
! Calculate acceleration due to panel segments with normal pn2
      flux=(oneAU**2/distance**2)*solarI*costheta_2
      accn2=-4.23387D-13*flux**2-1.44701D-10*flux+2.98084*10D-10
      acc_pn2(0)=E2J_area*accn2*pn2(0)
      acc_pn2(1)=E2J_area*accn2*pn2(1)
      acc_pn2(2)=E2J_area*accn2*pn2(2)
!
! Calculate the TRR acceleration vector
      arrayTRR(0)=acc_pn1(0)+acc_pn2(0)
      arrayTRR(1)=acc_pn1(1)+acc_pn2(1)
      arrayTRR(2)=acc_pn1(2)+acc_pn2(2)
!
! Scale by JASON:ENVISAT panel area ratio and mass of spacecraft
      arrayTRR(0)=arrayTRR(0)/mass
      arrayTRR(1)=arrayTRR(1)/mass
      arrayTRR(2)=arrayTRR(2)/mass

      RETURN
      END
