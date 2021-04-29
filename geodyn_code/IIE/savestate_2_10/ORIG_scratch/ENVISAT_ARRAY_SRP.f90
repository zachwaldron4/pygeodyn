!$ENVISAT_ARRAY_SRP
      SUBROUTINE ENVISAT_ARRAY_SRP(sun,aa,n,np,acc_array)
      IMPLICIT NONE
      INTEGER J
      DOUBLE PRECISION sun(0:2),aa(0:2),n(0:2)
      DOUBLE PRECISION rnorm
      DOUBLE PRECISION np(0:2),acc_array(0:2)
!     spacecraft mass in KG
      DOUBLE PRECISION nom_mass
      DOUBLE PRECISION acc_temp(0:2)
      DOUBLE PRECISION M_PI,K1,K2,K3
      PARAMETER(nom_mass=8040.8D0) !nominal mass used to create SRP model
      PARAMETER(M_PI = 3.14159265358979D0)
      PARAMETER(K1=0.069926811943510D0) !used in main panel normal calc =
      PARAMETER(K2=0.938130912810886D0) !used to recover boom arm ECI_TOD
      PARAMETER(K3=0.346280797083253D0) !used to recover boom arm ECI_TOD
!     attitude rotation coefficients, nominally in degrees
!     CONSTANTS FOR ARRAY COMPONENTS IN SRP DETERMINATION
!     component areas
      DOUBLE PRECISION A1,A2,A3,A4,A5
!     thermo-optical properties
      DOUBLE PRECISION REFL1,SPEC1,REFL2,SPEC2
!     boom arm
      DOUBLE PRECISION radius,height,A_BOOM
!     boom normal vector and normal & shear forces
      DOUBLE PRECISION boom_n(0:2),boom_nn(0:2),BOOM_FN,BOOM_FS
      DOUBLE PRECISION COS_TH
!     set the constant values
      PARAMETER(A1=0.15258D0)
      PARAMETER(A2=0.125D0)
      PARAMETER(A3=1.080308D0)
      PARAMETER(A4=35.0D0)    !USE PANEL MATERIAL PROPERTIES
      PARAMETER(A5=0.05D0)
      PARAMETER(A_BOOM=0.18945D0)
      PARAMETER(REFL1=0.41D0) !MLI
      PARAMETER(SPEC1=0.5D0)
      PARAMETER(REFL2=0.25D0) !panel FACE - same as JASON!
      PARAMETER(SPEC2=0.85D0) !panel FACE - same as JASON!
      PARAMETER(radius=0.025D0)
      PARAMETER(height=3.789D0)
!     other array component normals
      DOUBLE PRECISION aa_neg(0:2),np_neg(0:2),boom_axis(0:2)
!     main panel normals
      DOUBLE PRECISION pn_temp(0:2),pn1(0:2),pn2(0:2),np_pn1(0:2)
      DOUBLE PRECISION E,c
!     //solar constant in W/m^2
      PARAMETER(E = 1368.0D0)
!     speed of light in vacuum
      PARAMETER(c = 299792458.0D0)
!     CALCULATE THE PANEL SRP ACCELERATIONS
!     create additional panel component normals
      DO J=0,2
        aa_neg(J)=-aa(J)
        np_neg(J)=-np(J)
      END DO
      DO J=0,2
        pn_temp(J)=n(J)+(K1*aa(J))
      END DO
      CALL VNORM(pn_temp,pn1,rnorm)
      DO J=0,2
        pn_temp(J)=n(J)-(K1*aa(J))
      END DO
      CALL VNORM(pn_temp,pn2,rnorm)
      CALL CROPDT(np,pn1,pn_temp)
      CALL VNORM(pn_temp,np_pn1,rnorm)
!     boom arm long axis (from array to bus) = shear vector
      DO J=0,2
        pn_temp(J)=(-K1*n(J))-(K2*aa(J))
      END DO
      CALL VNORM(pn_temp,boom_axis,rnorm)
!     boom arm normal vector
      CALL CROPDT(sun,boom_axis,pn_temp)
      CALL CROPDT(pn_temp,boom_axis,boom_n)
      CALL VNORM(boom_n,boom_nn,rnorm)
!     angle between sun and boom long axis
!     COS_TH=-DOT_N(sun,boom_axis)
      COS_TH = -sun(0)*boom_axis(0)-sun(1)*boom_axis(1)- &
     &          sun(2)*boom_axis(2)

!     calculate the separate flat panel component contributions
!     initialise accelerations
      acc_array(0)=0.0D0
      acc_array(1)=0.0D0
      acc_array(2)=0.0D0
      CALL PROCESS_COMPONENT(A1,REFL1,SPEC1,sun,np,acc_temp)
      DO J=0,2
        acc_array(J)=acc_array(J)+acc_temp(J)
      END DO
      CALL PROCESS_COMPONENT(A1,REFL1,SPEC1,sun,np_neg,acc_temp)
      DO J=0,2
        acc_array(J)=acc_array(J)+acc_temp(J)
      END DO
      CALL PROCESS_COMPONENT(A2,REFL1,SPEC1,sun,n,acc_temp)
      DO J=0,2
        acc_array(J)=acc_array(J)+acc_temp(J)
      END DO
      CALL PROCESS_COMPONENT(A3,REFL1,SPEC1,sun,aa,acc_temp)
      DO J=0,2
        acc_array(J)=acc_array(J)+acc_temp(J)
      END DO
      CALL PROCESS_COMPONENT(A3,REFL1,SPEC1,sun,aa_neg,acc_temp)
      DO J=0,2
        acc_array(J)=acc_array(J)+acc_temp(J)
      END DO
      CALL PROCESS_COMPONENT(A4,REFL2,SPEC2,sun,pn1,acc_temp)
      DO J=0,2
        acc_array(J)=acc_array(J)+acc_temp(J)
      END DO
      CALL PROCESS_COMPONENT(A4,REFL2,SPEC2,sun,pn2,acc_temp)
      DO J=0,2
        acc_array(J)=acc_array(J)+acc_temp(J)
      END DO
      CALL PROCESS_COMPONENT(A5,REFL1,SPEC1,sun,np_pn1,acc_temp)
      DO J=0,2
        acc_array(J)=acc_array(J)+acc_temp(J)
      END DO
!     finally, add the contribution from the boom arm
      BOOM_FN=(1.D0+(REFL1*SPEC1)/3.D0)*COS_TH
      BOOM_FN=BOOM_FN+((M_PI/6.D0)*REFL1*(1.D0-SPEC1))
      BOOM_FN=BOOM_FN*(-(A_BOOM*E/c)*COS_TH)
      BOOM_FS=-(A_BOOM*E/c)*COS_TH*SIN(ACOS(COS_TH))*(1.D0-REFL1*SPEC1)
      DO J=0,2
        acc_array(J)=acc_array(J)+BOOM_FN*boom_nn(J)/nom_mass+ &
     &  BOOM_FS*boom_axis(J)/nom_mass
      END DO

      RETURN
      END
