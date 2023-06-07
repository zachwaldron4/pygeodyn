!$MODDRIA
      SUBROUTINE MODDRIA(TDNRM1,TDNRM2,TDNRM3,                          &
     &                   NFACE,VEL, XDOT, YDOT, ZDOT,                   &
     &                   BWAREA,MEANMOL,TEMP,NOXA, CD,                  &
     &                   DXDD,PXDDT,JARADJ,IMAPAR,NEQN,                 &
     &                   ISHFLG,SPLADG,B0,B,SCAREA,RHO,TC1,TOTARE,kin_2,&
     &                   CDprime, Ldrag_ScalingFactor, SPEED_RATIO)
!                                                                       &
!      - note from zach, fortran wants the dyn.arrays (TDNRM1,TDNRM2,TDNRM3)
!                       to be first and on their own lines.  It not, they get
!                        passed as zeros...
!********************************************************************
! ROUTINE NAME: MODDRIA       January/2022       written by Zach Waldron and Vishal Ray     
!
!
!  FUNCTION - 1) Calculates physical drag coefficient based on the
!                Modified Diffuse Reflection Incomplete Accommodation (DRIA) model
!             2) sums up the drag-coefficient for individual plates to calculate total
!             3) Output Cd considers the varying cross-sectional area. No need to multiply.
!             4) Ref : Drag Coefficient Model Using the 
!                      Cercignani–Lampis–Lord Gas–Surface Interaction Model, 10.2514/1.A32677
                    ! SENTMAN MODEL
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   TDNRM1   A      I    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (X COMP.)
!   TDNRM2   A      I    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (Y COMP.)
!   TDNRM3   A      I    SPACECRAFT FLAT PLATE NORMAL VECTORS IN TRUE OF
!                        DATE SYSTEM (Z COMP.)
!   NFACE    S      I    NUMBER OF FLAT PANELS 
!   VEL      S      I    SPACECRAFT VELOCITY WITH RESPECT TO THE ATMOSPH
!   XDOT     S      I    SPACECRAFT X-VELOCITY (RELATIVE TO ATMOSPHERE) MAGNITUDE
!   YDOT     S      I    SPACECRAFT Y-VELOCITY (RELATIVE TO ATMOSPHERE) MAGNITUDE
!   ZDOT     S      I    SPACECRAFT Z-VELOCITY (RELATIVE TO ATMOSPHERE) MAGNITUDE
!   BWAREA   A      I    FLATE PLATE AREAS
!   TW       A      I    SATELLITE PLATE TEMPERATURES
!   MS       A      I    MOLAR MASS FOR EACH PANEL (g/mol)
!   ALPHA    S      I    ENERGY ACCOMMODATION COEFFICIENT
!   MEANMOL  S      I    MEAN MOLAR MASS OF THE ATMOSPHERE
!   TEMP     S      I    AMBIENT TEMPERATURE of the atmopshere
!   NOXA     S      I    NUMBER DENSITY OF ATOMIC OXYGEN (m^-3)
!   KL       S      I    LANGMUIR PARAMETER
!   FRACOX   S      I    FRACTION OF SURFACE COVERED BY ATOMIC OXYGEN
!   NEQN     I      S    NUMBER OF FORCE MODEL PARAMETERS
!   ISHFLG   I      S    SELF SHADOWING INDICATOR                                      
!   SPLADG   I      S    the interpolated cross-section or ratio which is calculated in EXTATT.f                          
!   SCAREA   S      I    SPACECRAFT CROSS SECTIONAL AREA AS ENTERED ON THE SATPAR CARD
!   DXDD     A      O    TRUE OF INTEGRATION ACCELERATION DUE TO DRAG
!   PXDDT    A      O    PARTIAL OF PLATE ACCELERATION WRT PLATE AREA
!   JARADJ   S      I    POINTER TO AREA STORAGE LOCATION IN PXDDT
!   IMAPAR   A      I    MAPPING POINTER FROM UNADJUSTED TO ADJUSTED
!   RHO      I      S    DENSITY OF ATMOSPHERE AS GIVEN BY model of choice (KG/M3)
!   B0       I      S    .5*AREA/MASS
!   B        I      S    B0*CD    (**note** this is the CD as input on DRAG CARD or determined by GEODYN)
!   CD      I/O     O    DRAG-COEFFICIENT   (only the drag coefficent **note** this is the physically calc. CD)
!   TC1     I/O     S    TC1=(SCAREA/SCMASS)/(2*VEL*CD) -- (**note** replace the GEODYN CD with Physically calculated CD)
!   TOTARE  I/O     S    TOTAL SPACRAFT AREA AS CALCUALTED BY THE VARIABLE AREA (FROM PANELS)                             

! *************************************************************************
!
!

!
!    To check that this was all implemented correctly we need to visualize the following:
!                         
!                       - CD*Area of satellite (force contribution-- density at the highest CD*Area are weighted highest)
!                       - angle of attack/sideslip
!                       - Relative velocity
!                       - Speed Ratio



      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      
      ! NFACE DIMS
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION BWAREA(NFACE)
      !DIMENSION MS(NFACE)
      DIMENSION DXDD(3)
      DIMENSION PXDDT(NEQN,3),PART(3)
      DIMENSION IMAPAR(NFACE)
      DIMENSION ACCEL(3)
      DIMENSION SPLADG(1)

      ! 3 DIMS
      REAL(8), DIMENSION(3) ::  XVEL

      !     DEFINE CONSTANTS 
      PARAMETER (R  = 8314.46D0)       !% universal gas constant ; Units: g⋅m2⋅s−2⋅K−1⋅mol−1
      PARAMETER (KB = 1.38064852D-23)  !% Boltzmann constant     ; Units: m2 kg s-2 K-1
      PARAMETER (PI = 4*ATAN(1.D0))
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,THREE/3.0D0/
      DATA HALF/0.5D0/,FOUR/4.0D0/
      DATA ICNT/0/

      ! USER INPUTS (eventually will become user inputs)
      !PARAMETER (TW     = 300.0D0 )  ! temperature of panel
      !PARAMETER (ALPHA  = 1.0D0 )    ! accomodation coefficient
      !PARAMETER (KL     = 0.0D0 )    ! LANGMUIR PARAMETER
      !PARAMETER (FRACOX = 0.90D0 )   !

      !!!! MAKE SURE THAT THE EVERYTHING IS THE CORRECT DATATYPE:
      real(8) :: MEANMOL
      real(8) :: TEMP
      real(8) :: NOXA
      real(8) :: XDOT
      real(8) :: VEL
      real(8) :: YDOT
      real(8) :: ZDOT
      real(8) :: FRAC
      real(8) :: CTHETA
      real(8) :: MRPAN
      real(8) :: ALPHA_S
      real(8) :: CDS
      real(8) :: VR
      real(8) :: CDADS
      
      !!!! PARAMETER INPUTS FROM FILE
      real(8), dimension(5) :: array_params
      real(8), dimension(NFACE) :: MS
      real(8) :: TW
      real(8) :: ALPHA 
      real(8) :: KL
      real(8) :: FRACOX
      CHARACTER(len=255) :: PATH_IO_GEODYN

      
! ********************************* 
!  BEGIN EXECUTABLE CODE
! *********************************

     ! INITIALIZE VARIABLES
      TOTARE = ZERO
      JCOUNT = JARADJ-1
      ICNT = ICNT+1
      MS   = 26.980D0   ! aluminum atomic weight
      MS(13) = 60.08D0
      MS(14) = 60.08D0     ! THIS NEEDS TO BE CHANGED!
!      !!!!   SET THE CD OPTIONS FROM INPUTS:
      if(ICNT.eq.1) then
          !!! Read the path to the options file from am environment variable
          CALL get_environment_variable("PATH_IO_GEODYN", PATH_IO_GEODYN)      
          !     
          WRITE(6,*) ' [MODDRIA.f90] - Reading Parameters from file'
          open (121,                                                 &
                    & file=trim(PATH_IO_GEODYN)//'/cd_params.txt',   &
                    &       status='old')
          do i=1,5   !#### Loop through the param file and save the values to an array.
             read(121,*) array_params(i)
          end do
          close (121)
!          
          MS     = array_params(1)    ! makes the NFACE array with same values for each index
          TW     = array_params(2)
          ALPHA  = array_params(3)
          KL     = array_params(4)
          FRACOX = array_params(5)
          MS(13) = 60.08D0
          MS(14) = 60.08D0

          
          WRITE(6,*) ' [MODDRIA.f90] - ALPHA  ',  ALPHA

          WRITE(6,*) ' [MODDRIA.f90] - MS     ',  MS
          WRITE(6,*) ' [MODDRIA.f90] - TW     ',  TW
          WRITE(6,*) ' [MODDRIA.f90] - KL     ',  KL
          WRITE(6,*) ' [MODDRIA.f90] - FRACOX ',  FRACOX

!          WRITE(6,*) ' [MODDRIA.f90] - array_params ',  array_params
    
      endif

      if(kin_2.eq.1) WRITE(6,*) ' [MODDRIA.f90] - MS     ',  MS


    ! REMOVE SPACECRAFT AREA AS DEFINED ON THE SATPAR CARD FROM APGMR
      b_coeff = B0/SCAREA
      if(kin_2.eq.1) WRITE(6,*) '     [MODDRIA.f90] - B/B0 = CD = ', B/B0
      !WRITE(6,*) '     [MODDRIA.f90] - B/B0 = CD = ', B/B0

      TC1 = TC1/(SCAREA*(B/B0))

      !CDprime =  (TC1*(2.0D0*SCMASS)/(SCAREA*C3)  )!  B/B0! TC1
      !WRITE(6,*) '     [MODDRIA.f90] - CDprime = ', CDprime

      ! FORM THE UNIT VELOCITY VECTOR
      XVEL(1) = XDOT/VEL
      XVEL(2) = YDOT/VEL
      XVEL(3) = ZDOT/VEL

    !CALCULATE INPUTS TO DRAG-COEFFICIENT MODEL
      RI    = R/MEANMOL
      S_ratio  = VEL/SQRT(2.0D0*RI*TEMP)      !% molecular speed ratio 
      
    ! COMPUTE FRACTION OF SURFACE COVERED BY ATOMIC OXYGEN
    !     IF NON-PHYSICAL VALUE SPECIFIED BY USER, CALCULATE HERE
    !     SESAM
      IF (FRACOX.LT.0) THEN
         PO   = NOXA*KB*TEMP             !% partial pressure of oxygen (n*kb*T)
         FRAC = (KL*PO)/(1.0D0+KL*PO)
      ELSE
         FRAC = FRACOX
      ENDIF
      
      
         
    ! CALCULATE DRAG-COEFFICIENT
      CDS   = ZERO       
      CDADS = ZERO       !% adsorbed surface
    ! ZERO OUT ACCELERATION ARRAY
      DXDD(1) = ZERO
      DXDD(2) = ZERO
      DXDD(3) = ZERO

 
      ! LOOP THROUGH EACH PLATE,
      DO 100 I=1,NFACE
        ! INITIALIZE PARTIAL POINTERS
        IF(IMAPAR(I).GT.0) JCOUNT=JCOUNT+1



           if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] TDNRM1(I)', TDNRM1(I)
           if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] TDNRM1(I)', TDNRM1(I)
           if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] TDNRM1(I)', TDNRM1(I)
           if(kin_2.eq.1) WRITE(6,*) ' [DRAG.f90] BWAREA(I)', BWAREA(I)


          ! FOR EACH PLATE, 
          !     FORM THE DOT PRODUCT OF PLATE NORMAL VECTOR 
          !     AND SATELLITE VELOCITY VECTOR
          CTHETA=(TDNRM1(I) * XVEL(1)   +  &      !  dot product of panel normal 
              &   TDNRM2(I) * XVEL(2)   +  &      !    and relative velocity vector
              &   TDNRM3(I) * XVEL(3)   )       
              
          ! MASS FRACTION OF PANELS
          MRPAN = MEANMOL/MS(I)   ! % mass fraction of panel

          ! VARIABLES IN SENTMAN'S MODEL
          G = 1.0D0/(2.0D0*S_ratio*S_ratio)          
          P = EXP(-(CTHETA**2)*(S_ratio**2))/S_ratio
          Q = 1.0D0+G
          Z = 1.0D0+ERF(CTHETA*S_ratio)
          

          ! ACCOMMODATION COEFFICIENT USING GOODMAN'S FORMULA
              !       According to Goodman’s model(Goodman, 1977), the value 
              !       of alpha in the absence of atomic oxygen should be 
              !       dictated by the surface properties of the satellite. 
              !
              ! MRPAN  :  Ratio of mass of free stream particle
              !           to that of surface material,
              ! Ks     :  (3.60D0) can lie between 2.4 and 3.6 
              !           depending on the shape of the satellite 
              !           with the choice generally left to the user.
              ! CTHETA :  Fractional coverage ofatomic oxygen (theta)
              !           as a function of partial pressure of atomic oxygen
          ALPHA_S    = (3.60D0*MRPAN*CTHETA)/(1.0D0+MRPAN)**2   
          !ALPHA_eff =  (1- 0)
          
          !(3.60D0*MRPAN*CTHETA)/(1.0D0+MRPAN)**2   

          ! INCOMPLETE ACCOMMODATION DRAG-COEFFICIENT
          VR_term1=(4.0D0*RI*TW/(VEL**2)) - 1.0D0
          VR_term2=0.50D0*(1+ALPHA_S*(VR_term1))
          VR = VEL*SQRT(VR_term2)

          CDS = CDS + (  P/SQRT(PI) +                             &
              &          CTHETA*Q*Z +                             &
              &          CTHETA/2.0D0*VR/VEL*(CTHETA*SQRT(PI)*Z + P)  &
              &        )*BWAREA(I)

          ! COMPLETE ACCOMMODATION DRAG-COEFFICIENT 
          VR_term1=(4.0D0*RI*TW/(VEL**2)) - 1.0D0
          VR_term2=0.50D0*(1+ALPHA*(VR_term1))
          VR = VEL*SQRT(VR_term2)

          CDADS = CDADS + (P/SQRT(PI) +                             &
              &           CTHETA*Q*Z  +                             &
              &           CTHETA/2.0D0*VR/VEL*(CTHETA*SQRT(PI)*Z + P)  &
              &        )*BWAREA(I)
          !!!!! -------------- END DRIA PORTION -----------------------------------


      IF(ISHFLG.EQ.0) THEN
     ! SELF SHADOWING NOT APPLIED
     ! DETERMINE IF THIS PLATE IS ILLUMINATED
        IF(CTHETA .LE. ZERO) GOTO 100
        !if(ICNT.eq.1) WRITE(6,*) ' [modria.f90] - 1'
        PLAREA = BWAREA(I)*CTHETA
      ELSE IF(ISHFLG.EQ.1) THEN
     ! CROSS-SECTION
        IF(SPLADG(I).LE.ZERO) GOTO 100
        !if(ICNT.eq.1) WRITE(6,*) ' [modria.f90] - 2'
        PLAREA = SPLADG(I)
      ELSE
     ! RATIO
        IF(SPLADG(I).LE.ZERO) GOTO 100
        !if(ICNT.eq.1) WRITE(6,*) ' [modria.f90] - 3'
        PLAREA = BWAREA(I)*SPLADG(I)
      ENDIF
     ! SUM TOTAL ILLUMINATED SPACECRAFT CROSS-SECTIONAL AREA
      TOTARE = TOTARE + PLAREA

          !if(ICNT.eq.1) WRITE(6,*) ' [MODDRIA.f90] - CDS ', CDS
          !if(ICNT.eq.1) WRITE(6,*) ' [MODDRIA.f90] - CDADS ', CDADS

  100 END DO

      
      !     TOTAL DRAG-COEFFICIENT
      CD = FRAC*CDADS + (1.0D0-FRAC)*CDS      
      
      
      ! COMPUTE DRAG ACCELERATION ON THIS PLATE IN
      ! X,Y,AND Z DIRECTION OF THE TRUE OF DATE SYSTEM
        TERM1 = -CD                !  CD contains the physical_CD*total area (in flow dir)
        TERM2 = b_coeff*VEL*RHO    ! complete the drag equation
          
        ! Needs to also include the Scaling factor for CD 
        if(Ldrag_ScalingFactor) then
            TERM3 = CDprime
        else
            TERM3=1.D0
        endif
        
        ACCEL(1) = TERM1*XDOT*TERM2*TERM3
        ACCEL(2) = TERM1*YDOT*TERM2*TERM3
        ACCEL(3) = TERM1*ZDOT*TERM2*TERM3
      !
      ! ADD ACCELERATIONS TO TOTAL
        DXDD(1) = DXDD(1)+ACCEL(1)
        DXDD(2) = DXDD(2)+ACCEL(2)
        DXDD(3) = DXDD(3)+ACCEL(3)
      !
      ! COMPUTE PARTIAL OF ACCELERATION WRT AREA, ROTATE TO TOR,
      ! AND ADD THE PARTIALS TO THOSE
      ! PREVIOUSLY COMPUTED FOR THE OTHER NON-CONSERVATIVE FORCES
            IF(IMAPAR(I).GT.0) THEN
               if(kin_2.eq.1) WRITE(6,*) '     [MODDRIA.f90] - PARTIALS in MODDRIA '
               PART(1) = ACCEL(1)/BWAREA(I)
               PART(2) = ACCEL(2)/BWAREA(I)
               PART(3) = ACCEL(3)/BWAREA(I)
               PXDDT(JCOUNT,1) = PXDDT(JCOUNT,1)+PART(1)
               PXDDT(JCOUNT,2) = PXDDT(JCOUNT,2)+PART(2)
               PXDDT(JCOUNT,3) = PXDDT(JCOUNT,3)+PART(3)
            ENDIF

      TC1 = TC1*CD
      CD = CD/TOTARE
      
      SPEED_RATIO = S_ratio
      
      if(kin_2.eq.1) WRITE(6,*) '     [MODDRIA.f90] ACCELERATION DUE TO DRAG UPDATED USING DRIA'
      if(kin_2.eq.1) WRITE(6,*) '     [MODDRIA.f90]  - DXDD(1)  ', DXDD(1)
      if(kin_2.eq.1) WRITE(6,*) '     [MODDRIA.f90]  - DXDD(2)  ', DXDD(2)
      if(kin_2.eq.1) WRITE(6,*) '     [MODDRIA.f90]  - DXDD(3)  ', DXDD(3)
      if(kin_2.eq.1) WRITE(6,*) '     [MODDRIA.f90]  - TOTARE   ', TOTARE
      if(kin_2.eq.1) WRITE(6,*) '     [MODDRIA.f90]  - CD       ', CD

            


      RETURN
      END
      
      
      
    

  
 