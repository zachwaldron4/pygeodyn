!$BWDRAG
      SUBROUTINE BWDRAG(TDNRM1,TDNRM2,TDNRM3,                           &
     &                  VEL,XDOT,YDOT,ZDOT,SCAREA,CD,BWAREA,            &
     &                  DXDD,PXDDT,JARADJ,IMAPAR,NFACE,NEQN,TOTARE,     &
     &                  ISHFLG,SPLADG)
!*******************************************************************
!  ROUTINE NAME:   BWDRAG   DATE: 08/31/90      PGMR: A. MARSHALL
!
!   FUNCTION - TO COMPUTE THE TOTAL DRAG ACCELERATION
!              FROM EACH FLATE PLATE IN THE MODEL AND
!              THE ASSOCIATED PARTIALS WITH RESPECT TO
!              PLATE AREA
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
!   VEL      S      I    SPACECRAFT VELOCITY WITH RESPECT TO THE ATMOSPH
!   XDOT     S      I    SPACECRAFT X-VELOCITY MAGNITUDE
!   YDOT     S      I    SPACECRAFT Y-VELOCITY MAGNITUDE
!   ZDOT     S      I    SPACECRAFT Z-VELOCITY MAGNITUDE
!   SCAREA   S      I    SPACELITE CROSS SECTIONAL AREA AS ENTERED ON TH
!                        SATPAR CARD
!   CD       S     I/O   CD=(SAT AREA/SAT MASS)/2*VELOCITY*
!                           (CD+CDDOT*TIME+CDDDOT*TIME**2)
!                        THE SPACECRAFT AREA WILL BE UPDATED TO REFLECT
!                        BOX-WING MODEL PROJECTED AREA
!   BWAREA   A      I    FLATE PLATE AREAS
!   DXDD     A      O    TRUE OF INTEGRATION ACCELERATION DUE TO DRAG
!   PXDDT    A      O    PARTIAL OF PLATE ACCELERATION WRT PLATE AREA
!   JARADJ   S      I    POINTER TO AREA STORAGE LOCATION IN PXDDT
!   IMAPAR   A      I    MAPPING POINTER FROM UNADJUSTED TO ADJUSTED
!                        PANAEL AREA
!   NFACE    S      I    NUMBER OF FLAT PLATES USED TO MODEL SATELLITE S
!
!************ BWSOLR LOCAL VARIABLE DEFFINITIONS************************
!   XVEL     A      W    SATELLITE VEL UNIT VECTOR IN TRUE OF DATE SYSTEM
!   CTHETA   S      W    COSINE OF ANGLE BETWEEN VELOCITY VECTOR
!                        AND PLATE NORMAL VECTOR
!   PLAREA   S      W    PROJECTED PLATE AREA
!   PART     A      W    BOX-WING PARAMETER PARTIALS IN TOD
!*******************************************************************
!
!
! NOTES:
!        1)  CD WILL BE MODIFIED TO REFLECT THE ACTUAL PROJECTED
!            SATELLITE AREA AS PREDICITED BY THE VARIABLE AREA MODEL
!            (BOX-WING).  THIS WILL EFFECT BOTH ACCELERATION AND PARTIAL
!            VALUES.
!
! REFERENCES:
!
!        "NON-GRAVITATIONAL PERTURBATIONS AND SATELLITE GEODESY,"
!         MILANI ET AL., 1987, PP. 98.
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CRMI/RMI(9)
!
      DIMENSION TDNRM1(NFACE),TDNRM2(NFACE),TDNRM3(NFACE)
      DIMENSION XVEL(3),BWAREA(NFACE),DXDD(3)
      DIMENSION PXDDT(NEQN,3),PART(3)
      DIMENSION IMAPAR(NFACE)
      DIMENSION ACCEL(3)
      DIMENSION SPLADG(1)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/,THREE/3.0D0/

      DATA ICNT/0/

!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**


      ICNT = ICNT+1

!
! INITIALIZE VARIABLES
      TOTARE = ZERO
      JCOUNT = JARADJ-1
!
! REMOVE SPACECRAFT AREA AS DEFINED ON THE SATPAR CARD FROM APGMR
      CD = CD/SCAREA
!
! ZERO OUT ACCELERATION ARRAY
      DXDD(1) = ZERO
      DXDD(2) = ZERO
      DXDD(3) = ZERO
! FORM UNIT VELOCITY VECTOR
      XVEL(1) = XDOT/VEL
      XVEL(2) = YDOT/VEL
      XVEL(3) = ZDOT/VEL
      
      !if(ICNT.eq.1)then
      !        WRITE(6,*) '     - TDNRM1    ', TDNRM1
      !        WRITE(6,*) '     - TDNRM2    ', TDNRM2
      !        WRITE(6,*) '     - TDNRM3    ', TDNRM3
      !        WRITE(6,*) '     - BWAREA    ', BWAREA
      !endif

!
! LOOP THROUGH EACH PLATE, COMPUTING ACCELERATION AND PARTIALS
      DO 100 I=1,NFACE
!
! INITIALIZE PARTIAL POINTERS
      IF(IMAPAR(I).GT.0) JCOUNT=JCOUNT+1
! FORM DOT PRODUCT OF PLATE NORMAL VECTOR AND SATELLITE VELOCITY VECTOR
      CTHETA=TDNRM1(I)*XVEL(1)+TDNRM2(I)*XVEL(2)+TDNRM3(I)*XVEL(3)



      !if(ICNT.eq.1)then
      !        WRITE(6,*) '  LOOP THRU FACES'
      !        WRITE(6,*) '     - I         ', I
      !        WRITE(6,*) '     - NFACE     ', NFACE
      !        WRITE(6,*) '     - CTHETA       ', CTHETA
      !        WRITE(6,*) '     - TDNRM1(I)    ', TDNRM1(I)
      !        WRITE(6,*) '     - TDNRM2(I)    ', TDNRM2(I)
      !        WRITE(6,*) '     - TDNRM3(I)    ', TDNRM3(I)
      !        WRITE(6,*) '     - BWAREA(I)    ', BWAREA(I)
      !endif

!
      IF(ISHFLG.EQ.0) THEN
! SELF SHADOWING NOT APPLIED
! DETERMINE IF THIS PLATE IS ILLUMINATED
        IF(CTHETA .LE. ZERO) GOTO 100
        !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - 1'
        PLAREA = BWAREA(I)*CTHETA
      ELSE IF(ISHFLG.EQ.1) THEN
! CROSS-SECTION
        IF(SPLADG(I).LE.ZERO) GOTO 100
        !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - 2'
        PLAREA = SPLADG(I)
      ELSE
! RATIO
        IF(SPLADG(I).LE.ZERO) GOTO 100
        !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - 3'
        PLAREA = BWAREA(I)*SPLADG(I)
      ENDIF
! SUM TOTAL ILLUMINATED SPACECRAFT CROSS-SECTIONAL AREA
      TOTARE = TOTARE + PLAREA
!
! COMPUTE SOLAR RADIATION PRESSURE ACCELERATION ON THIS PLATE IN
! X,Y,AND Z DIRECTION OF THE TRUE OF DATE SYSTEM
      TERM1 = -CD*PLAREA
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - TERM1  ', TERM1
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - CD     ', CD
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - PLAREA ', PLAREA
      ACCEL(1) = TERM1*XDOT
      ACCEL(2) = TERM1*YDOT
      ACCEL(3) = TERM1*ZDOT
      
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - ACCEL(1)  ', ACCEL(1)
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - ACCEL(2)  ', ACCEL(2)
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - ACCEL(3)  ', ACCEL(3)

!
! ADD ACCELERATIONS TO TOTAL
      DXDD(1) = DXDD(1)+ACCEL(1)
      DXDD(2) = DXDD(2)+ACCEL(2)
      DXDD(3) = DXDD(3)+ACCEL(3)
      
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - DXDD(1)  ', DXDD(1)
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - DXDD(2)  ', DXDD(2)
      !if(ICNT.eq.1) WRITE(6,*) ' [bwdrag.f90] - DXDD(3)  ', DXDD(3)

!
! COMPUTE PARTIAL OF ACCELERATION WRT AREA, ROTATE TO TOR,
! AND ADD THE PARTIALS TO THOSE
! PREVIOUSLY COMPUTED FOR THE OTHER NON-CONSERVATIVE FORCES
      IF(IMAPAR(I).GT.0) THEN
         PART(1) = ACCEL(1)/BWAREA(I)
         PART(2) = ACCEL(2)/BWAREA(I)
         PART(3) = ACCEL(3)/BWAREA(I)
         PXDDT(JCOUNT,1) = PXDDT(JCOUNT,1)+PART(1)
         PXDDT(JCOUNT,2) = PXDDT(JCOUNT,2)+PART(2)
         PXDDT(JCOUNT,3) = PXDDT(JCOUNT,3)+PART(3)
      ENDIF
!
  100 END DO
! UPDATE CD VALUE TO REFLECT TOTAL PROJECTED AREA
      CD = CD*TOTARE
      
      !if(ICNT.eq.1) WRITE(6,*) ' [BWDRAG.f90] - CD                ', CD
      !if(ICNT.eq.1) WRITE(6,*) ' [BWDRAG.f90] - CD/TOTARE                ', CD/TOTARE
      !if(ICNT.eq.1) WRITE(6,*) ' [BWDRAG.f90] - TOTARE                ', TOTARE

      
      RETURN
      END
