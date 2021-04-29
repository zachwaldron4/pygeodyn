!      PROGRAM READ_OROGRAPHY
      SUBROUTINE READ_OROGRAPHY(OROGRAPHY_ARRAY)
!*******************************************************************************
! FUNCTION: Reads the file orography_ell.txt and creates a (91,145) array
!           containing mean grid heights.
!
!
! I/O PARAMETERS:
!
!   NAME               I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------             ---  ---   --------------------------------------------
!   OROGRAPHY_ARRAY    I/O   A    Array containg mean grid heights
!
!
!*******************************************************************************
      IMPLICIT NONE

      INTEGER :: DELTA_LAT,LAT_P,LAT_M
      INTEGER :: N, I, J , K, ORO_COL
      DOUBLE PRECISION :: BOUND_MM, BOUND_PP, BOUND_MP, BOUND_PM
      DOUBLE PRECISION :: LAT_PLUS,LAT_MINUS,LON_PLUS,LON_MINUS
      DOUBLE PRECISION :: DELTA_LON,HEIGHT_INTERP
      DOUBLE PRECISION :: HEADER(6),LON_IN,LAT_IN,LON_MOD
      DOUBLE PRECISION :: DELTA_LAT_INV,DELTA_LON_INV,t,u,t1,u1
      LOGICAL :: L_COMPUTE
      INTEGER :: BUFFER(15,10),RECORD(10),IRET,LONBOUNDLOC(2)
      INTEGER :: LATBOUNDLOC(2)
      INTEGER :: OROGRAPHY_ARRAY(91,145)


!*******************************************************************************
! START EXECUTABLE CODE
!*******************************************************************************
!DEBUG
!      LAT_IN = 90.0
!      LON_IN = 00.0

!      write(6,*) "READ_ORO: LAT_IN", LAT_IN
!      write(6,*) "READ_ORO: LON_IN", LON_IN
!DEBUG



!*******************************************
! I want to read the orography file and
! create orography array.
!*******************************************

      OPEN (unit=410, file="orography_ell.txt", status='old')

      READ (410,1111) HEADER(1),HEADER(2),HEADER(3),HEADER(4),  &
     &               HEADER(5),HEADER(6)
 1111 format(3x,f6.2,3x,f6.2,4x,f6.2,4x,f6.2,4x,f6.2,4x,f6.2)

!      write(6,*) "HEADER: ",HEADER(1:6)

      DELTA_LAT = HEADER(5)
      DELTA_LON = HEADER(6)

      DELTA_LON_INV = 1.D0/DELTA_LON
      DELTA_LAT_INV = 1.D0/DELTA_LAT

!      write(6,*) "ROE: DELTA:", DELTA_LON_INV,DELTA_LAT_INV


!*******************************
! READ DATA RECORDS
!*******************************
      ROW : DO J = 1,91   !NUMBER OF LATITUDE BANDS

      READ_BAND : DO N = 1,15

         READ(410,'(10(i8))' ) RECORD

!         write(6,*) "RECORD is :", RECORD

         DO I = 1,10

            BUFFER(N,I) = RECORD(I)

         ENDDO

      ENDDO READ_BAND


!      write(6,*) "BUFFER:", BUFFER(1:15,1:10)


!TRANSFER LATITUDE BAND TO OROGRAPHY ARRAY
       ORO_COL = 1

       DO N = 1,15

          DO I = 1,10

           IF (ORO_COL > SIZE(OROGRAPHY_ARRAY,2)) EXIT

           OROGRAPHY_ARRAY(J,ORO_COL) = BUFFER(N,I)

           ORO_COL = ORO_COL + 1

          ENDDO

       ENDDO
      ENDDO ROW

!      write(6,*) " ORO_ARRAY is :", OROGRAPHY_ARRAY(91,1:150)


       close(410)

!      CALL DET_HEIGHTS(LAT_IN,LON_IN,OROGRAPHY_ARRAY, &
!     &                 DELTA_LAT,DELTA_LON,DELTA_LAT_INV,&
!     &                 DELTA_LON_INV,HEIGHT_INTERP)

!
!      write(6,*) "HEIGHT_INTERP is:", HEIGHT_INTERP


       RETURN
      END SUBROUTINE
