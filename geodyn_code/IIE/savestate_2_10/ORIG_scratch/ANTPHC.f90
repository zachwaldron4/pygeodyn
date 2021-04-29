!$ANTPHC
      SUBROUTINE ANTPHC(ENV,UHAT,PHASEC,NDIMA,NDIMZ,PHAHDR,PHC,NM,WORK)
!********1*********2*********3*********4*********5*********6*********7**
! ANTPHC                                         PGMR - D. ROWLANDS
!
!
! FUNCTION:  COMPUTE THE ANTENNA MAP CORRECTION AT A GROUND STATION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ENV      I    A    THE LOCAL EAST NORTH VERTICAL VECTORS
!   UHAT     I    A    UHAT EARTH FIXED UNIT VECTORS FROM
!                      STATION TO SATELLITE
!   PHASEC   I    A    ANTENNA CORRECTION ARRAY. CORRECTIONS SHOULD
!                      BE ADDED      TO   COMPUTED    OR
!                         SUBTRACTED FROM OBSERVED
!   PHAHDR   I    A    HEADER RECORD OF PHASE CORRECTION TABLE
!   NDIMA    I    S    AZIMUTH DIEMENSION OF THE PHASE TABLE
!   NDIMZ    I    S    ZENITH DIMENSION   OF THE PHASE TABLE
!   PHAHDR   I    A    THE HEADER RECORD FOR THIS TABLE
!   PHC      O    A    CORRECTION FROM PHASEC
!   NM       I    S    NUMBER OF MEASUREMENTS
!   WORK     I    A    SCRATCH SPACE
!
!
! COMMENTS: SEE VOLUME 3 (ANTPHC CARD) FOR STRUCTURE OF PHASEC ARRAY
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION ENV(3,3),UHAT(NM,3)
      DIMENSION PHASEC(NDIMA,NDIMZ),PHC(NM),PHAHDR(6)
      DIMENSION WORK(NM,3)

!------------------------------------------------------------------------

!      write(6,'(A,3(1x,I5))')'antphc: NDIMA, NDIMZ, NM ', &
!                                      NDIMA, NDIMZ, NM
!      write(6,'(A,6(1x,F6.1))')'antphc: PHAHDR ', PHAHDR(1:6)

!     IF(PHAHDR(3).GT.0.D0) THEN
!       ZENMIN=PHAHDR(2)
!       ZENMAX=PHAHDR(2)+(INT(PHAHDR(1))-1.D0)*PHAHDR(3)
!     ELSE
!       ZENMAX=PHAHDR(2)
!       ZENMIN=PHAHDR(2)+(INT(PHAHDR(1))-1.D0)*PHAHDR(3)
!     ENDIF

      DO 10 I=1,NM

      WORK(I,2)=UHAT(I,1)*ENV(1,3)+UHAT(I,2)*ENV(2,3)+UHAT(I,3)*ENV(3,3)

      ! WORK(I,1) ZENITH ANGLE IN DEGREES

      WORK(I,1)=ACOS(WORK(I,2))/DEGRAD

!      if( i == 1 )write(6,*) 'antphc: i, station zenith ', i, work(i,1)

      WORK(I,2)=UHAT(I,1)*ENV(1,2)+UHAT(I,2)*ENV(2,2)+UHAT(I,3)*ENV(3,2)
      WORK(I,3)=UHAT(I,1)*ENV(1,1)+UHAT(I,2)*ENV(2,1)+UHAT(I,3)*ENV(3,1)

!      if( i == 1 )write(6,*) 'antphc: i, sta az=', &
!          i, dmod( 3600.d0+datan2(work(i,3),work(i,2)) / degrad, 360.d0)

      WORK(I,3)=3600.D0+ATAN2(WORK(I,2),WORK(I,3))/DEGRAD

! WORK(I,2) AZIMUTH ANGLE IN DEGREES
! THE STATION AZIMUTH IS COUNTED FROM NORTH AND CLOCKWISE

      WORK(I,2)=MOD((450.D0-MOD(WORK(I,3),360.D0)),360.D0)

!      if( i == 1 )then
!          write(6,'(A,1x,I2,2(1x,F10.5))') &
!                'antphc: i, zenith, az =', i, work(i,1), work(i,2)
!      endif

   10 END DO


      DO 20 II=1,NM
! ZERO PHC FIRST
        PHC(II) = 0.D0
        J1=WORK(II,1)/PHAHDR(3)
        J1=J1+1
        IF(J1.GE.NDIMZ) J1=NDIMZ-1
        J2=J1+1

        !if( ii == 1 )write(6,*) 'antphc: ii, J1, J2 ', ii, J1, J2

        ZEN1=PHAHDR(2)+DBLE(J1-1)*PHAHDR(3)

        ! choose 1-d or 2-d interpolation
        IF(NDIMA.GT.1) THEN
          I1=WORK(II,2)/PHAHDR(6)
          I1=I1+1
          I2=I1+1
          IF(I2.GT.NDIMA) I2=I2-NDIMA

          ! TWO DIMENSIONAL INTERPOLATION

          PHV1 = PHASEC(I1,J1) + (WORK(II,1)-ZEN1) *  &
                    ( PHASEC(I1,J2)-PHASEC(I1,J1) ) / PHAHDR(3)
          PHV2 = PHASEC(I2,J1) + (WORK(II,1)-ZEN1) *  &
                    ( PHASEC(I2,J2)-PHASEC(I2,J1) ) / PHAHDR(3)

          AZ1 = PHAHDR(5)+DBLE(I1-1)*PHAHDR(6)

          PHC(II) = PHV1 + (WORK(II,2)-AZ1) * (PHV2-PHV1) / PHAHDR(6)

        ELSE

          ! ONE DIMENSIONAL INTERPOLATION

          PHC(II) = PHASEC(1,J1) + (WORK(II,1)-ZEN1) *  &
                       ( PHASEC(1,J2)-PHASEC(1,J1) ) / PHAHDR(3)

        ENDIF !  NDIMA.GT.1

!        if( ii == 1 )then
!            write(6,'(A,1x,I2,1x,F20.10)') &
!                  'antphc: ii, phc(ii) ', ii, phc(ii)
!        endif ! ii == 1

   20 END DO

      RETURN

      END
