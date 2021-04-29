!$GPSPHC
      SUBROUTINE GPSPHC(NM,XSM,UI,NDIMRA,PHASEC,NDIMA,NDIMZ,PHAHDR,     &
     &                  L1ST,DR)
!********1*********2*********3*********4*********5*********6*********7**
! GPSPHC                                         PGMR - C. DENG
!
!
! FUNCTION:  COMPUTE THE ANTENNA MAP CORRECTION FOR A GPS SATELLITE
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I    S    NUMBER OF OBSERVATIONS
!   XSM      I    A    SATELLITE INERTIAL VECTOR IN TOR
!   UI       I    A    UNIT VECTOR FROM RECEIVER TO GPS SATELLITE IN TOR
!   NDIMRA   I    S    DIMENSION OF UI ARRAY
!   PHASEC   I    A    ANTENNA CORRECTION ARRAY. CORRECTIONS SHOULD
!                      BE ADDED TO COMPUTED OR SUBTRACTED FROM OBSERVED
!   NDIMA    I    S    AZIMUTH DIEMENSION OF THE PHASE TABLE
!   NDIMZ    I    S    ZENIS DIMENSION OF THE PHASE TABLE
!   PHAHDR   I    A    THE HEADER RECORD FOR THIS TABLE
!   L1ST     I    S    INDICATE WHICH SIGN SHOULD BE USED FOR CORRECTION
!   DR      I/O   A    TO BE ADDED WITH THE PHASE CENTER VARIATION
!
!
! COMMENTS: SEE VOLUME 3 (ANTPHC CARD) FOR STRUCTURE OF PHASEC ARRAY
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG
      DIMENSION XSM(MINTIM,3),UI(NDIMRA,3)
      DIMENSION PHASEC(NDIMA,NDIMZ), PHAHDR(6)
      DIMENSION DR(NM)
!
      DO I=1,NM
        R1=XSM(I,1)*XSM(I,1)+XSM(I,2)*XSM(I,2)+XSM(I,3)*XSM(I,3)
        R1=SQRT(R1)
        R2=UI(I,1)*UI(I,1)+UI(I,2)*UI(I,2)+UI(I,3)*UI(I,3)
        R2=SQRT(R2)
        DOT=XSM(I,1)*UI(I,1)+XSM(I,2)*UI(I,2)+XSM(I,3)*UI(I,3)
        DOT=ABS(DOT/(R1*R2))
! CALCULATE THE ZENIS ANGLE
        ZEN=ACOS(DOT)/DEGRAD
          !print*, 'ddd zenis for GPS Satellite ', zen
        J1=(ZEN-PHAHDR(2))/PHAHDR(3)
        J1=J1+1
        IF(J1.GE.NDIMZ) J1=NDIMZ-1
        J2=J1+1
        ZEN1=PHAHDR(2)+(DBLE(J1)-1.D0)*PHAHDR(3)
! ONLY DO ONE-DIMENSIONAL INTERPOLATION FOR GPS SATELLITES
        PHC=PHASEC(1,J1)+(ZEN-ZEN1)*(PHASEC(1,J2)-PHASEC(1,J1))/PHAHDR(3)
           !print*,'R1: ',r1
           !print*,'R2: ',r2
           !print*,'dot: ',dot
           !print*,'ddd L1ST: ',l1st
           !stop
        IF(L1ST) THEN
            DR(I)=DR(I)+PHC
        ELSE
            DR(I)=DR(I)-PHC
        ENDIF
 100    CONTINUE
      ENDDO
      END
