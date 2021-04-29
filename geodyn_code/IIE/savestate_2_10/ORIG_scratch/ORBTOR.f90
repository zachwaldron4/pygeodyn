!$ORBTOR
      SUBROUTINE ORBTOR(XSAT,VSAT,SPF,NSAT)
!*******************************************************************
!  ROUTINE NAME:   ORBTOR   DATE:               PGMR: D. PAVLIS
!
!   FUNCTION - TO COMPUTE ROTATION MATRIX FROM SATELLITE ALONG TRACK
!              CROSS TRACK AND RADIAL TO CARTESIAN TOR
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   XSAT     I      A    TOR POSITION COORDINATES OF S/C
!   VSAT     I      A    TOR VELOCITY COORDINATES OF S/C
!   SPF      O      A    ROTATION MATRIX FROM SATELLITE ALONG TRACK,
!                        CROSS TRACK, RADIAL TO TOR
!   NSAT     I      S    NUMBER OF SATELLITES INTEGRATED IN THIS CALL
!
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CRMI/RMI(9)

      DIMENSION XSAT(3,NSAT),VSAT(3,NSAT)
      DIMENSION XTOD(3),YTOD(3),ZTOD(3)
      DIMENSION SPF(3,3,NSAT)
      DIMENSION ROT(3,3)
!
!
! COMPUTE THE ROTATION FROM THE SPF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF EARTH
!         YTOD = TOD VECTOR NORMAL TO  ORBIT PLANE (Y = Z X VSAT)
!         XTOD = TOD VECTOR TANGENT TO ORBIT PLANE (X = Y X Z)
!
      DO 100 I=1,NSAT
! ZAXIS ROTATION
      ZMAG = SQRT(XSAT(1,I)**2+XSAT(2,I)**2+XSAT(3,I)**2)
      ZTOD(1) =-XSAT(1,I)/ZMAG
      ZTOD(2) =-XSAT(2,I)/ZMAG
      ZTOD(3) =-XSAT(3,I)/ZMAG
      SPF(1,3,I)= ZTOD(1)
      SPF(2,3,I)= ZTOD(2)
      SPF(3,3,I)= ZTOD(3)
!
! YAXIS ROTATION
      YTOD(1) = ZTOD(2)*VSAT(3,I)-ZTOD(3)*VSAT(2,I)
      YTOD(2) = ZTOD(3)*VSAT(1,I)-ZTOD(1)*VSAT(3,I)
      YTOD(3) = ZTOD(1)*VSAT(2,I)-ZTOD(2)*VSAT(1,I)
      YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
      YTOD(1) = YTOD(1)/YMAG
      YTOD(2) = YTOD(2)/YMAG
      YTOD(3) = YTOD(3)/YMAG
      SPF(1,2,I)= YTOD(1)
      SPF(2,2,I)= YTOD(2)
      SPF(3,2,I)= YTOD(3)
!
! XAXIS ROTATION
      XTOD(1) = YTOD(2)*ZTOD(3)-YTOD(3)*ZTOD(2)
      XTOD(2) = YTOD(3)*ZTOD(1)-YTOD(1)*ZTOD(3)
      XTOD(3) = YTOD(1)*ZTOD(2)-YTOD(2)*ZTOD(1)
      XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
      XTOD(1) = XTOD(1)/XMAG
      XTOD(2) = XTOD(2)/XMAG
      XTOD(3) = XTOD(3)/XMAG
      SPF(1,1,I)= XTOD(1)
      SPF(2,1,I)= XTOD(2)
      SPF(3,1,I)= XTOD(3)

  100 END DO

!     ROTATE FROM TOD TO TOR

      DO 200 I=1,NSAT

      ROT(1,1)=SPF(1,1,I)*RMI(1) + SPF(2,1,I)*RMI(2) + SPF(3,1,I)*RMI(3)
      ROT(2,1)=SPF(1,1,I)*RMI(4) + SPF(2,1,I)*RMI(5) + SPF(3,1,I)*RMI(6)
      ROT(3,1)=SPF(1,1,I)*RMI(7) + SPF(2,1,I)*RMI(8) + SPF(3,1,I)*RMI(9)
      ROT(1,2)=SPF(1,2,I)*RMI(1) + SPF(2,2,I)*RMI(2) + SPF(3,2,I)*RMI(3)
      ROT(2,2)=SPF(1,2,I)*RMI(4) + SPF(2,2,I)*RMI(5) + SPF(3,2,I)*RMI(6)
      ROT(3,2)=SPF(1,2,I)*RMI(7) + SPF(2,2,I)*RMI(8) + SPF(3,2,I)*RMI(9)
      ROT(1,3)=SPF(1,3,I)*RMI(1) + SPF(2,3,I)*RMI(2) + SPF(3,3,I)*RMI(3)
      ROT(2,3)=SPF(1,3,I)*RMI(4) + SPF(2,3,I)*RMI(5) + SPF(3,3,I)*RMI(6)
      ROT(3,3)=SPF(1,3,I)*RMI(7) + SPF(2,3,I)*RMI(8) + SPF(3,3,I)*RMI(9)

      SPF(1,1,I)=ROT(1,1)
      SPF(2,1,I)=ROT(2,1)
      SPF(3,1,I)=ROT(3,1)
      SPF(1,2,I)=ROT(1,2)
      SPF(2,2,I)=ROT(2,2)
      SPF(3,2,I)=ROT(3,2)
      SPF(1,3,I)=ROT(1,3)
      SPF(2,3,I)=ROT(2,3)
      SPF(3,3,I)=ROT(3,3)

  200 END DO



      RETURN
      END
