!$PLANPR
      SUBROUTINE PLANPR(AEI)
!********1*********2*********3*********4*********5*********6*********7**
! PLANPR           87/12/28            8801.0    PGMR - WEI XIA
!
! FUNCTION:  PRINT OUT ELEVATION ANGLE FROM ORBIT PLANE TO
!            TRACKING BODY. THIS ANGLE SHOWS WHAT PART OF
!            THE ORBIT IS VISIBLE FROM TRACKING BODY.
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AEI      I    A    KEPLER ELEMENTS OF SATELLITE
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CBARYC/CBODY(6),TBODY(6),SUNRF(6)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      DIMENSION DCT(3),UDCT(3),RCOM(3,3),RI(3,3),RLOM(3,3)
      DIMENSION RTEMP1(3),RTEMP2(3),UNEW(3)
      DIMENSION AEI(6)
      DATA ZERO/0.0D0/,ONE/1.0D0/,THOUS/1000.D0/,C360/360.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! GET ROTATION MATRICES
      COSCOM=COS(AEI(4)*DEGRAD)
      SINCOM=SIN(AEI(4)*DEGRAD)
      COSI=COS(AEI(3)*DEGRAD)
      SINI=SIN(AEI(3)*DEGRAD)
      COSLOM=COS(AEI(5)*DEGRAD)
      SINLOM=SIN(AEI(5)*DEGRAD)
      RCOM(1,1)=COSCOM
      RCOM(1,2)=SINCOM
      RCOM(1,3)=ZERO
      RCOM(2,1)=-SINCOM
      RCOM(2,2)=COSCOM
      RCOM(2,3)=ZERO
      RCOM(3,1)=ZERO
      RCOM(3,2)=ZERO
      RCOM(3,3)=ONE
      RI(1,1)=ONE
      RI(1,2)=ZERO
      RI(1,3)=ZERO
      RI(2,1)=ZERO
      RI(2,2)=COSI
      RI(2,3)=SINI
      RI(3,1)=ZERO
      RI(3,2)=-SINI
      RI(3,3)=COSI
      RLOM(1,1)=COSLOM
      RLOM(1,2)=SINLOM
      RLOM(1,3)=ZERO
      RLOM(2,1)=-SINLOM
      RLOM(2,2)=COSLOM
      RLOM(2,3)=ZERO
      RLOM(3,1)=ZERO
      RLOM(3,2)=ZERO
      RLOM(3,3)=ONE
!
!
      DO 2000 JPBODY=1,2
      IF(JPBODY.GT.1) GO TO 20
      DO 10 I=1,3
      DCT(I)=TBODY(I)
   10 END DO
      GO TO 40
   20 CONTINUE
      DO 30 I=1,3
      DCT(I)=SUNRF(I)
   30 END DO
   40 CONTINUE
!
!     ZERO OUT VECTORS
      DO 50 I=1,3
      RTEMP1(I)=ZERO
      RTEMP2(I)=ZERO
      UNEW(I)=ZERO
   50 END DO
!     CALCULATE THE UNIT VECTOR
!     NOTE: LITPRX MUST HAVE BEEN CALLED !!!!!!
      SUM=ZERO
      DO 100 I=1,3
        DCT(I)=DCT(I)-CBODY(I)
        SUM=SUM+DCT(I)*DCT(I)
  100 END DO
      DNORM=SQRT(SUM)
      DO 200 I=1,3
        UDCT(I)=DCT(I)/DNORM
  200 END DO
      DO 400 I=1,3
         DO 300 J=1,3
            RTEMP1(I)=RTEMP1(I)+RCOM(I,J)*UDCT(J)
  300    CONTINUE
  400 END DO
      DO 600 I=1,3
         DO 500 J=1,3
            RTEMP2(I)=RTEMP2(I)+RI(I,J)*RTEMP1(J)
  500    CONTINUE
  600 END DO
      DO 800 I=1,3
         DO 700 J=1,3
            UNEW(I)=UNEW(I)+RLOM(I,J)*RTEMP2(J)
  700    CONTINUE
  800 END DO
      DNORM2=UNEW(1)*UNEW(1)+UNEW(2)*UNEW(2)
      DNORM2=SQRT(DNORM2)
      DNORM=DNORM/THOUS
      SINAZ=UNEW(2)/DNORM2
      COSAZ=UNEW(1)/DNORM2
      AZ=ATAN2(SINAZ,COSAZ)/DEGRAD
      IF(AZ.LT.ZERO) AZ=AZ+C360
      AZ=C360-AZ
      ELEV=ATAN(UNEW(3)/DNORM2)/DEGRAD
      WRITE(IOUT6,10000)
      IF(JPBODY.EQ.1) WRITE(IOUT6,10001)
      IF(JPBODY.EQ.1) WRITE(IOUT9,40001)
      IF(JPBODY.EQ.2) WRITE(IOUT6,20001)
      IF(JPBODY.EQ.2) WRITE(IOUT9,50001)
      WRITE(IOUT6,30001) AZ,ELEV,DNORM
      WRITE(IOUT9,60001) AZ,ELEV,DNORM
 2000 END DO
      RETURN
10000 FORMAT('-')
10001 FORMAT(1X,' SPHERICAL COORDINATES OF TRACKING BODY AS',           &
     &          ' SEEN FROM ORBIT PLANE (X AXIS POINTING',              &
     &          ' TOWARDS PERIGEE) AT EPOCH:')
20001 FORMAT(1X,' SPHERICAL COORDINATES OF SUN AS',                     &
     &          ' SEEN FROM ORBIT PLANE (X AXIS POINTING TOWARDS',      &
     &          ' PERIGEE) AT EPOCH:')
30001 FORMAT(1X,' AZIMUTH =',F7.2,' (DEGREES). ELEVATION=',F7.2,        &
     &          ' (DEGREES). DISTANCE=',F16.0,' (KM)')
40001 FORMAT(1X,' SPHERICAL COORDINATES OF TRACKING BODY AS',           &
     &          ' SEEN FROM ORBIT PLANE',/,1X,' (X AXIS POINTING',      &
     &          ' TOWARDS PERIGEE) AT EPOCH:')
50001 FORMAT(1X,' SPHERICAL COORDINATES OF SUN AS',                     &
     &         ' SEEN FROM ORBIT PLANE',/,1X,' (X AXIS POINTING TOWARDS'&
     &          ,' PERIGEE) AT EPOCH:')
60001 FORMAT(1X,' AZIMUTH  =',F7.2,' (DEGREES).',/,1X,' ELEVATION=',F7.2&
     &          ,' (DEGREES).',/,1X,' DISTANCE =',F16.0,' (KM)')
      END
