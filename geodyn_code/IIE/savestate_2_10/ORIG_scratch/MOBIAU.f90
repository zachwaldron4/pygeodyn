!$MOBIAU
      SUBROUTINE MOBIAU(RM,VPMOBE,OBMTAU,AUTMOB)
!********1*********2*********3*********4*********5*********6*********7**
! MOBIAU          97/03/27            0000.0    PGMR - S. LUO
!
!  FUNCTION:  CREATE A MATRIX TO TRANSFORM THE REFERENCE FRAME BETWEEN
!             MARS MEAN ORBIT AND MARS CENTERED IAU
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   RM       I    A    DIRECTION OF MARS VERNAL EQUINOX
!   VPMOBE   I    A    POLE OF MARS VERNAL EQUINOX
!   OBMTAU   O    A    MATRICS FROM MARS MEAN ORBIT TO MARS IAU
!   AUTMOB   O    A    MATRICS FROM MARS IAU TO MARS MEAN ORBIT
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!     COMMON/CMOAEJ/OBMTAU(3,3),AUTMOB(3,3)
      DIMENSION RM(3), VPMOBE(3),OBMTAU(3,3),AUTMOB(3,3)
      DIMENSION AA(3,3),RMJ2(3),PMOBJ2(3),ANV(3)
      DIMENSION A(3,3),B(3,3),C(3,3),BA(3,3),CBA(3,3),CCM(3,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
   15 CONTINUE
!
      EE0=23.4392911D0*DEGRAD
      CALL ROTAT(-EE0,AA,1)
!
! ROTAT RM(3) to RMJ2(3) and ROTATE VPMOBE(3) to PMOBJ2(3)
! note: RM(3) and VPMOBE(3) in Earth mean ecliptic system of J2000
!       RMJ2(3) and PMOBJ2(3) are in Earth EQUATOR J2000.
!
      CALL MULTI(AA,RM,RMJ2,3,3,1)
      CALL MULTI(AA,VPMOBE,PMOBJ2,3,3,1)
!
!  CONVERT VPMOBE(3) to AMPOB (right ascension)and BMPOB (declination)of
!      MARS North pole of orbit in J2000 of Earth
!
      CSB=SQRT(PMOBJ2(1)*PMOBJ2(1)+PMOBJ2(2)*PMOBJ2(2))
      SNB=PMOBJ2(3)
      TGA=PMOBJ2(2)/PMOBJ2(1)
      TGB=SNB/CSB
!
      AMPOB=ATAN(TGA)
      BMPOB=ATAN(TGB)
      AMNOBJ=PI/2+AMPOB
      DINOBJ=PI/2-BMPOB
!
!  Get the angle between ascending node of Mars orbit on J2000 earth equ
!  plane and its vernal equinox.
!
!     Ascending Node vector ANV(3)=(COS(AMNOBJ),SIN(AMNOBJ),0)
!
      ANV(1)=COS(AMNOBJ)
      ANV(2)=SIN(AMNOBJ)
      ANV(3)=0.D0
!     The angle = DACOS(RMJ2 * ANV )
      CSRMAN=RMJ2(1)*ANV(1)+RMJ2(2)*ANV(2)+RMJ2(3)*ANV(3)
      AREUI=ACOS(CSRMAN)
!
! get OBMTAU and AUTMOB
!
      CALL ROTAT(-AMNOBJ,A,3)
      CALL ROTAT(-DINOBJ,B,1)
      CALL ROTAT(AREUI,C,3)
      CALL MULTI(B,A,BA,3,3,3)
      CALL MULTI(C,BA,CBA,3,3,3)
      DO 35 I=1,3
       DO 30 J=1,3
       OBMTAU(I,J)=CBA(I,J)
   30 END DO
   35 END DO
!
      CALL ROTAT(AMNOBJ,A,3)
      CALL ROTAT(DINOBJ,B,1)
      CALL ROTAT(-AREUI,C,3)
!     CALL MULTI(B,A,BA,3,3,3)
!     CALL MULTI(C,BA,CBA,3,3,3)
      CALL MULTI(B,C,BA,3,3,3)
      CALL MULTI(A,BA,CBA,3,3,3)
      DO 45 I=1,3
       DO 40 J=1,3
       AUTMOB(I,J)=CBA(I,J)
   40 END DO
   45 END DO
!
! CHECK the Matrix   OBMTAU*AUTMOB=1
!
      CALL MULTI(OBMTAU,AUTMOB,CCM,3,3,3)
!     PRINT *,'MOB2IAU ** CCM*= ', CCM
!
! Deburg..
      AMPOB=AMPOB/DEGRAD
      BMPOB=BMPOB/DEGRAD
      AMNOBJ=AMNOBJ/DEGRAD
      DINOBJ=DINOBJ/DEGRAD
      AREUI=AREUI/DEGRAD
!     PRINT *,'MOB2IAU**AREUI, AMPOB,BMPOB,AMNOBJ,DINOBJ=*',
!    *                  AREUI, AMPOB,BMPOB,AMNOBJ,DINOBJ
      RETURN
      END
