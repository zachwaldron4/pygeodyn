!$KPTRAN
      SUBROUTINE KPTRAN(XKP,INDEX,I324,IKPAP,AP,IERR)
!********1*********2*********3*********4*********5*********6*********7**
! KPTRAN           00/00/00            0000.0    PGMR - J. RIDGEWAY
!
! FUNCTION:
!  THIS ROUTINE HAS TWO FUNCTIONS:  1) TRANSFORM KP TO AP VALUES
!       IF NECESSARY, DEPENDING ON IKPAP FLAG, AND
!    2) FILL UP AP ARRAY WITH AP VALUES AS FOLLOWS, FOR THE MSIS
!       MODEL:  AP(1) = DAILY AP VALUE (USED ONLY IF I324 = 24).
!       (2) = 3HR CURRENT AP
!       (3) = 3HR AP FOR 3 HOURS BEFORE CURRENT TIME
!       (4) = "   "   "  6  "     "       "      "
!       (5) = "   "   "  9  "     "       "      "
!       (6) = AVERAGE OF EIGHT 3HR AP INDICIES FROM 12 TO 33 HOURS
!             PRIOR TO CURRENT TIME
!       (7) = AVERAGE OF EIGHT 3HR AP INDICIES FROM 36 TO 59 HOURS
!             PRIOR TO CURRENT TIME.
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XKP(*)   I    A    AN ARRAY CONTAINING EITHER KP OR AP VALUES,
!                      DEPENDING ON IKPAP FLAG.  IT CONTAINS EITHER
!                      DAILY OR 3-HOURLY VALUES, DEPENDING ON I324 FLAG.
!   INDEX    I    S    THE POSITION IN XKP WHICH CONTAINS CURRENT
!                      3-HOURLY OR DAILY VALUE.
!   I324     I    S    =3 MEANS 3-HOURLY VALUES, =24 MEANS DAILY KP/AP.
!   IKPAP         S    >0 MEANS XKP HOLDS KP, <0 MEANS XKP HOLDS AP.
!
!   AP(7)    O    A    AN ARRAY HOLDING AP VALUES, AS DESCRIBED IN 2)
!                      ABOVE. NOTE THAT AP(1) HAS MEANING ONLY IF I324
!                      EQUALS 24.
!   IERR     O    S    ERROR FLAG.  IERR = 0 => NO ERROR, >0 => ERROR.
!                      ROUTINE WILL RETURN IF AN ERROR OCCURS.
!
! COMMENTS:
!
! REFERENCES:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION XKP(*),AP(7), xkptmp(20)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      IERR = 0
!
!  ERROR CHECK on XKP.  If INDEX > 19, then there is no problem getting
!  averages back 2.5 days.  If INDEX < 14, then there do not exist
!  enough Kp values in XKP to calculate averages 33 - 59 hours prior
!  to present (i.e. error condition).  If 13 < INDEX < 20, then the
!  33 - 59 hour average will be calculated with INDEX-11 elements.
!
      IF( I324 .EQ. 3 ) THEN
!
        IF( INDEX .GE. 20 ) THEN
          ICNT = 19
        ELSEIF(INDEX .LT. 14) THEN
          WRITE(6,700) INDEX
  700     FORMAT(/,2X,'** ERROR IN KPTRAN ROUTINE.  NOT ENOUGH ',       &
     &    'ELEMENTS IN XKP TO DO AVERAGES: ',I9)
          IERR = 1
          GO TO 99
        ELSE
          ICNT = INDEX-1
        ENDIF
!
      ENDIF
!
!** TESTING.  TAKE OUT WHEN DONE TESTING
!     PRINT 801,INDEX,XKP(INDEX),XKP(INDEX-1),XKP(INDEX-ICNT)
! 801 FORMAT(2X,'INDEX = ',I5,2X,
!    @ 'XKP(INDEX,INDEX-1,INDEX-19): ',3(F10.3,1X))
!* END OF TESTING.
!******************
!
!  FILL UP AP ARRAY.
      IF( I324 .EQ. 24) THEN
!
!        24-HOURLY CASE HERE.
!
         IF( IKPAP .GE. 0 ) THEN
!             AP(1) = KPAP(XKP(INDEX))
              xkptmp(1) = xkp(index)
              call kpap(xkp(index), 0, xkptmp )
              AP(1) = xkptmp(1)
         ELSE
             AP(1) = XKP(INDEX)
         ENDIF
      ELSE
!
!       3-HOURLY CASE HERE.
!
        IF( IKPAP .GT. 0 ) THEN
!
!      ...CONVERT KP TO AP AND AVERAGE.
!
          icnt1 = icnt+1
          do 150 k=1,icnt1
             xkptmp(k) = xkp(index+1-k)
  150     continue
          call kpap( xkptmp, icnt1, xkptmp )
          AP(2) = xkptmp(1)
          AP(3) = xkptmp(2)
          AP(4) = xkptmp(3)
          AP(5) = xkptmp(4)
!
          SUM = 0.0D0
          DO 5 I=4,11
    5       SUM = SUM + xkptmp(I+1)
          AP(6) = SUM/8.0D0
!
          SUM = 0.0D0
          DO 7 I=12,ICNT
    7       SUM = SUM + xkptmp(I+1)
          AP(7) = SUM/DBLE(ICNT-11)
!
        ELSE
!
!      ...AVERAGE AP.
!
          AP(2) = XKP(INDEX)
          AP(3) = XKP(INDEX-1)
          AP(4) = XKP(INDEX-2)
          AP(5) = XKP(INDEX-3)
!
          SUM = 0.0D0
          DO 15 I=4,11
   15       SUM = SUM + XKP(INDEX-I)
          AP(6) = SUM/8.0D0
!
          SUM = 0.0D0
          DO 17 I=12,ICNT
   17       SUM = SUM + XKP(INDEX-I)
          AP(7) = SUM/DBLE(ICNT-11)
!
        ENDIF
!
      ENDIF
!****************
!** TEST PRINTOUT OF AP ARRAY.
!**   PRINT 802,(AP(J),J=1,7)
!C  802 FORMAT(2X,'=> AP(1-7): ',7(F5.1,1X))
!
   99 RETURN
      END
