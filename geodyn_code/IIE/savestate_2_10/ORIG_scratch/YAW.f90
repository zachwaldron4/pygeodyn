!$YAW
      SUBROUTINE YAW(LRAMP,LRDOWN,LRUP,BETAP,PRVBET,                    &
     &               SOMEGA,PRVOMG,YAWLIM,YAWANG,LFIX0,LFIX90,LSIN)
!*******************************************************************
!  ROUTINE NAME:   YAW   DATE: 08/10/90      PGMR: A. MARSHALL
!
!   FUNCTION - TO COMPUTE ROTATION ABOUT TOPEX YAW AXIS WHERE ZERO
!              YAW INDICATES THAT THE BODY-FIXED X-AXIS IS ALIGNED
!              THE VELOCITY VECTOR.
!
!  I/O PARAMETERS:
!
!   NAME    A/S    I/O   DESCRIPTION OF I/O PARAMETERS IN ARGUMENT LIST
!   -----  ------ -----  -----------------------------------------------
!   LRAMP    L      O    RAMP UP OR DOWN IS IN PROGRESS
!   LRDOWN   L      O    RAMP FROM SINUSOIDAL TO FIXED YAW AT PROPER ORB
!   LRUP     L      O    RAMP FROM FIXED TO SINUSOIDAL YAW AT PROPER ORB
!   LFIX0    L      O    SPACECRAFT IS IN FIXED YAW = 0
!   LFIX90   L      O    SPACECRAFT IS IN FIXED YAW = 90
!   LSIN     L      O    SPACECRAFT IS IN SINUSOIDAL YAW
!   BETAP    S      I    BETAPRIME ANGLE(SUN INCLINATION TO ORBIT PLANE)
!   PRVBET   S      I    LAST CALCULATED BETAPRIME ANGLE
!   SOMEGA   S      I    SPACECRAFT ORBIT ANGLE
!   PRVOMG   S      I    LAST CALCUALTED SPACECRAFT ORBIT ANGLE
!   YAWLIM   A      I    YAW LIMITS (BETAPRIME BOUNDARY VALUES, DICTATIN
!                                    YAW ALGORITHM)
!                            1 = BOUNDARY FOR FIXED/SINUSOIDAL YAW AT LO
!                            2 = BOUNDARY FOR 180 DEG YAW FLIPS
!                            3 = BOUNDARY FOR FIXED/SINUSOIDAL YAW AT HI
!   YAWANG   S      O    YAW ANGLE (ANGLE BETWEEN BODY-FIXED XAXIS      !
!
! NOTES:
!         1) ASSUME SPACECRAFT FLIPS INSTANTANEOUSLY WHEN BETAP.LT.YAWLI
!         2) FIRST CALL TO THIS ROUTINE HAS INACCURATE BACK VALUES.  THE
!            INITIAL YAW ANGLE COMPUTATION MIGHT BE IN ERROR.
!
! REFERENCES:
!            FRIEDER, M., "TOPEX ATTITUDE COMMANDS IN OBC", FAIRCHILD
!            MEMO ACS-87-020, 20 OCTOBER 1987.
!
!            ZIMBELMAN, D., "FINAL VERSION OF TOPEX EULERC SUBROUTINE",
!            FAIRCHILD MEMO GNC:TOPEX:89229, 17 OCTOBER 1989.
!
!            ZIMBELMAN, D., "TOPEX REFERENCE ELLIPSOID AND CALCUALTION
!            OF THE LOCAL VERTICAL OFFSET ANGLE CORRECTION", FAIRCHILD
!            MEMO GNC:TOPEX:90-083A, 19 JUNE 1990.
!
!            PERRYGO, C., "TOPEX SATELLITE YAW MANUEVERS", FAIRCHILD
!            MEMO REF:986:SE:87-074, 11 NOVEMBER 1987.
!*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
!
      DIMENSION YAWLIM(3)
!
      DATA ZERO/0.0D0/,ONE/1.0D0/,TWO/2.0D0/
!
!********1*********2*********3*********4*********5*********6*********7**
! START OF EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
!
      PID2 = PI/TWO
! COMPUTE ABSOLUTE VALUES
      ABETAP = ABS(BETAP)
      APVBET = ABS(PRVBET)
!
! BETAPRIME VALUES OUTSIDE YAWLIM(1)
      IF(ABETAP.LT.YAWLIM(1)) GOTO 1000
!
! ....BETAPRIME ABOVE YAWLIM(3), FIXED + OR - 90 DEG YAW
        IF(ABETAP.GE.YAWLIM(3)) THEN
!          FIXED YAW
           IF(BETAP.GE.ZERO) THEN
              YAWANG = PID2
           ELSE
              YAWANG = -PID2
           ENDIF
        LFIX90=.TRUE.
        LFIX0 =.FALSE.
        LSIN  =.FALSE.
        LRAMP =.FALSE.
        RETURN
        ENDIF
!
!......BETAPRIME CROSSING YAWLIM(1) BOUNDARY, RAMP UP NOW POSSIBLE
       IF(APVBET.LT.YAWLIM(1)) THEN
!          FIXED YAW
           LRUP = .TRUE.
           IF(BETAP.GE.ZERO) THEN
              YAWANG = ZERO
           ELSE
              YAWANG = -PI
           ENDIF
        LFIX90=.FALSE.
        LFIX0 =.TRUE.
        LSIN  =.FALSE.
        LRAMP =.FALSE.
        RETURN
        ENDIF
!
       IF(.NOT.LRUP) THEN
!......BETAPRIME IN NOMINAL SINUSOIDAL YAW REGION
           IF(BETAP.GE.ZERO) THEN
              YAWANG = PID2 + (PID2-BETAP)*COS(SOMEGA)
           ELSE
              YAWANG =-PID2 - (PID2+BETAP)*COS(SOMEGA)
           ENDIF
        LFIX90=.FALSE.
        LFIX0 =.FALSE.
        LSIN  =.TRUE.
        LRAMP =.FALSE.
        RETURN
        ENDIF
!
!.......NEAR OR IN THE PROCESS OF RAMP UP, POSITIVE BETAP
        IF(BETAP.LT.ZERO) GOTO 500
           IF(SOMEGA.GE.PID2 .AND. SOMEGA.LE.PI) THEN
              IF(LRAMP) THEN
!................RAMPING UP
                 YAWANG =ABETAP*COS(SOMEGA)**2
                 RETURN
              ELSE
                 IF(PRVOMG.GE.PID2 .AND. PRVOMG.LE.PI) THEN
!...................POPPED UP IN RAMPING REGION, WAITING UNTIL NEXT REV
!...................FIXED YAW
                    YAWANG = ZERO
                    LRAMP =.FALSE.
                    LFIX90=.FALSE.
                    LFIX0 =.TRUE.
                    LSIN  =.FALSE.
                    RETURN
                 ELSE
!...................CROSS RAMPING BOUNDARY, BEGIN RAMP UP
                    LRAMP = .TRUE.
                    LFIX90=.FALSE.
                    LFIX0 =.FALSE.
                    LSIN  =.FALSE.
                    YAWANG = BETAP*COS(SOMEGA)**2
                    RETURN
                 ENDIF
              ENDIF
           ELSE
              IF(LRAMP) THEN
!................END RAMP UP, BEGIN SINUSOIDAL YAW
                    LRAMP =.FALSE.
                    LFIX90=.FALSE.
                    LFIX0 =.FALSE.
                    LSIN  =.TRUE.
                    LRUP  =.FALSE.
                 YAWANG = PID2 + (PID2-BETAP)*COS(SOMEGA)
                 RETURN
              ELSE
!................POPPED UP IN RAMPING REGION, WAITING UNTIL NEXT REV TO
!................FIXED YAW
                 YAWANG = ZERO
                  LRAMP =.FALSE.
                  LFIX90=.FALSE.
                  LFIX0 =.TRUE.
                  LSIN  =.FALSE.
                 RETURN
              ENDIF
           ENDIF
!      WRITE(6,*) ' FAILED IN YAW, NO CONDITIONS SATISFIED '
!      STOP
!
  500 CONTINUE
!
!.......NEAR OR IN THE PROCESS OF RAMP UP, NEGATIVE BETAP
           IF(SOMEGA.GE.-PID2 .AND. SOMEGA.LE.ZERO) THEN
              IF(LRAMP) THEN
!................RAMPING UP
                 YAWANG =ABETAP*COS(SOMEGA)**2 - PI
                 RETURN
              ELSE
                 IF(PRVOMG.GE.-PID2 .AND. PRVOMG.LE.ZERO) THEN
!...................POPPED UP IN RAMPING REGION, WAITING UNTIL NEXT REV
!...................FIXED YAW
                    LRAMP =.FALSE.
                    LFIX90=.FALSE.
                    LFIX0 =.TRUE.
                    LSIN  =.FALSE.
                    YAWANG = -PI
                    RETURN
                 ELSE
!...................CROSS RAMPING BOUNDARY, BEGIN RAMP UP
                    LRAMP = .TRUE.
                    LFIX90=.FALSE.
                    LFIX0 =.FALSE.
                    LSIN  =.FALSE.
                    YAWANG = BETAP*COS(SOMEGA)**2 - PI
                    RETURN
                 ENDIF
              ENDIF
           ELSE
              IF(LRAMP) THEN
!................END RAMP UP, BEGIN SINUSOIDAL YAW
                 LRAMP = .FALSE.
                 LRUP  = .FALSE.
                 LFIX90=.FALSE.
                 LFIX0 =.FALSE.
                 LSIN  =.TRUE.
                 YAWANG = -PID2 - (PID2+BETAP)*COS(SOMEGA)
                 RETURN
              ELSE
!................POPPED UP IN RAMPING REGION, WAITING UNTIL NEXT REV TO
!................FIXED YAW
                 YAWANG = -PI
                 LRAMP = .FALSE.
                 LFIX90=.FALSE.
                 LFIX0 =.TRUE.
                 LSIN  =.FALSE.
                 RETURN
              ENDIF
           ENDIF
!      WRITE(6,*) ' FAILED IN YAW, NO CONDITIONS SATISFIED '
!      STOP
!
!
!
 1000 CONTINUE
! BETAPRIME VALUES INSIDE YAWLIM(1)
! ....BETAPRIME INSIDE YAWLIM(2), FLIP
        IF(ABETAP.LT.YAWLIM(2)) THEN
!          FIXED YAW, FLIP
           IF(BETAP.GE.ZERO) THEN
              YAWANG = ZERO
           ELSE
              YAWANG = -PI
           ENDIF
        LRAMP = .FALSE.
        LFIX90=.FALSE.
        LFIX0 =.TRUE.
        LSIN  =.FALSE.
        RETURN
        ENDIF
!
!....BETAPRIME CROSSING RAMP DOWN BOUNDARY, REMAIN IN SINUSOIDAL YAW
       IF(APVBET .GE. YAWLIM(1)) THEN
           LRDOWN= .TRUE.
           IF(BETAP.GE.ZERO) THEN
              YAWANG = PID2 + (PID2-BETAP)*COS(SOMEGA)
           ELSE
              YAWANG =-PID2 - (PID2+BETAP)*COS(SOMEGA)
           ENDIF
       LRAMP = .FALSE.
       LFIX90=.FALSE.
       LFIX0 =.FALSE.
       LSIN  =.TRUE.
       RETURN
       ENDIF
!
!
       IF(.NOT.LRDOWN) THEN
!......BETAPRIME IN FIXED YAW REGION
           IF(BETAP.GE.ZERO) THEN
              YAWANG = ZERO
           ELSE
              YAWANG = -PI
           ENDIF
        LRAMP = .FALSE.
        LFIX90=.FALSE.
        LFIX0 =.TRUE.
        LSIN  =.FALSE.
        RETURN
        ENDIF
!
!.......NEAR OR IN THE PROCESS OF RAMP DOWN, POSITIVE BETAP
        IF(BETAP.LT.ZERO) GOTO 1500
           IF(SOMEGA.GE.-PI .AND. SOMEGA.LE.-PID2) THEN
              IF(LRAMP) THEN
!................RAMPING DOWN
                 YAWANG =ABETAP*COS(SOMEGA)**2
                 RETURN
              ELSE
                 IF(PRVOMG.GE.-PI .AND. PRVOMG.LE.-PID2) THEN
!...................POPPED UP IN RAMPING REGION, WAITING UNTIL NEXT REV
!...................SINUSOIDAL YAW
                    LRAMP = .FALSE.
                    LFIX90=.FALSE.
                    LFIX0 =.FALSE.
                    LSIN  =.TRUE.
                    YAWANG = PID2 + (PID2-BETAP)*COS(SOMEGA)
                    RETURN
                 ELSE
!...................CROSS RAMPING BOUNDARY, BEGIN RAMP DOWN
                    LRAMP = .TRUE.
                    LFIX90=.FALSE.
                    LFIX0 =.FALSE.
                    LSIN  =.FALSE.
                    YAWANG =ABETAP*COS(SOMEGA)**2
                    RETURN
                 ENDIF
              ENDIF
           ELSE
              IF(LRAMP) THEN
!................END RAMP DOWN, BEGIN FIXED YAW
                 LRAMP = .FALSE.
                 LRDOWN= .FALSE.
                 LFIX90=.FALSE.
                 LFIX0 =.TRUE.
                 LSIN  =.FALSE.
                 YAWANG = ZERO
                 RETURN
              ELSE
!................POPPED UP IN RAMPING REGION, WAITING UNTIL NEXT REV TO
!................SINUSOIDAL YAW
                 YAWANG = PID2 + (PID2-BETAP)*COS(SOMEGA)
                 LRAMP = .FALSE.
                 LFIX90=.FALSE.
                 LFIX0 =.FALSE.
                 LSIN  =.TRUE.
                 RETURN
              ENDIF
           ENDIF
!      WRITE(6,*) ' FAILED IN YAW, NO CONDITIONS SATISFIED '
!      STOP
!
 1500 CONTINUE
!
!.......NEAR OR IN THE PROCESS OF RAMP DOWN, NEGATIVE BETAP
           IF(SOMEGA.GE.ZERO .AND. SOMEGA.LE.PID2) THEN
              IF(LRAMP) THEN
!................RAMPING DOWN
                 YAWANG =ABETAP*COS(SOMEGA)**2 - PI
                 RETURN
              ELSE
                 IF(PRVOMG.GE.ZERO .AND. PRVOMG.LE.PID2) THEN
!...................POPPED UP IN RAMPING REGION, WAITING UNTIL NEXT REV
!...................SINUSOIDAL YAW
                    LRAMP = .FALSE.
                    LFIX90=.FALSE.
                    LFIX0 =.FALSE.
                    LSIN  =.TRUE.
                    YAWANG = -PID2 - (PID2+BETAP)*COS(SOMEGA)
                    RETURN
                 ELSE
!...................CROSS RAMPING BOUNDARY, BEGIN RAMP DOWN
                    LRAMP = .TRUE.
                    LFIX90=.FALSE.
                    LFIX0 =.FALSE.
                    LSIN  =.FALSE.
                    YAWANG =ABETAP*COS(SOMEGA)**2 - PI
                    RETURN
                 ENDIF
              ENDIF
           ELSE
              IF(LRAMP) THEN
!................END RAMP DOWN, BEGIN FIXED YAW
                 LRAMP = .FALSE.
                 LRDOWN= .FALSE.
                 LFIX90=.FALSE.
                 LFIX0 =.TRUE.
                 LSIN  =.FALSE.
                 YAWANG = -PI
                 RETURN
              ELSE
!................POPPED UP IN RAMPING REGION, WAITING UNTIL NEXT REV TO
!................SINUSOIDAL YAW
                 YAWANG = -PID2 - (PID2+BETAP)*COS(SOMEGA)
                 LRAMP = .FALSE.
                 LFIX90=.FALSE.
                 LFIX0 =.FALSE.
                 LSIN  =.TRUE.
                 RETURN
              ENDIF
           ENDIF
!      WRITE(6,*) ' FAILED IN YAW, NO CONDITIONS SATISFIED '
!      STOP
!
!C      RETURN
      END
