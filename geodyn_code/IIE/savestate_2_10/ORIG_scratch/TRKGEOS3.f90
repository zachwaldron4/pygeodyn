!$TRKGEOS3
      SUBROUTINE TRKGEOS3(MJDSN,FSECN,XSAT,VSAT,XSTA,RNGCOR,NM,UI,      &
     &                    DWRKDO,LOFFAJ)
!********1*********2*********3*********4*********5*********6*********7**
! TRKGEOS3         99/05/27            9907      PGMR - PATRICK DAHIROC
!
!
! FUNCTION:  COMPUTE CORRECTIONS TO RANGE DUE TO LOCATION OF
!            TRACKING POINT AND S/C C.G. NOT AT BODY-FIXED
!            ORIGIN FOR GEOS-3
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSN    I    S
!   FSECN    I    A
!   XSAT     I    A    SPACECRAFT POSITION IN TRUE OF DATE SYSTEM (M)
!   VSAT     I    A    SPACECRAFT VELOCITY IN TRUE OF DATE SYSTEM (M/S)
!   XSTA     I    A    STATION POSITION IN TRUE OF REFERENCE
!   RNGCOR   O    A    RANGE CORRECTION ARRAY (M)
!   NM       I    S    BLOCK SIZE
!
! COMMENTS: RANGE CORRECTION DATA FROM Optical and Infrared Transfer
!           Function of the GEOS 3 Retroreflector Array
!           Technical Report: RTOP 161-05-02
!           Author: David A. Arnold     October 1975
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CNIGLO/MINTIM,MSATG3,MEQNG ,MEQNG3,MSATG ,MSATOB,MSATA ,   &
     &              MSATA3,MSETA ,MINTVL,MSORDR,MSORDV,NMXORD,          &
     &       MCIPV ,MXBACK,MXI   ,MPXPF ,MAXAB ,MSETDG,MXSATD,          &
     &       MXDEGS,MXDRP ,MXDRPA,MXSRP ,MXSRPA,MXGAP ,MSATDR,          &
     &       MSATSR,MSATGA,MXDRPD,MXSRPD,MXGAPD,MXBCKP,MXTPMS,          &
     &       NSTAIN,NXCNIG

      DIMENSION XSAT(1000,3),VSAT(1000,3),XSTA(MINTIM,3)
      DIMENSION FSECN(NM),RNGCOR(NM),AA(1),XTOD(3),YTOD(3),ZTOD(3)
      DIMENSION RCTABLE(0:111),UH(3),URNG(3),BDFRRA(3),SPF(3,3)
      DIMENSION UI(NM,3)
      DIMENSION DWRKDO(NM,3)
      DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE :: TDXSTA
      DOUBLE PRECISION, PARAMETER :: PI_L = 3.14159265359
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! RANGE CORRECTION TABLE FOR GEOS-3
      RCTABLE(0)=1.3043
      RCTABLE(1)=1.3046
      RCTABLE(2)=1.3057
      RCTABLE(3)=1.3074
      RCTABLE(4)=1.3098
      RCTABLE(5)=1.3126
      RCTABLE(6)=1.3160
      RCTABLE(7)=1.3198
      RCTABLE(8)=1.3238
      RCTABLE(9)=1.3281
      RCTABLE(10)=1.3324
      RCTABLE(11)=1.3368
      RCTABLE(12)=1.3410
      RCTABLE(13)=1.3450
      RCTABLE(14)=1.3489
      RCTABLE(15)=1.3524
      RCTABLE(16)=1.3557
      RCTABLE(17)=1.3586
      RCTABLE(18)=1.3612
      RCTABLE(19)=1.3629
      RCTABLE(20)=1.3639
      RCTABLE(21)=1.3645
      RCTABLE(22)=1.3646
      RCTABLE(23)=1.3643
      RCTABLE(24)=1.3637
      RCTABLE(25)=1.3627
      RCTABLE(26)=1.3614
      RCTABLE(27)=1.3587
      RCTABLE(28)=1.3576
      RCTABLE(29)=1.3550
      RCTABLE(30)=1.3520
      RCTABLE(31)=1.3486
      RCTABLE(32)=1.3448
      RCTABLE(33)=1.3405
      RCTABLE(34)=1.3359
      RCTABLE(35)=1.3308
      RCTABLE(36)=1.3253
      RCTABLE(37)=1.3194
      RCTABLE(38)=1.3131
      RCTABLE(39)=1.3064
      RCTABLE(40)=1.2993
      RCTABLE(41)=1.2917
      RCTABLE(42)=1.2837
      RCTABLE(43)=1.2752
      RCTABLE(44)=1.2663
      RCTABLE(45)=1.2569
      RCTABLE(46)=1.2471
      RCTABLE(47)=1.2369
      RCTABLE(48)=1.2262
      RCTABLE(49)=1.2152
      RCTABLE(50)=1.2038
      RCTABLE(51)=1.1920
      RCTABLE(52)=1.1799
      RCTABLE(53)=1.1675
      RCTABLE(54)=1.1547
      RCTABLE(55)=1.1416
      RCTABLE(56)=1.1281
      RCTABLE(57)=1.1143
      RCTABLE(58)=1.1002
      RCTABLE(59)=1.0857
      RCTABLE(60)=1.0710
      RCTABLE(61)=1.0560
      RCTABLE(62)=1.0407
      RCTABLE(63)=1.0252
      RCTABLE(64)=1.0094
      RCTABLE(65)=0.9333
      RCTABLE(66)=0.9771
      RCTABLE(67)=0.9606
      RCTABLE(68)=0.9438
      RCTABLE(69)=0.9266
      RCTABLE(70)=0.9091
      RCTABLE(71)=0.8913
      RCTABLE(72)=0.8732
      RCTABLE(73)=0.8548
      RCTABLE(74)=0.8361
      RCTABLE(75)=0.8170
      RCTABLE(76)=0.7977
      RCTABLE(77)=0.7781
      RCTABLE(78)=0.7583
      RCTABLE(79)=0.7382
      RCTABLE(80)=0.7178
      RCTABLE(81)=0.6973
      RCTABLE(82)=0.6765
      RCTABLE(83)=0.6555
      RCTABLE(84)=0.6343
      RCTABLE(85)=0.6130
      RCTABLE(86)=0.5916
      RCTABLE(87)=0.5700
      RCTABLE(88)=0.5484
      RCTABLE(89)=0.5268
      RCTABLE(90)=0.5053
      RCTABLE(91)=0.4839
      RCTABLE(92)=0.4629
      RCTABLE(93)=0.4424
      RCTABLE(94)=0.4216
      RCTABLE(95)=0.4002
      RCTABLE(96)=0.3788
      RCTABLE(97)=0.3568
      RCTABLE(98)=0.3343
      RCTABLE(99)=0.3122
      RCTABLE(100)=0.2892
      RCTABLE(101)=0.2651
      RCTABLE(102)=0.2415
      RCTABLE(103)=0.2187
      RCTABLE(104)=0.1952
      RCTABLE(105)=0.1694
      RCTABLE(106)=0.1430
      RCTABLE(107)=0.1185
      RCTABLE(108)=0.0984
      RCTABLE(109)=0.0840
      RCTABLE(110)=0.0740
      RCTABLE(111)=0.0491
!
! BODY FIXED COORDINATE OF RETRO-REFLECTOR ARRAY CENTER
                           ! X
      BDFRRA(1)=0.0
                           ! Y
      BDFRRA(2)=0.0
                           ! Z
      BDFRRA(3)=-1.3358
!
! COPY XSTA TO TDXSTA AND CONVERT TDXSTA FROM TOR TO TOD
      ALLOCATE(TDXSTA(NM,3))
      IF(.NOT.ALLOCATED(TDXSTA)) THEN
         PRINT *,'PROBLEM ALLOCATING TDXSTA IN TRKGEOS3.f'
         STOP
      ENDIF
      DO II=1,NM
         DO JJ=1,3
            TDXSTA(II,JJ)=XSTA(II,JJ)
         ENDDO
      ENDDO
      CALL TDORTR(MJDSN,FSECN,TDXSTA,TDXSTA,                            &
     &     AA,NM,NM,.TRUE.,.FALSE.)

      DO I=1,NM
!
! COMPUTE THE ROTATION FROM THE SPF TO TOD FRAME WHERE
!         ZTOD = TOD VECTOR FROM S/C TO CENTER OF EARTH
!         YTOD = TOD VECTOR NORMAL TO  ORBIT PLANE (Y = Z X VSAT)
!         XTOD = TOD VECTOR TANGENT TO ORBIT PLANE (X = Y X Z)
!
! ZAXIS ROTATION
         ZMAG = SQRT(XSAT(I,1)**2+XSAT(I,2)**2+XSAT(I,3)**2)
         ZTOD(1) =-XSAT(I,1)/ZMAG
         ZTOD(2) =-XSAT(I,2)/ZMAG
         ZTOD(3) =-XSAT(I,3)/ZMAG
         SPF(1,3)= ZTOD(1)
         SPF(2,3)= ZTOD(2)
         SPF(3,3)= ZTOD(3)
!
! YAXIS ROTATION
         YTOD(1) = ZTOD(2)*VSAT(I,3)-ZTOD(3)*VSAT(I,2)
         YTOD(2) = ZTOD(3)*VSAT(I,1)-ZTOD(1)*VSAT(I,3)
         YTOD(3) = ZTOD(1)*VSAT(I,2)-ZTOD(2)*VSAT(I,1)
         YMAG = SQRT(YTOD(1)**2+YTOD(2)**2+YTOD(3)**2)
         YTOD(1) = YTOD(1)/YMAG
         YTOD(2) = YTOD(2)/YMAG
         YTOD(3) = YTOD(3)/YMAG
         SPF(1,2)= YTOD(1)
         SPF(2,2)= YTOD(2)
         SPF(3,2)= YTOD(3)
!
! XAXIS ROTATION
         XTOD(1) = YTOD(2)*ZTOD(3)-YTOD(3)*ZTOD(2)
         XTOD(2) = YTOD(3)*ZTOD(1)-YTOD(1)*ZTOD(3)
         XTOD(3) = YTOD(1)*ZTOD(2)-YTOD(2)*ZTOD(1)
         XMAG = SQRT(XTOD(1)**2+XTOD(2)**2+XTOD(3)**2)
         XTOD(1) = XTOD(1)/XMAG
         XTOD(2) = XTOD(2)/XMAG
         XTOD(3) = XTOD(3)/XMAG
         SPF(1,1)= XTOD(1)
         SPF(2,1)= XTOD(2)
         SPF(3,1)= XTOD(3)
!
! COMPUTE TRUE OF DATE COOR. RETROREFLECTOR ARRAY CENTER
         DO J=1,3
            BBB=0
            DO K=1,3
               BBB = BBB + SPF(J,K)*BDFRRA(K)
            ENDDO
            UH(J) = XSAT(I,J) - BBB
         ENDDO
!
! COMPUTE RANGE VECTOR FROM STATION TO SATELLITE
         DO J=1,3
            URNG(J) = UH(J) - TDXSTA(I,J)
         ENDDO
!
! COMPUTE UNIT HEIGHT VECTOR (UH) AND UNIT RANGE VECTOR (URNG)
         HMAG = SQRT(UH(1)**2 + UH(2)**2 + UH(3)**2)
         RNGMAG = SQRT(URNG(1)**2 + URNG(2)**2 + URNG(3)**2)
         DO J=1,3
            UH(J) = UH(J)/HMAG
            URNG(J) = URNG(J)/RNGMAG
         ENDDO
!
! COMPUTE UH.URNG AND ANGLE PHI
         DOT = URNG(1)*UH(1) + URNG(2)*UH(2) + URNG(3)*UH(3)
         PHI = ACOS(DOT)*180/PI_L
!
! DETERMINE THE INTERVAL WHERE PHI LIES IN THE RCTABLE ARRAY
         J1 = FLOOR(PHI)
         J2 = J1+1
!
! USE LAGRANGE INTERPOLATION FOR 2 POINTS TO DETERMINE THE RANGE CORRECT
! AT PHI
         IF((J1.LT.0).OR.(J1.GT.111)) THEN
            RNGCOR(I) = 0.0
         ELSE
            RNGCOR(I)=-(-(PHI-(J2))*RCTABLE(J1)+(PHI-J1)*RCTABLE(J2))
         ENDIF

!JTW, IS THIS CORRECT?

      IF(LOFFAJ) THEN
         DWRKDO(I,1)=UI(I,1)*SPF(1,1)                                   &
     &              +UI(I,2)*SPF(2,1)                                   &
     &              +UI(I,3)*SPF(3,1)
         DWRKDO(I,2)=UI(I,1)*SPF(1,2)                                   &
     &              +UI(I,2)*SPF(2,2)                                   &
     &              +UI(I,3)*SPF(3,2)
         DWRKDO(I,3)=UI(I,1)*SPF(1,3)                                   &
     &              +UI(I,2)*SPF(2,3)                                   &
     &              +UI(I,3)*SPF(3,3)
      ENDIF


      ENDDO
      DEALLOCATE(TDXSTA)
      END
