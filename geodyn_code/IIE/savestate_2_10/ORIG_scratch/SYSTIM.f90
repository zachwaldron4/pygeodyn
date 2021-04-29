!$SYSTIM
      SUBROUTINE SYSTIM
!********1*********2*********3*********4*********5*********6*********7**
! SYSTIM           MM/DD/YY            0000.0    PGMR - EDDY
!
!
! FUNCTION                    CALLS SYSTEM ROUTINE FOR CURRENT DATE AND
!                             TIME INFORMATION AND LOADS DATE AND TIME
!                             INTO A SINGLE FLOATING POINT WORD
!
! COMMENTS
!
!
!            OUTPUT IS YYMMDDHHMMSS. IN COMMON G2SINF
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!      CHARACTER*1 LINTWD,LTIME        ! f77
!      CHARACTER*8 CTIME             ! f77
      CHARACTER*10 C1L, C2L, C3L        ! f90
!
      INCLUDE 'COMMON_DECL.inc'
      COMMON/G2EINF/G2EVER,G2EDAT,G2ERTM
      COMMON/G2SINF/G2SVER,G2SDAT,G2SRTM,G2SCOM,TDFVER,TDFRTM,SETCDS,   &
     &              GPCKSM,GPDEG ,GPORD ,XG2SIN
      COMMON/UNITS/IUNT11,IUNT12,IUNT13,IUNT19,IUNT30,IUNT71,IUNT72,    &
     &             IUNT73,IUNT05,IUNT14,IUNT65,IUNT88,IUNT21,IUNT22,    &
     &             IUNT23,IUNT24,IUNT25,IUNT26
!
!      DIMENSION LTIME(8)            ! f77
!      DIMENSION LINTWD(4)           ! f77
      DIMENSION IDATES(8)                ! f90
!      EQUIVALENCE (LINTWD(1),INTWRD)    ! f77
!      EQUIVALENCE (CTIME,LTIME(1))      ! f77
!
      DATA NUMOFF/48/
      DATA C1D6/1.0D6/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!      INTWRD=0                     ! f77
!      JDATE =0                     ! f77
!      ITIMEA =0                     ! f77
!
! GET DATE AND TIME
!
!      call idate( month, iday, iyear )                      ! f77
!      call time( ctime )                                    ! f77
      call date_and_time( C1L,C2L,C3L, Idates )                  ! f90
      IYEAR = Idates(1)                                        ! f90
      MONTH = Idates(2)                                         ! f90
      IDAY  = Idates(3)                                          ! f90
!
!
!   CONVERT DATE TO MMDDYY
      JDATE = IYEAR * 10000 + MONTH * 100 + IDAY
!
! CONVERT  'HH:MM:SS' TO HHMMSS
!
!      ITIMEA =0                                              ! f77
!      INTWRD=0                                              ! f77
!      DO 1500 I=1,8                                         ! f77
!         LINTWD(4)=LTIME(I)                                 ! f77
!         NXTDIG=INTWRD-NUMOFF                               ! f77
!         IF(NXTDIG.LT.0 .OR. NXTDIG.GT.9) GO TO 1500        ! f77
!         ITIMEA =ITIMEA*10+NXTDIG                             ! f77
! 1500 CONTINUE                                              ! f77
!
         ITIMEA =IDATES(5)*10000 + IDATES(6)*100 + IDATES(7)   !f90
!
!
      XDATE =DBLE(JDATE)
      XTIME =DBLE(ITIMEA)
      G2ERTM=XDATE*C1D6+XTIME
      RETURN
      END
