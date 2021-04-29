      MODULE mssgfc_mod

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)

! 6 IS THE MAXIMUM NUMBER OF EXTERNAL INPUT GRIDS
!     INTEGER, PARAMETER :: r4b = selected_real_kind(p=6, r=37)
!     INTEGER, PARAMETER :: i2b =selected_int_kind(4)

      DOUBLE PRECISION  DPHI(6),DLAM(6),XLONW(6),XLATS(6)
      DIMENSION NNL(6),NEW(6)
      DIMENSION LGLOB(6)
      DATA NNL/6*0/,DPHI/6*0.D0/,DLAM/6*0.D0/,XLONW/6*0.D0/
      DATA XLATS/6*0.0D0/,NEW/6*0/
      DATA LMSS/.FALSE./
      DATA LDEM/.FALSE./
      DATA LGLOB/6*.FALSE./
! POSSIBILITY OF ALLOCATION FOR UP TO 6 GRID MODELS
!     REAL(kind=r4b), ALLOCATABLE :: GRID1(:,:)
!     REAL(kind=r4b), ALLOCATABLE :: GRID2(:,:)
!     REAL(kind=r4b), ALLOCATABLE :: GRID3(:,:)
!     REAL(kind=r4b), ALLOCATABLE :: GRID4(:,:)
!     REAL(kind=r4b), ALLOCATABLE :: GRID5(:,:)
!     REAL(kind=r4b), ALLOCATABLE :: GRID6(:,:)
      REAL , ALLOCATABLE :: GRID1(:,:)
      REAL , ALLOCATABLE :: GRID2(:,:)
      REAL , ALLOCATABLE :: GRID3(:,:)
      REAL , ALLOCATABLE :: GRID4(:,:)
      REAL , ALLOCATABLE :: GRID5(:,:)
      REAL , ALLOCATABLE :: GRID6(:,:)
      INTEGER :: ios1

      CONTAINS
!
! SUBROUTINE TO LOAD MSS GRID
!
      SUBROUTINE LDMSSG(BLAT,GDNAME,GRSIZE,GRCNT,GRDATA,NGRD,NREF,  &
     & NCENTR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION GDNAME(1),GRSIZE(1),GRCNT(1),GRDATA(4,1)
      DIMENSION NREF(6),NCENTR(6),NG(6)
      CHARACTER*8  ::  GDNAME
      REAL , ALLOCATABLE :: AREA(:)
      DATA NG/6*0/
!
!  INITIALIZATIONS FOR UP TO 6 POSSIBLE GRIDS
        write(6,*)' dbg MSS COMPUTATIONS '
        LDEMGR=.FALSE.
        LMSSGR=.FALSE.
        IF(GDNAME(NGRD).EQ.'DEMGRD') THEN
        LDEMGR=.TRUE.
        LDEM=.TRUE.
        NG(NGRD)=2
        ENDIF
        IF(GDNAME(NGRD).EQ.'MSSGRD') THEN
        LMSSGR=.TRUE.
        LMSS=.TRUE.
        NG(NGRD)=1
        ENDIF
!
!*********************************************************************
!
      WRITE(6,*)'******************************************************'
      WRITE(6,*)'                                                      '
      WRITE(6,1000)GDNAME(NGRD)
1000  FORMAT(1X,'PROCESSING THE FOLLOWING GRID: ',A6)
!       do j=1,4
!       write(6,*)' dbg GRDATA ',GRDATA(J,NGRD),j
!       enddo
      WRITE(6,1001)
      WRITE(6,1002)
      WRITE(6,1003)
1001  FORMAT(1X,'IF INDEX IS 0, THE REFERENCE SURFACE IS THE ELLIPSOID')
1002  FORMAT(1X,'IF INDEX IS 1, THE REFERENCE SURFACE IS MEAN SEA SURF')
1003  FORMAT(1X,'IF INDEX IS 2, THE REFERENCE SURFACE IS THE PLAN.CENT')
      WRITE(6,*)' INDEX= ',NREF(NGRD)
      WRITE(6,1004)
      WRITE(6,1005)
1004  FORMAT(1X,'IF NCENTR IS 0, ELEVATIONS ON THE GRID INTERSACTIONS ')
1005  FORMAT(1X,'IF NCENTR IS 1, ELEVATIONS AT THE CENTER OF THE CELL')
      WRITE(6,*)'NCENTR= ',NCENTR(NGRD)
!***********************************************************************
!
!..
!...DETERMINE THE LONGITUDE and LATITUDE SPACE COVERED BY GRID FILE
!...FROM (MAX to MIN DEG. LAT.)
!...
! Longitude
      DPHI(NG(NGRD))=GRSIZE(NGRD)/3600.D0
      DIFF=0.D0
      IF(NCENTR(NGRD).EQ.1) DIFF=DPHI(NG(NGRD))/2
      XLONW(NG(NGRD))=GRDATA(3,NGRD)+DIFF
      XLONE=GRDATA(4,NGRD)-DIFF
!      IF(XLONW(NG(NGRD)).EQ.0.0.AND.XLONE.EQ.360.0) THEN
      IF(GRDATA(3,NGRD).EQ.0.0.AND.GRDATA(4,NGRD).EQ.360.0) THEN
      LGLOB(NG(NGRD))=.TRUE.
      XLONW(NG(NGRD))=XLONW(NG(NGRD))-2.0D0*DPHI(NG(NGRD))
      ENDIF
! Latitude
      XLMAX=BLAT
      XLMIN=-BLAT
      XLATMA=GRDATA(1,NGRD)-DIFF
      XLATMI=GRDATA(2,NGRD)+DIFF
      NOFF = 0
      IF(XLMAX.LT.XLATMA) THEN
       IMAX = INT((XLATMA-XLMAX)/DPHI(NG(NGRD))) + 1
       XLATMA = XLATMA - IMAX*DPHI(NG(NGRD))
!... DETERMINE READ OFF NUMBER OF READING GRID FILE
       NOFF = IMAX
      ENDIF
      IF(XLMIN.GT.XLATMI) THEN
       IMIN = INT((XLMIN-XLATMI)/DPHI(NG(NGRD))) + 1
       XLATMI = XLATMI + IMIN*DPHI(NG(NGRD))
      ENDIF
      XLATS(NG(NGRD))=XLATMI
      NNL(NG(NGRD))=NINT((XLATMA-XLATMI)/DPHI(NG(NGRD))) + 1
      NUM=NINT((GRDATA(4,NGRD)-GRDATA(3,NGRD))/DPHI(NG(NGRD)))
      IF (NCENTR(NGRD) == 0) THEN
          NUM = NUM + 1
      END IF
      NEW(NG(NGRD))=NUM+4
      ALLOCATE(AREA(NUM))
!      print *,'ldmssg: dphi: ',dphi(NG(NGRD))
!      print *,'ldmssg: xlmax: ',xlmax
!      print *,'ldmssg: xlmin: ',xlmin
!      print *,'ldmssg: xlatma: ',xlatma
!      print *,'ldmssg: xlatmi: ',xlatmi
!      print *,'NNL: ',nnl(NG(NGRD))
!      print *,'xlats: ',xlats(ng(ngrd))
!      print *,'ldmssg: noff: ',noff
!*********************************************************************

      WRITE(6,*)'                                                      '
      WRITE(6,*)'******************************************************'
      WRITE(6,*)'                                                      '
      WRITE(6,*)' PRESENT GRID SPECIFICS '
      WRITE(6,*)' GRID # :',NGRD
      WRITE(6,*)' MINIMUM LATITUDE: ',XLATS(NG(NGRD))
      WRITE(6,*)' MAXIMUM LATITUDE: ',XLATMA
      WRITE(6,*)' WEST LONGITUDE:   ',XLONW(NG(NGRD))
      WRITE(6,*)' NUMBER OF RECORDS IN THE GRID :',NNL(NG(NGRD))
      WRITE(6,*)' NUMBER OF WORDS IN EACH RECORD:',NEW(NG(NGRD))-4
      WRITE(6,*)' GRIDSIZE :',DPHI(NG(NGRD))
      WRITE(6,*)' LGLOB :',LGLOB(NG(NGRD))
      WRITE(6,*)' READ OFF #: ',NOFF
      WRITE(6,*)'                                                      '
      WRITE(6,*)'******************************************************'
      WRITE(6,*)'                                                      '
!*********************************************************************

          IF(LDEMGR) THEN

      ALLOCATE(GRID2(NNL(NG(NGRD)),NEW(NG(NGRD))),STAT=memmss)

      IF(MEMMSS.NE.0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN LDMSSG'
       WRITE(6,*) 'TROUBLE ALLOCATING MEMORY FOR DEM GRID'
       WRITE(6,*) 'MEMMSS IS: ', MEMMSS
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

!...
!...READ DEM GRID INTO DYNAMICALLY ALLOCATED MEMORY
!...
      OPEN(unit=29,file=GDNAME(NGRD),status='old',action='read',   &
     &     form='unformatted',iostat=ioerror)
      IF(ioerror > 0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN LDMSSG'
       WRITE(6,*) 'TROUBLE OPENING DEMGRD'
       WRITE(6,*) 'ioerror IS: ', ioerror
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

!...FIRST READ OFF DEM LATITUDE OVER LATMX
      DO I=1,NOFF
       READ(29,IOSTAT=ios1) AREA
       IF(ioerror .ne. 0) THEN
        WRITE(6,*) ''
        WRITE(6,*) '*****************************************'
        WRITE(6,*) 'ABNORMAL TERMINATION IN LDMSSG'
        WRITE(6,*) 'TROUBLE READING DEMGRD'
        WRITE(6,*) 'ioerror IS: ', ioerror
        WRITE(6,*) '*****************************************'
        WRITE(6,*) ''
        STOP 69
       ENDIF
      END DO

!...READ DEM GRID DATA
      DO I=1,NNL(NG(NGRD))
       II=NNL(NG(NGRD))-I+1
       READ(29,END=5000) AREA
        DO K=1,NUM
       GRID2(II,K+2)=AREA(K)
        ENDDO
       IF(ioerror .ne. 0) THEN
        WRITE(6,*) ''
        WRITE(6,*) '*****************************************'
        WRITE(6,*) 'ABNORMAL TERMINATION IN LDMSSG'
        WRITE(6,*) 'TROUBLE LOADING DEMGRD'
        WRITE(6,*) 'ioerror IS: ', ioerror
        WRITE(6,*) '*****************************************'
        WRITE(6,*) ''
        STOP 69
       ENDIF
       GRID2(II,1)=GRID2(II,3)
       GRID2(II,2)=GRID2(II,3)
       GRID2(II,NUM+4)=GRID2(II,NUM+2)
       GRID2(II,NUM+3)=GRID2(II,NUM+2)
      END DO
5000   CONTINUE
          ENDIF


!
      IF(LDEMGR) GOTO 7000
!
          IF(LMSSGR) THEN

      ALLOCATE(GRID1(NNL(NG(NGRD)),NEW(NG(NGRD))),STAT=memmss)

      IF(MEMMSS.NE.0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN LDMSSG'
       WRITE(6,*) 'TROUBLE ALLOCATING MEMORY FOR MSS GRID'
       WRITE(6,*) 'MEMMSS IS: ', MEMMSS
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

!...
!...READ MSS GRID INTO DYNAMICALLY ALLOCATED MEMORY
!...
      OPEN(unit=29,file=GDNAME(NGRD),status='old',action='read',  &
     &     form='unformatted',iostat=ioerror)
      IF(ioerror > 0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN LDMSSG'
       WRITE(6,*) 'TROUBLE OPENING MSSGRD'
       WRITE(6,*) 'ioerror IS: ', ioerror
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF

!...FIRST READ OFF MSS LATITUDE OVER LATMX
      DO I=1,NOFF
       READ(29,IOSTAT=ios1) AREA
       IF(ioerror .ne. 0) THEN
        WRITE(6,*) ''
        WRITE(6,*) '*****************************************'
        WRITE(6,*) 'ABNORMAL TERMINATION IN LDMSSG'
        WRITE(6,*) 'TROUBLE READING MSS'
        WRITE(6,*) 'ioerror IS: ', ioerror
        WRITE(6,*) '*****************************************'
        WRITE(6,*) ''
        STOP 69
       ENDIF
      END DO

!...READ MSS GRID DATA
      DO I=1,NNL(NG(NGRD))
       II=NNL(NG(NGRD))-I+1
       READ(29,END=6000) AREA
        DO K=1,NUM
       GRID1(II,K+2)=AREA(K)
        ENDDO
       IF(ioerror .ne. 0) THEN
        WRITE(6,*) ''
        WRITE(6,*) '*****************************************'
        WRITE(6,*) 'ABNORMAL TERMINATION IN LDMSSG'
        WRITE(6,*) 'TROUBLE LOADING MSSGRD'
        WRITE(6,*) 'ioerror IS: ', ioerror
        WRITE(6,*) '*****************************************'
        WRITE(6,*) ''
        STOP 69
       ENDIF
       GRID1(II,1)=GRID1(II,3)
       GRID1(II,2)=GRID1(II,3)
       GRID1(II,NUM+4)=GRID1(II,NUM+2)
       GRID1(II,NUM+3)=GRID1(II,NUM+2)
      END DO
6000   CONTINUE
          ENDIF
!
7000   CONTINUE
      CLOSE(29)

      DEALLOCATE(AREA)

      END SUBROUTINE LDMSSG

!
! SUBROUTINE TO UNLOAD MSS GRID
!
      SUBROUTINE ULMSSG
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)

      IF(LMSS) THEN
      DEALLOCATE(GRID1,STAT=memmss)
      IF(MEMMSS.NE.0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN ULMSSG'
       WRITE(6,*) 'TROUBLE DEALLOCATING MEMORY FOR MSS GRID'
       WRITE(6,*) 'MEMMSS IS: ', MEMMSS
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF
      ENDIF

      IF(LDEM) THEN
      DEALLOCATE(GRID2,STAT=memmss)
      IF(MEMMSS.NE.0) THEN
       WRITE(6,*) ''
       WRITE(6,*) '*****************************************'
       WRITE(6,*) 'ABNORMAL TERMINATION IN ULMSSG'
       WRITE(6,*) 'TROUBLE DEALLOCATING MEMORY FOR DEM GRID'
       WRITE(6,*) 'MEMMSS IS: ', MEMMSS
       WRITE(6,*) '*****************************************'
       WRITE(6,*) ''
       STOP 69
      ENDIF
      ENDIF

      END SUBROUTINE ULMSSG

      END MODULE mssgfc_mod
