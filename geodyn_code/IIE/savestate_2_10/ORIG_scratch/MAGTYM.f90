!$MAGTYM
!      SUBROUTINE MAGTYM(DAY)
      SUBROUTINE MAGTYM( IYMD, IHMS )
!********1*********2*********3*********4*********5*********6*********7**
! MAGTYM           00/00/00            0000.0    PGMR -
!
! FUNCTION: fill COEFT magnetic coefficient array
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    REAL DYNAMIC ARRAY
!   II      I/O   A    INTEGER DYNAMIC ARRAY
!   LL      I/O   A    LOGICAL DYNAMIC ARRAY

!   IYMD     I    S    YYMMDD of current time
!   IHMS     I    S    HHMMSS of current time
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      COMMON/MAGCF0/ GH0(8), GH1(8), GH2(8), GH3(8),                    &
     &               GH4(8), GH5(8), GH6(8), GH7(8),                    &
     &               GHDOT0(8), GHDOT1(8), GHDOT2(8), GHDOT3(8),        &
     &               GHDOT4(8), GHDOT5(8), GHDOT6(8), GHDOT7(8),        &
     &               DUM, NMAXGH
      COMMON/MAGCOF/COEFT(8,8),DUM1,NMXGH1
      COMMON/PRTCTL/IPRINT
!
!      DIMENSION AA(*),II(*),LL(*)
      DIMENSION COEFDT(8,8)
      DIMENSION COEF(8,8)
      EQUIVALENCE (COEFDT(1,1), GHDOT0(1))
      EQUIVALENCE (COEF(1,1), GH0(1))
!
!
      data kentry/0/
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
      kentry = kentry + 1
!
      NMXGH1=NMAXGH
      NMAXG8=NMXGH1+1
      NMAXG8=MAX(1,NMAXG8)
!
!>>>>>>> need correct time subroutine here !!!!!!!
!      DAY0=YMDAY(600100,0,0.0D0)
!
!      DT=(DAY-DAY0)/365.25D0
!>>>>>>>
!
      IYMD0 = 600100
      IHMS0 = 000000
      call diff( iymd0, ihms0, iymd, ihms, iday, isec )
      DT = IDAY + DBLE( ISEC ) / 86400.D0
!      if( kentry .le. iprint ) then
!        write(6,*) 'magtym: iymd0, ihms0, iymd, ihms, iday, isec ',
!     &                      iymd0, ihms0, iymd, ihms, iday, isec
!        write(6,*) 'magtym: dt ', dt
!        write(6,*) 'magtym: NMAXG8 ', NMAXG8
!      endif
!
      DO 10099 I=1,NMAXG8
         DO 10098 J=1,NMAXG8
            COEFT(I,J)=COEF(I,J)+DT*COEFDT(I,J)
!      if( kentry .le. iprint ) then
!        write(6,*) 'magtym: i, j, coef(i,j), coeft(i,j) ',
!     &                      i, j, coef(i,j), coeft(i,j)
!      endif
10098    CONTINUE
10099 END DO
      RETURN
      END
