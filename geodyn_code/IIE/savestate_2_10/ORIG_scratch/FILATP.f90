!$FILATP
      SUBROUTINE FILATP(PMATT,J,XSCR,ARR1,ARR2,ARR3,NM,SCALE,JJ,NUCON)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL (L)
      SAVE
!
      DIMENSION ARR1(NM)
      DIMENSION ARR2(NM)
      DIMENSION ARR3(NM)
      DIMENSION PMATT(NUCON,3,3)
      DIMENSION XSCR(3,NM)
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
      IADD=0
      DO I=1,NUCON
      PMATT(I,1,J)=(ARR1(JJ+IADD)-XSCR(1,JJ+IADD))*SCALE
      PMATT(I,2,J)=(ARR2(JJ+IADD)-XSCR(2,JJ+IADD))*SCALE
      PMATT(I,3,J)=(ARR3(JJ+IADD)-XSCR(3,JJ+IADD))*SCALE
      IADD=IADD+1
!     write(6,*)' dbg at pa x ',pmatt(i,1,j),i,' 1',j,xscr(1,I),arr1(i)
!     write(6,*)' dbg at pa y ',pmatt(i,2,j),i,' 2',j,xscr(2,i),arr2(i)
!     write(6,*)' dbg at pa z ',pmatt(i,3,j),i,' 3',j,xscr(3,i),arr3(i)
      ENDDO
      RETURN
      END
