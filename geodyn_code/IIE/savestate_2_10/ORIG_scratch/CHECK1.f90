!
      SUBROUTINE CHECK1( SELECT, TITLE )
!
!  Under the assumption that the TITLEs of the gridded input data
!  have the names of the tidal constituents somewhere in them,
!  this routine makes an elementary check for a user setup error.
!  If the tidal constituent name is not detected, it is assumed
!  that an error has occurred and the program is stopped.
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER     (NT=8)
      LOGICAL        SELECT(NT)
      CHARACTER*160  TITLE(2,NT)
      INTRINSIC      INDEX
      LOGICAL        STOP
      CHARACTER*2    NAMES(NT)
      DATA   NAMES/'Q1','O1','P1','K1','N2','M2','S2','K2'/
!
      STOP = .FALSE.
      DO 10 I=1,NT
         IF (.NOT.SELECT(I)) GO TO 10
         IF (INDEX(TITLE(1,I),NAMES(I)).EQ.0) STOP = .TRUE.
         IF (INDEX(TITLE(2,I),NAMES(I)).EQ.0) STOP = .TRUE.
   10 END DO
!
      IF (STOP) THEN
         WRITE(6,20)
   20    FORMAT(/' Subroutine CHECK1 has detected a likely error in',   &
     &        ' the input data.'/' Possible causes:'/                   &
     &       4X,'The grids were read in the wrong order.'/              &
     &       4X,'A grid was input for a tide not SELECTed.'/            &
     &      ' Expected:',5X,'But read:')
         DO 30 I=1,NT
            IF (.NOT.SELECT(I)) GO TO 30
            WRITE(6,35) NAMES(I),TITLE(1,I)(1:60),                      &
     &                           TITLE(2,I)(1:60)
   30    CONTINUE
   35    FORMAT(5X,A2,5X,A60/12X,A60)
         STOP 1
      ENDIF
      RETURN
      END
