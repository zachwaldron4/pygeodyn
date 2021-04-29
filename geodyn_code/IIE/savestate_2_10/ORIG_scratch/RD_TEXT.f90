      SUBROUTINE RD_TEXT ( FINAM, MBUF, BUF, LBUF, IUER )
! ************************************************************************
! *                                                                      *
! *  Subprogram  RD_TEXT  reads text file  FINAM and puts its content in *
! *  character array BUF.                                                *
! *                                                                      *
! *  ###  30-JUN-91     RD_TEXT   V7.2  (C) Petrov L.  15-AUG-2005 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  MBUF, LBUF, IUER
      CHARACTER  FINAM*(*), BUF(MBUF)*(*)
      CHARACTER  STR*32, STR1*32
      INTEGER  IOS, IS, J1, LEN_STR, LUN, OFFSET, UNIX_DATE
      !!!orig INTEGER, EXTERNAL :: I_LEN, ILEN, GET_UNIT, FILE_INFO, FTEL
      INTEGER*8  SIZE_I8
      LOGICAL    L_EXIST
!
      IF ( LEN(FINAM) .EQ. 0 ) THEN
      WRITE(6,*)' INCORRECT FIRST ARGUMENT - LENGTH OF STRING  = 0'
      STOP
      END IF
!
      IF ( MBUF .LE. 0 ) THEN
      WRITE(6,*)' INCORRECT SECOND ARGUMENT - MBUF = 0 '
      STOP
      END IF
!
      OPEN ( UNIT=68, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
      WRITE(6,*)' ERROR OPENING QUASAR COORDINATES FILE - RWFIL STOPS '
      STOP
      END IF
!
      LBUF=0
      DO 410 J1=1,MBUF
         CALL CLEARL ( BUF(J1), MBUF )
!        READ ( UNIT=68, FMT='(Q,A)', IOSTAT=IOS ) LEN_STR, BUF(J1)
         READ ( UNIT=68, FMT='(A)',   IOSTAT=IOS )          BUF(J1)
         IF(IOS.EQ.-1) GO TO 810
         LBUF=LBUF+1
!        write(6,2222)BUF(J1),LBUF,IOS
!2222  format(a,I5,I5)
  410 CONTINUE
!
!
 810  CONTINUE
      CLOSE ( UNIT=68 )
      IF ( LBUF .EQ. 0 ) THEN
      WRITE(6,*)' FILE :SOURCE_COORDINATES  APPEARS TO BE EMPTY '
      STOP
      END IF
      RETURN
      END
