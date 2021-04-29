!$FILSPC
      SUBROUTINE FILSPC(IOUNIT,ISIZPF,RT,JCS,MCHAR,NCHAR)
!********1*********2*********3*********4*********5*********6*********7**
! FILSPC           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IOUNIT        A    CHARACTER ARRAY. FILE UNIT NUMBER
!   ISIZPF        S
!   RT            A    CHARACTER ARRAY. NUMBER OF BLOCKS IN FILE
!   JCS           A    CHARACTER ARRAY. CHARACTER 'W'
!   MCHAR         S
!   NCHAR         S
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      CHARACTER*1 IOUNIT,RT,JCS,DIGITS,SLASH,COMMA,BLANK,PERIOD,        &
     &   RTE
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
      DIMENSION IOUNIT(8),JCS(MCHAR),DIGITS(10),RTE(3)
      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9'/
      DATA SLASH/'/'/,COMMA/','/,BLANK/' '/,PERIOD/'.'/
      DATA RTE/'R','T','='/
      DATA MDIGIT/10/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(MCHAR.LT.1) GO TO 8000
! CLEAR FILE NAME ARRAY
      DO 1000 I=1,MCHAR
      JCS(I)=BLANK
 1000 END DO
! FIRST CHARACTER AFTER COMMAND NAME MUST BE COMMA
      NCHAR=1
      JCS(1)=COMMA
! FILE NAME MUST COME NEXT
      DO 2000 I=1,6
      NCHAR=NCHAR+1
      IF(NCHAR.GT.MCHAR) GO TO 9000
      IF(IOUNIT(I).EQ.BLANK) GO TO 3000
      JCS(NCHAR)=IOUNIT(I)
 2000 END DO
      NCHAR=NCHAR+1
      IF(NCHAR.GT.MCHAR) GO TO 9000
! FOLLOWED BY SLASH
 3000 CONTINUE
      JCS(NCHAR)=SLASH
! NEXT COMES THE FILE SIZE
      LSKIP=.TRUE.
      ISCALE=10**(MDIGIT-1)
      NUMBER=ISIZPF
      DO 5000 I=1,MDIGIT
      IDIGIT=NUMBER/ISCALE
      LSKIP=LSKIP.AND.IDIGIT.EQ.0
      IF(LSKIP) GO TO 4000
      NCHAR=NCHAR+1
      IF(NCHAR.GT.MCHAR) GO TO 9000
      JCS(NCHAR)=DIGITS(IDIGIT+1)
      NUMBER=NUMBER-IDIGIT*ISCALE
 4000 CONTINUE
      ISCALE=ISCALE/10
 5000 END DO
! FOLLOWED BY A COMMA
      NCHAR=NCHAR+1
      IF(NCHAR.GT.MCHAR) GO TO 9000
      JCS(NCHAR)=COMMA
! NEXT COMES RT=
      DO 6000 I=1,3
      NCHAR=NCHAR+1
      IF(NCHAR.GT.MCHAR) GO TO 9000
      JCS(NCHAR)=RTE(I)
 6000 END DO
! FOLLOWED BY THE FILE TYPE
      NCHAR=NCHAR+1
      IF(NCHAR.GT.MCHAR) GO TO 9000
      JCS(NCHAR)=RT
! TERMINATED WITH A PERIOD
      NCHAR=NCHAR+1
      IF(NCHAR.GT.MCHAR) GO TO 9000
      JCS(NCHAR)=PERIOD
      RETURN
 8000 CONTINUE
      WRITE(IOUT6,80000) IOUNIT,ISIZPF,RT,MCHAR
      WRITE(IOUT6,99000)
      STOP 16
 9000 CONTINUE
      WRITE(IOUT6,90000) IOUNIT,ISIZPF,RT,MCHAR,NCHAR,JCS
      WRITE(IOUT6,99000)
      STOP 16
80000 FORMAT('0** FILSPC **  ERROR IN CALL TO SUBROUTINE FILSPC.'/      &
     &   '0 IOUNIT,ISIZPF,RT,MCHAR = "',8A1,'" ',I16,' "',A1,'" ',      &
     &   I16)
90000 FORMAT('0** FILSPC **  ARRAY OVERFLOW IN SUBROUTINE FILSPC.'/     &
     &   '0 IOUNIT,ISIZPF,RT = "',8A1,'" ',I16,' "',A1,'" '/            &
     &   '0 MCHAR,NCHAR =',2I16/                                        &
     &   '0 JCS = "',65A1,'"')
99000 FORMAT('0EXECUTION TERMINATING.'/1X )
      END
