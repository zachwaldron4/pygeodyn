!$REDUCE
      SUBROUTINE REDUCE(ATPA,ATPL,PARVAR,NARCN,NTOT,ILINK,ICLINK)
!********1*********2*********3*********4*********5*********6*********7**
! REDUCE           00/00/00
!
!
! FUNCTION:  REDUCES THGE SIZE OF THE NORMAL MATRIX DUE TP RESENCE
!            OF LINK CONDITIONS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ATPA     I    A    ATWA MATRIX(NORMAL MATRIX)
!   ATPL     I    A    RIGHT HAND SIDE
!   NARCN    I/O  S    DIMENSION OF ARC PART OF ATPA ARRAY
!   NTOT     I/O  S    DIMENSION OF THE ATPA ARRAY
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION ATPA(1),ATPL(1),PARVAR(1),ILINK(ICLINK,2)
!
      COMMON/CITER /NINNER,NARC,NGLOBL
      COMMON/ARCLOA/NEWLEN
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
!      .....1......2............
!       .   1      C           .
!        .  1 <--- C           .
!         . 1      C           .
!          .1      C           .
!           11111112111111111111
!            ^---- 2           .
!             ^-TR 2  PARAL    .
!              .   2     ^     .
!               .  2     |     .
!                . 2     |     .
!                 .2           .
!                  2222222222222
!                   .          .
!                    .         .
!                     .        .
!                      .       .
!                       .      .
!                        .     .
!                         .    .
!                          .   .
!                           .  .
!                            . .
!                              .

      INDXNO(I)=NTOT*(I-1)-(I*(I-1))/2

      IONEP=99999999
      ITWOP=99999999

! ICLINK= NUMBER OF LINKS
      DO 1000 ITIMES=1,ICLINK
! COMPUTE THE TOTAL SIZE OF ATPA
      NT=(NTOT*(NTOT+1))/2
      LARC=.FALSE.
      LGLB=.FALSE.
      IONE=ILINK(ITIMES,1)
      ITWO=ILINK(ITIMES,2)
! FOR A SECOND LINK UPDATE THE NUMBER OF THE FIRST PARAMETER IF NEEDED
! FOR THE SECOND PARAMETER THIS IS NOT NECESSARY AS THEY ARE ORDERED (II
      IF(IONE.GT.ITWOP) THEN
      IONE=IONE-1
      ENDIF
      IONEP=IONE
      ITWOP=ITWO
! FIND IF WE ARE LINKING AN ARC PARAMETER OR A GLOBAL PARAMETER
      IF(ITWO.LE.NARCN) THEN
      WRITE(6,*)'   '
      WRITE(6,*)'********************  LINK REPORT ********************'&
     &,'***************'
      WRITE(6,*)'   '
      WRITE(6,20000) ITWO, IONE
20000 FORMAT(1X,'FOLDING ARC PARAMETER ',I5,' INTO ARC PARAMETER ',I5)
      LARC=.TRUE.
      ELSE
      LGLB=.TRUE.

      IF(IONE.GT.NARCN) THEN
      WRITE(6,20010) ITWO,IONE
20010 FORMAT(1X,'FOLDING GLB PARAMETER ',I5,' INTO GLB PARAMETER ',I5)
      ELSE
      WRITE(6,20020) ITWO,IONE
20020 FORMAT(1X,'FOLDING GLB PARAMETER ',I5,' INTO ARC PARAMETER ',I5)
      ENDIF

      ENDIF
      IDIFF=ITWO-IONE
      IDO=IDIFF+1

! DBG PRINT NORMAL MATRIX
!     DO J=1,NT
!     WRITE(6,*)' dbg REDUCE ',ATPA(J),J
!     ENDDO
! DBG PRINT NORMAL MATRIX

! MANIPULATE ATPA BEGIN

! FOLD COLUMN (SEE ABOVE)
      J=0
      DO I = 1,IONE-1
      ATPA(IONE+J)=ATPA(IONE+J)+ATPA(ITWO+J)
!     write(6,*)' dbg fold elem 1 ',ITEO+J,' into elem ',IONE+J
! ZERO OUT COLUMN
      ATPA(ITWO+J)=0.D0
      J=J+NTOT-I
      ENDDO
! FOLD ROW
! FIRST FOLD SECOND PARAMETER INTO FIRST (TRIANGLE)
      I1=INDXNO(IONE)+IONE
      I2=INDXNO(ITWO)+ITWO
      IN2=INDXNO(ITWO+1)+ITWO+1
      DO J=1,IDO
      ATPA(I1+J-1)=ATPA(I1+J-1)+                                        &
     &                 ATPA(INDXNO(IONE+J-1)+IONE+IDIFF)
!     write(6,*)' dbg fold elem 2  ',INDXNO(IONE+J-1)+IONE+IDIFF,'
!    .into elem ',I1+J-1
! ZERO OUT SECOND PARAMETER ROW
      ATPA(INDXNO(IONE+J-1)+IONE+IDIFF)=0.D0
      ENDDO
! THEN FOLD SECOND PARAMETER INTO FIRST (PARALLELOGRAM))
      K=NTOT-ITWO
      DO J=1,K
!     ATPA(I1+IDO+J)=ATPA(I1+IDO+J)+
      ATPA(I1+IDIFF+J)=ATPA(I1+IDIFF+J)+                                &
     &                 ATPA(I2+J)
!     write(6,*)' dbg fold elem 3  ',I2+J,' into elem ',I1+IDIFF+J
! ZERO OUT SECOND PARAMETER ROW
      ATPA(I2+J)=0.D0
      ENDDO

! MANIPULATE ATPA END

! REMOVE ONE ROW AND ONE COLUMN CORRESPONDING TO THE SECOND PARAMETER

! FIRST COLUMN
      I=ITWO
      JJ=1
      K=NTOT-2
 1100 CONTINUE
      IF(JJ.GE.ITWO) GOTO 1200
      DO J=I,I+(K-JJ)
      ATPA(J)=ATPA(J+JJ)
!     write(6,*)' dbg ',J+JJ,'  goes to  ',J
      ENDDO
      JJ=JJ+1
      I=J
      GOTO 1100
 1200 CONTINUE

! THEN ROW
      JJ=I+NTOT
      DO I=JJ,NT
      ATPA(I-NTOT)=ATPA(I)
!     write(6,*)' the rest ',I,' GOES TO ',I-NTOT
      ENDDO

! DEBUG OLD ATPL
!     DO I=1,NTOT-1
!     WRITE(6,*)' OLD ATPL ',ATPL(I),I
!     ENDDO
! DEBUG OLD ATPL


! MANIPULATE ATPL
      ATPL(IONE)=ATPL(IONE)+ATPL(ITWO)
! REMOVE ONE ELEMENT FROM ATPL AND PARVAR AND REDUCE VECTORS
      DO I=ITWO+1,NTOT
      ATPL(I-1)=ATPL(I)
      IF(NINNER.EQ.1.AND.NGLOBL.EQ.1) PARVAR(I-1)=PARVAR(I)
      ENDDO

! DEBUG NEW ATPL
!     DO I=1,NTOT-1
!     WRITE(6,*)' NEW ATPL ',ATPL(I),I
!     ENDDO
! DEBUG NEW ATPL

! SET NEW NTOT
      NTOT=NTOT-1
      NARCN=NARCN-1
      NTN=(NTOT*(NTOT+1))/2

! DEBUG REDUCED ATPA
!     DO J=1,NT
!     WRITE(6,*)' dbg REDUCED',ATPA(J),J
!     ENDDO
! DEBUG REDUCED ATPA

      WRITE(6,20030)NARCN,NTOT,NT,NTN,NTOT
20030 FORMAT(1X,'END OF FIRST LINK : NEW NUMBER OF ARC PARAMETERS: ',   &
     &I6,1X,/,                                                          &
     &1X,'NEW TOTAL NUMBER OF PARAMETERS: ',I6,/,                       &
     &1X,'OLD SIZE OF NORMAL MATRIX:  ',I10,/,                          &
     &1X,'NEW SIZE OF NORMAL MATRIX:  ',I10,/,                          &
     &1X,'RHS HAS BEEN REDUCED TO SIZE:  ',I5)
      WRITE(6,*)'   '
      WRITE(6,*)'**************** END  LINK REPORT ********************'&
     &,'***************'
      WRITE(6,*)'   '

 1000 END DO
!
! FIGURE OUT THE NEW LENGTH OF AA(KSUM1) THAT SHOULD BE LOADED AND UNLOA
! FROM VIRTUAL MEMORY TO DYNAMIC ARRAYS WHEN THE MATRIX ATPA IS REDUCED
      NGL=NTOT-NARCN
      NEWLEN=NTOT*(NTOT+1)/2 - NGL*(NGL+1)/2
      RETURN
      END
