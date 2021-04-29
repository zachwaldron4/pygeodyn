!$MSORT
      SUBROUTINE MSORT(IARRAY,NM,JJ,KK)
!********1*********2*********3*********4*********5*********6*********7**
! MSORT
!
! FUNCTION  SORT ONE COLUMN OF A TWO-DIMENSIONAL INTEGER ARRAY ACCORDING
!           TO ONE COLUMN
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  IARRAY    I/O  A    INTEGER ARRAY TO BE SORTED
!  NM        I    S    FIRST DIMENSION OF IARRAY
!  JJ        I    S    SECOND DIMENSION OF IARRAY
!  KK        I    S    NUMBER OF COLUMN WITHIN JJ TO BE SORTED
!
! COMMENTS
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
      DIMENSION IARRAY(NM,JJ),INDEX(200),IV(200)
      DO I=1,NM
      IV(I)=IARRAY(I,KK)
      ENDDO
      CALL BBSORT(INDEX,IV,NM)
      DO I=1,NM
      IARRAY(I,KK)=IV(INDEX(I))
      ENDDO
! RE-ORDER ALL THE OTHERS
      DO I=1,JJ
      IF(I.NE.KK) THEN
      DO M=1,NM
      IV(M)=IARRAY(M,I)
      ENDDO
      DO M=1,NM
      IARRAY(M,I)=IV(INDEX(M))
      ENDDO
      ENDIF
      ENDDO
      RETURN
      END
