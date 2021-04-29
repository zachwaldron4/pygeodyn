!$INDXNM
      SUBROUTINE INDXNM(I,ICS,N,M)
!********1*********2*********3*********4*********5*********6*********7**
! INDXNM            00/00/00            9212.0    PGMR - N.PAVLIS
!
! FUNCTION:       GIVEN THE LOCATION OF THE ELEMENT WITHIN A STREAM
!                 OF C AND S COEFFICIENTS, THIS SUBROUTINE IDENTIFIES
!                 THE NATURE (C OR S) OF THE ELEMENT, THE DEGREE AND
!                 THE ORDER WITHIN THE EXPANSION
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   I        I    S    Ith ELEMENT OF ATMOSPHERIC DATA RECORD
!                       CONTAINING C AND S COEFFICIENTS)
!   ICS      O    S    C AND S COEFFICIENTS
!                      ICS=0  - C COEFFICIENT
!                      ICS=1  - S COEFFICIENT
!
!   N        O    S    DEGREE OF ELEMENT
!
!   M        O    S    ORDER OF ELEMENT
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
!
      IN=INT(SQRT(DBLE(I-1)))
      N=IN
      IM1=I-IN*IN-1
      IM2=MOD(IM1,2)
      IM=(IM1+1)/2
!   S COEFFICIENT
      IF(IM2.EQ.0.AND.IM.NE.0) THEN
!        write(6,*) 'INDXNM: S COEFFICIENT '
        ICS=1
      ENDIF
!   C COEFFICIENT
      IF(IM2.NE.0.OR.IM.EQ.0) THEN
!        write(6,*) 'INDXNM: C COEFFICIENT '
       ICS=0
      ENDIF
      M=IM
      RETURN
      END
