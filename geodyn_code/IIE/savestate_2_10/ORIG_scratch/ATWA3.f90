!$ATWA3
      SUBROUTINE ATWA3(A,WTI,WTO)
!********1*********2*********3*********4*********5*********6*********7**
! ATWA3            83/10/30            8311.0    PGMR - D. ROWLANDS
!
! FUNCTION: GET THE ATWA MATRX GIVEN 3X3 MATRICES A&W
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A        I    A    3X3 OUTSIDE MATRX IN THE ATWA PRODUCT
!   WTI      I    A    3X3 MIDDLE MATRX IN THE ATWA PRODUCT
!   WTO      O    A    ATWA PRODUCT
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION TEMP(3,3),WTI(6),WTO(6),A(3,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      TEMP(1,1)=A(1,1)*WTI(1)+A(2,1)*WTI(2)+                            &
     &          A(3,1)*WTI(3)
      TEMP(2,1)=A(1,2)*WTI(1)+A(2,2)*WTI(2)+                            &
     &          A(3,2)*WTI(3)
      TEMP(3,1)=A(1,3)*WTI(1)+A(2,3)*WTI(2)+                            &
     &          A(3,3)*WTI(3)
      TEMP(1,2)=A(1,1)*WTI(2)+A(2,1)*WTI(4)+                            &
     &          A(3,1)*WTI(5)
      TEMP(2,2)=A(1,2)*WTI(2)+A(2,2)*WTI(4)+                            &
     &          A(3,2)*WTI(5)
      TEMP(3,2)=A(1,3)*WTI(2)+A(2,3)*WTI(4)+                            &
     &          A(3,3)*WTI(5)
      TEMP(1,3)=A(1,1)*WTI(3)+A(2,1)*WTI(5)+                            &
     &          A(3,1)*WTI(6)
      TEMP(2,3)=A(1,2)*WTI(3)+A(2,2)*WTI(5)+                            &
     &          A(3,2)*WTI(6)
      TEMP(3,3)=A(1,3)*WTI(3)+A(2,3)*WTI(5)+                            &
     &          A(3,3)*WTI(6)
      WTO(1)=TEMP(1,1)*A(1,1)+TEMP(1,2)*A(2,1)+                         &
     &          TEMP(1,3)*A(3,1)
      WTO(2)=TEMP(2,1)*A(1,1)+TEMP(2,2)*A(2,1)+                         &
     &          TEMP(2,3)*A(3,1)
      WTO(3)=TEMP(3,1)*A(1,1)+TEMP(3,2)*A(2,1)+                         &
     &          TEMP(3,3)*A(3,1)
      WTO(4)=TEMP(2,1)*A(1,2)+TEMP(2,2)*A(2,2)+                         &
     &          TEMP(2,3)*A(3,2)
      WTO(5)=TEMP(3,1)*A(1,2)+TEMP(3,2)*A(2,2)+                         &
     &          TEMP(3,3)*A(3,2)
      WTO(6)=TEMP(3,1)*A(1,3)+TEMP(3,2)*A(2,3)+                         &
     &          TEMP(3,3)*A(3,3)
      RETURN
      END
