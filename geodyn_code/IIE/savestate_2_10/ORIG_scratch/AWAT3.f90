!$AWAT3
      SUBROUTINE AWAT3(A,WT)
!********1*********2*********3*********4*********5*********6*********7**
! AWAT3            83/10/30            8311.0    PGMR - D. ROWLANDS
!
! FUNCTION: GET THE AWAT MATRX GIVEN 3X3 MATRICES A&W
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   A        I         3X3 OUTSIDE MATRX IN THE AWAT PRODUCT
!   WT      I\O        3X3 MIDDLE MATRX IN THE AWAT PRODUCT AS INPUT
!                      AWAT PRODUCT AS OUTPUT
!
! COMMENTS: NOTE THAT  WT CHANGES FROM INPUT TO OUTPUT
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION TEMP(3,3),WT(3,3),A(3,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      TEMP(1,1)=A(1,1)*WT(1,1)+A(1,2)*WT(2,1)+                          &
     &          A(1,3)*WT(3,1)
      TEMP(2,1)=A(2,1)*WT(1,1)+A(2,2)*WT(2,1)+                          &
     &          A(2,3)*WT(3,1)
      TEMP(3,1)=A(3,1)*WT(1,1)+A(3,2)*WT(2,1)+                          &
     &          A(3,3)*WT(3,1)
      TEMP(1,2)=A(1,1)*WT(1,2)+A(1,2)*WT(2,2)+                          &
     &          A(1,3)*WT(3,2)
      TEMP(2,2)=A(2,1)*WT(1,2)+A(2,2)*WT(2,2)+                          &
     &          A(2,3)*WT(3,2)
      TEMP(3,2)=A(3,1)*WT(1,2)+A(3,2)*WT(2,2)+                          &
     &          A(3,3)*WT(3,2)
      TEMP(1,3)=A(1,1)*WT(1,3)+A(1,2)*WT(2,3)+                          &
     &          A(1,3)*WT(3,3)
      TEMP(2,3)=A(2,1)*WT(1,3)+A(2,2)*WT(2,3)+                          &
     &          A(2,3)*WT(3,3)
      TEMP(3,3)=A(3,1)*WT(1,3)+A(3,2)*WT(2,3)+                          &
     &          A(3,3)*WT(3,3)
      WT(1,1)=TEMP(1,1)*A(1,1)+TEMP(1,2)*A(1,2)+                        &
     &          TEMP(1,3)*A(1,3)
      WT(2,1)=TEMP(2,1)*A(1,1)+TEMP(2,2)*A(1,2)+                        &
     &          TEMP(2,3)*A(1,3)
      WT(3,1)=TEMP(3,1)*A(1,1)+TEMP(3,2)*A(1,2)+                        &
     &          TEMP(3,3)*A(1,3)
      WT(1,2)=TEMP(1,1)*A(2,1)+TEMP(1,2)*A(2,2)+                        &
     &          TEMP(1,3)*A(2,3)
      WT(2,2)=TEMP(2,1)*A(2,1)+TEMP(2,2)*A(2,2)+                        &
     &          TEMP(2,3)*A(2,3)
      WT(3,2)=TEMP(3,1)*A(2,1)+TEMP(3,2)*A(2,2)+                        &
     &          TEMP(3,3)*A(2,3)
      WT(1,3)=TEMP(1,1)*A(3,1)+TEMP(1,2)*A(3,2)+                        &
     &          TEMP(1,3)*A(3,3)
      WT(2,3)=TEMP(2,1)*A(3,1)+TEMP(2,2)*A(3,2)+                        &
     &          TEMP(2,3)*A(3,3)
      WT(3,3)=TEMP(3,1)*A(3,1)+TEMP(3,2)*A(3,2)+                        &
     &          TEMP(3,3)*A(3,3)
      RETURN
      END
