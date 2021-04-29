!
! ------------------------------------------------------------------------
!
      SUBROUTINE MULTI_ADD_3 ( N, OUT_MAT, &
     &                         C1,  MAT1,  &
     &                         C2,  MAT2,  &
     &                         C3,  MAT3,  &
     &                         C4,  MAT4,  &
     &                         C5,  MAT5,  &
     &                         C6,  MAT6,  &
     &                         C7,  MAT7,  &
     &                         C8,  MAT8,  &
     &                         C9,  MAT9,  &
     &                         C10, MAT10, &
     &                         C11, MAT11, &
     &                         C12, MAT12, &
     &                         C13, MAT13, &
     &                         C14, MAT14, &
     &                         C15, MAT15, &
     &                         C16, MAT16  )
! ************************************************************************
! *                                                                      *
! *   Routine MUILTU_ADD_3 computes a sum of N 3x3 matrices and writes   *
! *   the result  in OUT_MAT. N should be in the range [1, 16]. This     *
! *   program supports the argument list of variable lengths.            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   N  ( INTEGER*4  ) -- Number of matrices to summ.                   *
! *   C1 ( REAL*8     ) -- Coefficient before the first input matrix 3x3.*
! * MAT1 ( REAL*8     ) -- First  input matrix 3x3.                      *
! *   C2 ( REAL*8     ) -- Coefficient before the sedond input matrix 3x3*
! * MAT2 ( REAL*8     ) -- Second input matrix 3x3.                      *
! *   C3 ( REAL*8     ) -- Coefficient before the third input matrix 3x3.*
! * MAT3 ( REAL*8     ) -- Third  input matrix 3x3.                      *
! *      etc.                                                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * OUT_MAT ( REAL*8     ) -- Output matrix.                             *
! *                                                                      *
! *  ### 07-JUN-2004  MULTI_ADD_3  v1.0 (c)  L. Petrov  07-JUN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N,I,J
      DOUBLE PRECISION OUT_MAT(3,3), MAT1(3,3), MAT2(3,3),  &
     &           MAT3(3,3), MAT4(3,3), &
     &           MAT5(3,3), MAT6(3,3), MAT7(3,3), MAT8(3,3), &
     &           MAT9(3,3), &
     &           MAT10(3,3), MAT11(3,3), MAT12(3,3), &
     &           MAT13(3,3), MAT14(3,3), &
     &           MAT15(3,3), MAT16(3,3)
      DOUBLE PRECISION C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, &
     &           C11, C12, C13, &
     &           C14, C15, C16
      DOUBLE PRECISION TEMP_OUT(3,3)
!
      IF ( N .LT. 1  .OR. N .GT. 16 ) THEN
           !CALL NOUT_R8 ( OUT_MAT )    ! orig
           CALL NOUT_R8 ( 9, OUT_MAT )  ! jjm added
           RETURN
      END IF
      DO 2021 I=1,3
      DO 2020 J=1,3
      OUT_MAT(I,J)=MAT1(I,J)
2020  CONTINUE
2021  CONTINUE
!     CALL LIB$MOVC3 ( 72,  MAT1, OUT_MAT )
      CALL MUL_VC_V  ( 9, OUT_MAT, C1 )
!
      IF ( N .GE. 2 ) THEN
      DO 3021 I=1,3
      DO 3020 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
3020  CONTINUE
3021  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C2, MAT2, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 3 ) THEN
      DO 4021 I=1,3
      DO 4020 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
4020  CONTINUE
4021  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C3, MAT3, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 4 ) THEN
      DO 5021 I=1,3
      DO 5020 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
5020  CONTINUE
5021  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C4, MAT4, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 5 ) THEN
      DO 6021 I=1,3
      DO 6020 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
6020  CONTINUE
6021  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C5, MAT5, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 6 ) THEN
      DO 7021 I=1,3
      DO 7020 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
7020  CONTINUE
7021  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C6, MAT6, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 7 ) THEN
      DO 8021 I=1,3
      DO 8020 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
8020  CONTINUE
8021  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C7, MAT7, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 8 ) THEN
      DO 9021 I=1,3
      DO 9020 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9020  CONTINUE
9021  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C8, MAT8, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 9 ) THEN
      DO 9121 I=1,3
      DO 9120 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9120  CONTINUE
9121  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C9, MAT9, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 10 ) THEN
      DO 9221 I=1,3
      DO 9220 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9220  CONTINUE
9221  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C10, MAT10, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 11 ) THEN
      DO 9321 I=1,3
      DO 9320 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9320  CONTINUE
9321  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C11, MAT11, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 12 ) THEN
      DO 9421 I=1,3
      DO 9420 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9420  CONTINUE
9421  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C12, MAT12, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 13 ) THEN
      DO 9521 I=1,3
      DO 9520 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9520  CONTINUE
9521  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C13, MAT13, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 14 ) THEN
      DO 9621 I=1,3
      DO 9620 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9620  CONTINUE
9621  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C14, MAT14, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 15 ) THEN
      DO 9721 I=1,3
      DO 9720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9720  CONTINUE
9721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C15, MAT15, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 16 ) THEN
      DO 9821 I=1,3
      DO 9820 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9820  CONTINUE
9821  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C16, MAT16, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
      RETURN
      END  !  MULTI_ADD_3
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MULTI_MUL_3 ( N, OUT_MAT, MAT1, MAT2, MAT3, &
     &           MAT4, MAT5, &
     &           MAT6, MAT7, MAT8, MAT9, MAT10, MAT11, MAT12, &
     &           MAT13, MAT14, &
     &           MAT15, MAT16  )
! ************************************************************************
! *                                                                      *
! *   Routine MUILTU_MUL_3 computes a product of N 3x3 matrices and      *
! *   writes the result  in OUT_MAT. N should be in the range [1, 16].   *
! *   This program supports the argument list of variable lengths.       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   N  ( INTEGER*4  ) -- Number of matrices to summ.                   *
! * MAT1 ( REAL*8     ) -- First  input matrix 3x3.                      *
! * MAT2 ( REAL*8     ) -- Second input matrix 3x3.                      *
! * MAT3 ( REAL*8     ) -- Third  input matrix 3x3.                      *
! *      etc.                                                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * OUT_MAT ( REAL*8     ) -- Output matrix.                             *
! *                                                                      *
! *  ### 07-JUN-2004  MULTI_MUL_3  v1.0 (c)  L. Petrov  07-JUN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER  N,I,J
      DOUBLE PRECISION OUT_MAT(3,3), MAT1(3,3), MAT2(3,3), &
     &           MAT3(3,3), MAT4(3,3), &
     &           MAT5(3,3), MAT6(3,3), MAT7(3,3), &
     &           MAT8(3,3), MAT9(3,3), &
     &           MAT10(3,3), MAT11(3,3), MAT12(3,3), &
     &           MAT13(3,3), MAT14(3,3), &
     &           MAT15(3,3), MAT16(3,3)
      DOUBLE PRECISION TEMP_OUT(3,3)
!
      IF ( N .LT. 1  .OR.  N .GT. 16 ) THEN
           !CALL NOUT_R8 ( OUT_MAT )    ! orig
           CALL NOUT_R8 ( 9, OUT_MAT )  ! jjm added
           RETURN
      END IF
      DO 2021 I=1,3
      DO 2020 J=1,3
      OUT_MAT(I,J)=MAT1(I,J)
2020  CONTINUE
2021  CONTINUE
!     CALL LIB$MOVC3 ( 72, MAT1, OUT_MAT )
!
      IF ( N .GE. 2 ) THEN
      DO 1721 I=1,3
      DO 1720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
1720  CONTINUE
1721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT2, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 3 ) THEN
      DO 2721 I=1,3
      DO 2720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
2720  CONTINUE
2721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT3, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 4 ) THEN
      DO 3721 I=1,3
      DO 3720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
3720  CONTINUE
3721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT4, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 5 ) THEN
      DO 4721 I=1,3
      DO 4720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
4720  CONTINUE
4721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT5, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 6 ) THEN
      DO 5721 I=1,3
      DO 5720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
5720  CONTINUE
5721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT6, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 7 ) THEN
      DO 6721 I=1,3
      DO 6720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
6720  CONTINUE
6721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT7, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 8 ) THEN
      DO 7721 I=1,3
      DO 7720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
7720  CONTINUE
7721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT8, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 9 ) THEN
      DO 8721 I=1,3
      DO 8720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
8720  CONTINUE
8721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT9, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 10 ) THEN
      DO 9721 I=1,3
      DO 9720 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9720  CONTINUE
9721  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT10, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 11 ) THEN
      DO 9821 I=1,3
      DO 9820 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9820  CONTINUE
9821  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT11, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 12 ) THEN
      DO 9831 I=1,3
      DO 9830 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9830  CONTINUE
9831  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT12, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 13 ) THEN
      DO 9841 I=1,3
      DO 9840 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9840  CONTINUE
9841  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT13, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 14 ) THEN
      DO 9851 I=1,3
      DO 9850 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9850  CONTINUE
9851  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT14, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 15 ) THEN
      DO 9861 I=1,3
      DO 9860 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9860  CONTINUE
9861  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT15, 3, 3, OUT_MAT,-1)
      END IF
!
      IF ( N .GE. 16 ) THEN
      DO 9871 I=1,3
      DO 9870 J=1,3
      TEMP_OUT(I,J)=OUT_MAT(I,J)
9870  CONTINUE
9871  CONTINUE
!          CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
      CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT16, 3, 3, OUT_MAT,-1)
      END IF
!
      RETURN
      END
