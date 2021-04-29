!$DNORMA
      SUBROUTINE DNORMA(NDEG,MORDER,C,S)
!********1*********2*********3*********4*********5*********6*********7**
! DNORMA           84/01/31            0000.0    PGMR - EDDY
!
! FUNCTION:  DENORMALIZE GEOPOTENTIAL COEFFICIENTS FROM DEGREE ONE
!            ORDER ZERO TO DEGREE N ORDER M
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NDEG     I         GEOPOTENTIAL MODEL WILL BE DENORMALIZED
!                      FROM DEGREE ONE TO THIS DEGREE(N)
!   MORDER   I         GEOPOTENTIAL MODEL WILL BE DENORMALIZED
!                      FROM ORDER ZERO TO THIS ORDER(M)
!   C       I/O        ARRAY OF NORMALIZED C COEFFICIENTS FOR INPUT
!                      ARRAY OF UNNORMALIZED C COEFFICIENTS FOR OUTPUT
!   S       I/O        ARRAY OF NORMALIZED S COEFFICIENTS FOR INPUT
!                      ARRAY OF UNNORMALIZED S COEFFICIENTS FOR OUTPUT
!
! COMMENTS:
!           THIS ROUTINE ASSUMES THE C AND S ARRAYS HAVE 2 EXTRA ZEROES
!           FOR EACH DEGREE. FOR EXAMPLE,FOR N=3,M=3 THE FOLLOWING WOULD
!           BE EXPECTED FOR THE C COEFFICIENTS
!           C(1,0),C(1,1),   0.0,   0.0
!           C(2,0),C(2,1),C(2,2),   0.0,  0.0
!           C(3,0),C(3,1),C(3,2),C(3,3),  0.0,  0.0
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      DIMENSION C(1),S(1)
      DIMENSION FACT(201)
!
      DATA ONE/1.0D0/,TWO/2.0D0/
      DATA MAXCS/201/
!
!**********************************************************************
! START OF EXECUTABLE CODE
!**********************************************************************
!
      IF(NDEG+MORDER .GT. MAXCS) GO TO 60100
      SQRT2=SQRT(TWO)
! COMPUTE SQUARE ROOTS OF FACTORIALS FOR NDEG+MORDER ELEMENTS
      NM=NDEG+MORDER
      FACT(1)=1.0D0
      DO 100 J=1,NM
      FACT(J+1)=FACT(J)*SQRT(DBLE(J))
  100 END DO
      IND=-2
      DO 400 N=1,NDEG
      N1=N+1
! ZONAL TERMS
      IND=IND+3
      CONST1=SQRT(TWO*N+1)
      C(IND)=C(IND)*CONST1
      S(IND)=S(IND)*CONST1
      CONST2=CONST1*SQRT2
! SECTORALS AND TESSERALS
      DO 300 M=1,N
      IND=IND+1
      DENORM=CONST2*FACT(N1-M)/FACT(N1+M)
      C(IND)=C(IND)*DENORM
      S(IND)=S(IND)*DENORM
  300 END DO
  400 END DO
      RETURN
60100 WRITE(IOUT6,80100) NDEG,MORDER,MAXCS
      STOP
80100 FORMAT(1X ,'GEOPOTENTIAL DEGREE PLUS ORDER EXCEEDS TEMPORARY', &
     &'STORAGE IN SUBROUTINE DNORMA. EXECUTION TERMINATING.',3I5)
      END
