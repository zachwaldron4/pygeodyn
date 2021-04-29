      SUBROUTINE YMD_TO_YDD (YY,MN,DD,DDD)
!
      IMPLICIT NONE
!
      INTEGER YY,MN,DD,DDD,N2(12),Y
      save
!
      DATA N2 / 0,31,59,90,120,151,181,212,243,273,304,334 /
!
      DDD = DD + N2(MN)
!
      IF (MN.LT.3) GO TO 2000
!
      IF (YY.LT.80) THEN  ! Taking care of the "Millenium Bug" (By Bernie
        Y = YY + 2000
      ELSE
        Y = YY + 1900
      ENDIF
!
      IF (MOD(Y,4).NE.0) GO TO 2000
      IF (MOD(Y,100).NE.0) GO TO 1000
      IF (MOD(Y,400).NE.0) GO TO 2000
 1000 DDD = DDD + 1
!
 2000 RETURN
      END
