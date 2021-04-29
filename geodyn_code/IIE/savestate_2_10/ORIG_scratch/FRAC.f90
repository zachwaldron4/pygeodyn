!$FRAC
      FUNCTION FRAC(x)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!
      ix = INT(x)
      frac = x - ix
      return
      END
