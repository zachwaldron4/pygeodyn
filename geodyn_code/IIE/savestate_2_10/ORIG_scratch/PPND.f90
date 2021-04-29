!$PPND
      DOUBLE PRECISION Function PPND(p, ifault)
!********1*********2*********3*********4*********5*********6*********7**
! PPND             00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!     Produces normal deviate corresponding to lower tail area of p.
!     Returns ifault = 1 in input p >= 1 or <= 0, ifault = 0
!     otherwise.  If ifault = 1, PPND value is set to 0.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    p       I    S
!   ifault
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!     Algorithm AS 111 Appl. Statist. (1977) Vol. 26, p. 118
!
!     Produces normal deviate corresponding to lower tail area of p.
!     Returns ifault = 1 in input p >= 1 or <= 0, ifault = 0
!     otherwise.  If ifault = 1, PPND value is set to 0.
!     Single precision version with error epsilon = 2 ** (-31).
!     For double precision version, change REAL to DOUBLE PRECISION
!     in the FUNCTION statement and the declaration of variables;
!     change E0 to D0 in the DATA statements and change ABS, ALOG
!     and SQRT to DABS, DLOG and DSQRT in the assignment statements.
!     The hash sums are the sums of the moduli of the coefficients.
!     They have no inherent meanings, but are included for use in
!     checking transpositions.
!
      DOUBLE PRECISION zero, split, half, one, a0, a1, a2, a3,                &
     & b1, b2, b3, b4, c0, c1, c2, c3, d1, d2, p, q, r
!
      Data zero, half, one, split /0.0D0, 0.5D0, 1.0D0, 0.42D0/
!
      Data a0 /       2.50662823884D0/,                            &
     & a1 /     -18.61500062529D0/,                                &
     & a2 /      41.39119773534D0/,                                &
     & a3 /     -25.44106049637D0/,                                &
     & b1 /      -8.47351093090D0/,                                &
     & b2 /      23.08336743743D0/,                                &
     & b3 /     -21.06224101826D0/,                                &
     & b4 /       3.13082909833D0/
!
!     Hash sum for a & b = 143.70383558076
!
      Data c0 /      -2.78718931138D0/,                            &
     & c1 /      -2.29796479134D0/,                                &
     & c2 /       4.85014127135D0/,                                &
     & c3 /       2.32121276858D0/,                                &
     & d1 /       3.54388924762D0/,                                &
     & d2 /       1.63706781897D0/
!
!     Hash sum for c & d = 17.43746520924
!
!
      ifault = 0
      q = p - half
      If (ABS(q) .gt. split) goto 1
      r = q * q
      PPND = q * (((a3 * r + a2) * r + a1) * r + a0) /             &
     & ((((b4 * r + b3) * r + b2) * r + b1) * r + one)
      Return
    1 r = p
      If (q .gt.zero) r = one - p
      If (r .lt.zero) goto 2
      r = SQRT(-LOG(r))
      PPND = (((c3 * r + c2) * r + c1) * r + c0) /                 &
     & ((d2 * r + d1) * r + one)
      If (q .lt. zero) PPND = -PPND
      Return
    2 ifault = 1
      PPND = zero
      Return
      END
