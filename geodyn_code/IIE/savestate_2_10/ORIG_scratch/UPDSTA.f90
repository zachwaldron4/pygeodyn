      SUBROUTINE UPDSTA(SUMPX,SUMRNP,PXDDOT,NEQN,NSAT,IPXST)
!********1*********2*********3*********4*********5*********6*********7**
! UPDSTA                                         PGMR - D. ROWLANDS
!
!
! FUNCTION:  UPDATE THE VARIATIONAL EQUATION INTEGRATION SUMS AFTER A
!            DELTA STATE EPOCH
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   SUMPX   I/O   A    INTEGRATION SUMS FOR ENTIRE SET OF VARIATIONAL EQ
!   SUMRNP   I    A    INTEGRATION SUMS FOR A DELTA STATE PARAMETER AT
!                      (NEAR) THE EPOCH OF THE DELTA STATE
!   PXDDOT   I    A    INTEGRATED PARTIAL DERIVATIVES
!   NEQN     I    S    NUMBER OF VARIATIONAL EQUATIONS
!   NSAT     I    S    NUMBER OF SATELLITES IN VARIATIONAL EQUATIONS
!   IPXST    I    S    STARTING LOCATION IN VARIATIONAL EQUATIONS OF
!                      THE DELTA STATE PARAMTER (WHICH HAS JUST OCCURED)
!
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION SUMPX(NEQN,3,NSAT,2),SUMRNP(6,3,NSAT,2)
      DIMENSION PXDDOT(NEQN,3,NSAT)
!
      DO 300 ISAT=1,NSAT
      DO 200 K=1,3
      IPT=IPXST
      DO 100 N=1,6
      SUMPX(IPT,K,ISAT,2)=SUMRNP(N,K,ISAT,2)                            &
     &                   +PXDDOT(IPT,K,ISAT)
      SUMPX(IPT,K,ISAT,1)=SUMPX(IPT,K,ISAT,2)                           &
     &                   +SUMRNP(N,K,ISAT,1)
      IPT=IPT+1
  100 END DO
  200 END DO
  300 END DO
      RETURN
      END
