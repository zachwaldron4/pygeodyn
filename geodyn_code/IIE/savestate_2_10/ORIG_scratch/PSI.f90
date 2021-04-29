!$PSI
      FUNCTION PSI(N,M,K,EPSLON)
!********1*********2*********3*********4*********5*********6*********7**
! PSI              83/08/17            8308.0    PGMR - DEMOS
!                                                       CHRISTODOULIDIS
!                                                PGMR - D. ROWLANDS
!
! FUNCTION:  COMPUTE A FACTOR IN THE DOODSON COEFFCINT
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   N        I         DEGREE OF COEFFCIENT
!   M        I         TIDAL EXPANSION ARGUMENT
!   K        I         TIDAL EXPANSION ARGUMENT
!   EPSLON   I         ECLPTIC
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DATA HALF/.5D0/,ONE/1.D0/
!
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!
      TAU=EPSLON*HALF
      S=ONE
      IK=ABS(K)
      IS=1-2*MOD(IK,2)
      IF(K.LT.0) S=(FACT(N-IK)/FACT(N+IK))*IS
      IR=K
      MPR=M+IR
      NLR=N-IR
      NLM=N-M
      IRLM=IR-M
      N2=N+N
      NPR=N+IR
      COST=COS(TAU)
      SINT=SIN(TAU)
      T=COST*COST
      PSI=COST**MPR*SINT**IRLM*F2NLR(T,N2,M,IR)/(FACT(NPR)*FACT(NLM))*  &
     &  FACT(NLR)
      PSI=S*PSI
      RETURN
      END
