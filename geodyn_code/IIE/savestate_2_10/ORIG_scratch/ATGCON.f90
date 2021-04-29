!$ATGCON
      SUBROUTINE ATGCON(T,ACOEF,C,S,CAUX,SAUX,NP,NSET,LINT,LEXT)
!********1*********2*********3*********4*********5*********6*********7**
! ATGCON            00/00/00            0000.0    PGMR - D.PAVLIS
!
! FUNCTION:        READS ATMOSPHERIC COEFFICIENTS  FROM
!                  DYNAMIC ARRAY AND CONTRIBUTES TO THE
!                  GRAVITY COEFFICIENTS.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   ACOEF    I    A     C AND S ATMOSPHERIC COEFFICIENT ARRAY
!   C        O    A     GRAVITY C COEFFICIENT  ARRAY / ATMOSPHERIC
!                       EFFECT APPLIED UPON OUTPUT
!   S        O    A     GRAVITY S COEFFICIENT  ARRAY / ATMOSPHERIC
!                       EFFECT APPLIED UPON OUTPUT
!   CAUX     I    A     AUXILIARY ARRAY HOLDING BACKGROUND C COEFF
!   SAUX     I    A     AUXILIARY ARRAY HOLDING BACKGROUND S COEFF
!   LINT     I    S     .TRUE.  IF INTERPOLATION OPTION IS ON
!                       .FALSE. IF INTERPOLATION OPTION IS OFF
!   LEXT     I    S     .TRUE.  PERFORM EXTRAPOLATION
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
      COMMON/AGRAV /NACSET,NADEG,NAORD,IAGCB,IAGCE,IDATB,IDATE,INTAG,   &
     &KCOUNT,KSKIP,INTC21,NXAGRA
      DIMENSION ACOEF(NADEG+1,NAORD+1,2,NACSET)
      DIMENSION C(1)
      DIMENSION S(1)
      DIMENSION CAUX(1)
      DIMENSION SAUX(1)
      IF(LINT) GOTO 100
!
      DO 20 N=2,NADEG
      NP1=N+1
!     DO 20 M=0,NAORD
      DO 20 M=0,N
      MP1=M+1
! COMMENTED OUT FOR SPECIAL VERSION
!      IF(N.EQ.2.AND.M.EQ.1)GOTO 20
! ADDED TO MAKE SPECIAL VERSION A USER OPTION IN MAINSTREAM
      IF (INTC21.EQ.0) THEN
        IF(N.EQ.2.AND.M.EQ.1)GOTO 20
      ENDIF
      NOFF=(N-1)*N/2+3*(N-1)+M+1
      C(NOFF)=CAUX(NOFF)+ACOEF(NP1,MP1,1,NSET)
      S(NOFF)=SAUX(NOFF)+ACOEF(NP1,MP1,2,NSET)
!     if(acoef(np1,mp1,1,nset).ne.0.d0)
!    +   write(6,2121) 'C',n,m,CAUX(NOFF),ACOEF(NP1,MP1,1,NSET)
!     if(acoef(np1,mp1,2,nset).ne.0.d0)
!    +   write(6,2121) 'S',n,m,SAUX(NOFF),ACOEF(NP1,MP1,1,NSET)
!2121 format(' ATGCON: ',A1,2i3,2D22.14)
   20 CONTINUE
      RETURN
  100 CONTINUE
! Perform the interpolation or extrapolation here
      DO 120 N=2,NADEG
      NP1=N+1
!     DO 120 M=0,NAORD
      DO 120 M=0,N
      MP1=M+1
!     IF(N.EQ.2.AND.M.EQ.1)GOTO 120
      NOFF=(N-1)*N/2+3*(N-1)+M+1
      C1=ACOEF(NP1,MP1,1,NSET)
      S1=ACOEF(NP1,MP1,2,NSET)
      C2=ACOEF(NP1,MP1,1,NSET+1)
      S2=ACOEF(NP1,MP1,2,NSET+1)
      CINT=C2-T(C2-C1)
      SINT=S2-T(S2-S1)
      C(NOFF)=CAUX(NOFF)+CINT
      S(NOFF)=SAUX(NOFF)+SINT
!     if(cint.ne.0.d0)
!    +   write(6,2121) 'C',n,m,CAUX(NOFF),cint
!     if(sint.ne.0.d0)
!    +   write(6,2121) 'S',n,m,SAUX(NOFF),sint
  120 CONTINUE
      RETURN
      END
