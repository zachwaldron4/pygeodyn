!$PQUASR
      SUBROUTINE PQUASR(PMPA,NDIM1,NDIM2,NM,INQUAS,LNPNM,QUAINF,B)
!********1*********2*********3*********4*********5*********6*********7**
! PQUASR           06.05.91            9106.0    LUCIA TSAOUSSI
!
!
! FUNCTION:  COMPUTES THE PARTIALS FOR THE QUASAR COORDINATES
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PMPA    I/O   A    A-MATRIX - PARTIAL DERIVATIVES OF MEASUREMENTS
!                      WRT ADJUSTED PARAMETERS
!   NDIM1   I     S    FIRST DIMENSION OF PMPA ARRAY
!   NDIM2   I     S    SECOND DIMENSION OF PMPA ARRAY
!   NM      I     S    NUMBER OF OBSERVATIONS IN THE BLOCK
!   INQUAS  I     S    POINTER TO THE QUASAR COORDINATES IN PMPA
!   LNPNM   I     S    .TRUE. IF THE FIRST DIMENSION IS THE NUMBER OF
!                      MEASUREMENTS IN THE BLOCK
!   DEDA    O     A    PARTIAL DERIVATIVE OF EHAT WRT RIGHT ASCENSION
!   DEDD    O     A    PARTIAL DERIVATIVE OF EHAT WRT DECLINATION
!   QUAINF  I     A    QUASAR INFORMATION ARRAY (SEE S/R INIVLB)
!   B       I     A    BASELINE VECTOR FROM SITE1 TO SITE2
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER (ZERO=0.D0)
!
      COMMON/CBLOKV/QUANO(3),EHAT(3),DEDA(3),DEDD(3),EFLG
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/VLBI/NQUA,NADJQ,NADJQV,NQUAB,IYMDV,NVOPT,NXVLBI
      DIMENSION PMPA(NDIM1,NDIM2),QUAINF(12,NQUAB),B(NM,3)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! *******
      DO 50 J=1,NADJQ
      IF (QUANO(1).EQ.QUAINF(2,J)) THEN
!     WRITE(6,*) ' PQUASR : QUASAR ID  ',QUANO(1)
!
!  *** PARTIALS OF EHAT WRT RIGHT ASCENSION
!
      DEDA(1)=-QUAINF(5,J)*QUAINF(8,J)
      DEDA(2)=QUAINF(6,J)*QUAINF(8,J)
      DEDA(3)=ZERO
!
!  ***  PARTIALS OF EHAT WRT DECLINATION
!
      DEDD(1)=-QUAINF(7,J)*QUAINF(6,J)
      DEDD(2)=-QUAINF(7,J)*QUAINF(5,J)
      DEDD(3)=QUAINF(8,J)
      ID=J
      END IF
   50 END DO
!     WRITE(6,*) ' PQUASR : INQUAS  ',INQUAS
!     WRITE(6,*) ' PQUASR : DE/DA  ',DEDA
!     WRITE(6,*) ' PQUASR : DE/DD  ',DEDD
!
!     WRITE(6,*) ' PQUASR : ID, QUAINF  ',ID,QUAINF(2,ID)
      IF(LNPNM) GO TO 200
      DO 100 N=1,NM
      PMPA(N,INQUAS+2*ID-2)=PMPA(N,INQUAS+2*ID-2)                       &
     &                     -DOTPDT(DEDA,B(N,1))/VLIGHT
      PMPA(N,INQUAS+2*ID-1)=PMPA(N,INQUAS+2*ID-1)                       &
     &                     -DOTPDT(DEDD,B(N,1))/VLIGHT
!     WRITE(6,*) ' PQUASR :  ',INQUAS+2*ID-2,INQUAS+2*ID-1
!     WRITE(6,*) ' PQUASR :  ',PMPA(INQUAS+2*ID-2),PMPA(INQUAS+2*ID-1)
  100 END DO
      RETURN
  200 CONTINUE
      DO 300 N=1,NM
      PMPA(INQUAS+2*ID-2,N)=PMPA(INQUAS+2*ID-2,N)                       &
     &                     -DOTPDT(DEDA,B(N,1))/VLIGHT
      PMPA(INQUAS+2*ID-1,N)=PMPA(INQUAS+2*ID-1,N)                       &
     &                     -DOTPDT(DEDD,B(N,1))/VLIGHT
!     WRITE(6,*) ' PQUASR : 2 ',INQUAS+2*ID-2,INQUAS+2*ID-1
!     WRITE(6,*) ' PQUASR :  ',PMPA(INQUAS+2*ID-2),PMPA(INQUAS+2*ID-1)
  300 END DO
      RETURN
      END
