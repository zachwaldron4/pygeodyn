!$TWOWAY
      SUBROUTINE TWOWAY(AA,PMPA,OBSC,TPARTL,NT,NM,NDIM1,NDIM2)
!********1*********2*********3*********4*********5*********6*********7**
! TWOWAY           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A
!   PMPA
!   OBSC
!   TPARTL
!   NT
!   NM
!   NDIM1
!   NDIM2
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CITERL/LSTGLB,LSTARC,LSTINR,LNADJ ,LITER1,LSTITR,          &
     &              LOBORB,LRESID,LFREEZ,LSAVEF,LHALT,LADJPI,LADJCI
      COMMON/COBBUF/IOBSBF,IBUF1 ,IBUF2 ,IWORD1,NHEADR,NWORDM
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/OCLRA /LOCLRA, LOCLRS, LOCLRC(3), LRAM51
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
!
      DIMENSION AA(1),PMPA(NDIM1,NDIM2),OBSC(NM),TPARTL(NM,NT)
!
      DATA HALF/0.5D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! DIVIDE COMPUTED OBSERVATIONS AND TIME DERIVATIVES BY 2
      DO 1000 M=1,NM
      OBSC(M)  =OBSC(M)  *HALF
      TPARTL(M,1)=TPARTL(M,1)*HALF
 1000 END DO
      IF(NT.LT.2) GO TO 1200
      DO 1100 M=1,NM
      TPARTL(M,2)=TPARTL(M,2)*HALF
 1100 END DO
 1200 CONTINUE
      IF(LNADJ) GO TO 4000
! DIVIDE PARTIALS BY 2
      DO 3000 N=1,NDIM2
      DO 2000 M=1,NDIM1
      PMPA(M,N)=PMPA(M,N)*HALF
 2000 END DO
 3000 END DO
 4000 CONTINUE
      IF(.NOT.LITER1) RETURN
! DIVIDE MEASUREMENT CORRECTIONS BY 2
      DO 7000 IHEADR=2,NHEADR
      JHEADR=IHEADR-1
      DO 6000 IWORD =2,8
!     ....fix to override data flags for LRARC card -- jjm 9/24/92
      if ((iword.eq.2).and.lram51 ) go to 6500
      IF(LPRE  (IWORD ,JHEADR)) GO TO 6000
      IF(.NOT.LSWTCH(IWORD ,IHEADR)) GO TO 6000
 6500 continue
      JOBCOR=KOBCOR(IWORD ,JHEADR)
      DO 5000 N=1,NM
      AA(JOBCOR)=AA(JOBCOR)*HALF
      JOBCOR=JOBCOR+1
 5000 END DO
 6000 END DO
 7000 END DO
      RETURN
      END
