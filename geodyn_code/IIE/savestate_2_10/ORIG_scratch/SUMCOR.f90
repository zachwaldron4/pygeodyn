!$SUMCOR
      SUBROUTINE SUMCOR(AA,NM,MTYPE)
!********1*********2*********3*********4*********5*********6*********7**
! SUMCOR           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   AA      I/O   A    Dynamic array of reals
!   NM       I    S    Number of measurements in the block
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/LDA/LDORANT
      COMMON/CMETI /MODEL(999),MWET(999),MDRY(999),MODESC(999),        &
     &              MAPWET(999),MAPDRY(999),METP(999),IRFRC,           &
     &              IRFPRT(10),NXCMTI
! /COBBUF/ OBSERVATION BLOCK POINTERS
      COMMON/COBBUF/IOBSBF,IBUF1 ,IBUF2 ,IWORD1,NHEADR,NWORDM
! /COBLOC/ OBSERVATION BLOCK DYNAMIC ARRAY POINTERS
!          STATION INFORMATION POINTERS TO COORDINATE SYSTEM ARRAYS
      COMMON/COBLOC/KXMN  (3,4)  ,KENVN (3,4)  ,KSTINF(3,4)  ,          &
     &       KOBS  ,KDTIME,KSMCOR,KSMCR2,KTMCOR,KOBTIM,KOBSIG,KOBSG2,   &
     &       KIAUNO,KOBCOR(9,7)  ,KFSECS(6,8)  ,KOBSUM(8)    ,          &
     &       KEDSIG,KEDSG2
      COMMON/CPRESW/LPRE9 (24),LPRE  (24,3),LSWTCH(10,8),LOBSIN,LPREPW, &
     &              LFS   (40)
      COMMON/OCLRA /LOCLRA, LOCLRS, LOCLRC(3), LRAM51
!
      DIMENSION AA(1)
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
! SUM COMPUTED OBSERVATION CORRECTIONS

      N1=KSMCOR
      N2=N1+NM-1

      !write(6, 11110)  (AA(N),N=N1,N2)
      !write(6,*) 'sumcor: NHEADR ', NHEADR

      DO 7000 IHEADR=2,NHEADR
      JHEADR=IHEADR-1
      DO 6000 IWORD =2,8
!
!     ....force adding offset, cgmas and/or lra corrections if present
!
      IF( lram51 ) go to 6100
!      if( iword == 2 )then
      if(ldorant.and.iword.eq.2) goto 6100
      !write(6,*) 'sumcor: IWORD, JHEADR, LPRE( IWORD ,JHEADR) ', &
      !                    IWORD, JHEADR, LPRE( IWORD ,JHEADR)
!      endif
      IF(LPRE  (IWORD ,JHEADR)) GO TO 6000
 6100 continue


!      if( iword == 2 )then
      !write(6,*) 'sumcor: IWORD, IHEADR, LSWTCH(IWORD ,IHEADR) ', &
      !                    IWORD, IHEADR, LSWTCH(IWORD ,IHEADR)
!      endif

      IF(.NOT.LSWTCH(IWORD ,IHEADR)) GO TO 6000

      IF(MDRY(MTYPE).EQ.1 .AND. IWORD.EQ. 3) GOTO 6000
      IF(MWET(MTYPE).EQ.1 .AND. IWORD.EQ. 4) GOTO 6000

      JOBCOR=KOBCOR(IWORD ,JHEADR)
      J2=JOBCOR+NM-1
!      if( iword == 2 )then
          !write(6,22222)  IWORD,JHEADR,(AA(J),J=JOBCOR,J2)
!      endif

      DO 5000 N=N1,N2
      AA(N     )   =AA(N     )   +AA(JOBCOR)
      JOBCOR=JOBCOR+1
 5000 END DO

 6000 END DO
 7000 END DO


      !write(6, 11111)  (AA(N),N=N1,N2)

      RETURN
11110 FORMAT(/'SUMCOR: at entry  SUMCOR ='/(1X,6D12.5))
11111 FORMAT('SUMCOR: at return SUMCOR ='/(1X,6D12.5)/)
22222 FORMAT(' ** SUMCOR **  IWORD,JHEADR =',2I12,5X / &
        'OBSCOR(N,I,J) = ',  (1X,6D12.5))
      END
