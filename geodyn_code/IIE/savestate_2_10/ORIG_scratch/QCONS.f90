      SUBROUTINE QCONS (NPAR,NOR_MAT,NOR_VEC,SIGSP,NBSPLN,NSSPLN,QUAINF,&
     & IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  QCONS
! *                                                                      *
! *  ### 14-JUL-2005      QCONS    v1.0 (c)  L. Petrov  22-JUL-2005 ###  *
!   QUAINF   O    A    QUASAR INFORMATION ARRAY AS FOLLOWS:
!                      QUAINF(1)=ALPHANUMERIC NAME
!                      QUAINF(2)=INTEGER ID
!                      QUAINF(3)=RIGHT ASCENSION AS GIVEN IN QUAPOS CARD
!                      QUAINF(4)=DECLINATION AS GIVEN IN QUAPOS CARDS
!                      QUAINF(5)=SINE OF RIGHT ASCENSION
!                      QUAINF(6)=COSINE OF RIGHT ASCENSION
!                      QUAINF(7)=SINE OF DECLINATION
!                      QUAINF(8)=COSINE OF DECLINATION
!
! ************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!     INCLUDE 'vtd.i'
!     INCLUDE 'ves.i'
!     TYPE  ( VES__TYPE      ) :: VES
!     TYPE  ( VLBI_GDN__TYPE ) :: GDN
      INTEGER  NPAR, IUER,NBSPLN(5,NCTSTA),NSSPLN(2,NCTSTA)
      DOUBLE PRECISION NOR_MAT(*), NOR_VEC(NPAR),SIGSP(6,NCTSTA)
      DOUBLE PRECISION SIG, SIG_OFF, SIG_RAT, SIG_MIN
      DIMENSION IARRAY(1000),INDEX(1000),SARRAY(1000)
      DIMENSION QUAINF(12,NQUAB)
      PARAMETER  ( SIG_MIN = 1.D-20 )
      INTEGER    IND_STA, IND_CL0, IND_FSG, PIN_CUR, PIN_NXT, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9,J13
      INTEGER N304,N501,N502,N503
      INTEGER  I, J, MV, LOCL
      DOUBLE PRECISION ,    ALLOCATABLE :: EQU_CNS(:,:)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/NETFLG/LNNT_CNS,LNNR_CNS,LNNR_SRC, &
     &  LNNT_CNS_USE(100),LNNR_CNS_USE(100),LNNR_SRC_USE(100)
      COMMON/NETSIG/DNNT_STA_SIGMA,DNNR_STA_SIGMA,DNNR_SRC_SIGMA, &
     &  DNNT_STA_RH(3),DNNR_STA_RH(3),DNNR_SRC_RH(3) ,            &
     &  DRCOO_TRS(3,100)
      COMMON/NPCOM /NPNAME,NPVAL(92),NPVAL0(92),IPVAL(92),IPVAL0(92),   &
     &              MPVAL(28),MPVAL0(28),NXNPCM
      COMMON/NPCOMX/IXARC ,IXSATP,IXDRAG,IXSLRD,IXACCL,IXGPCA,IXGPSA,   &
     &              IXAREA,IXSPRF,IXDFRF,IXEMIS,IXTMPA,IXTMPC,IXTIMD,   &
     &              IXTIMF,IXTHTX,IXTHDR,IXOFFS,IXBISA,IXFAGM,IXFAFM,   &
     &              IXATUD,IXRSEP,IXACCB,IXDXYZ,IXGPSBW,IXCAME,IXBURN,  &
     &              IXGLBL,IXGPC ,IXGPS, IXTGPC,IXTGPS,IXGPCT,IXGPST,   &
     &              IXTIDE,IXETDE,IXOTDE,IXOTPC,IXOTPS,IXLOCG,IXKF  ,   &
     &              IXGM  ,IXSMA ,IXFLTP,IXFLTE,IXPLTP,IXPLTV,IXPMGM,   &
     &              IXPMJ2,IXVLIT,IXEPHC,IXEPHT,IXH2LV,IXL2LV,IXOLOD,   &
     &              IXPOLX,IXPOLY,IXUT1 ,IXPXDT,IXPYDT,IXUTDT,IXVLBI,   &
     &              IXVLBV,IXXTRO,IXBISG,IXSSTF,IXFGGM,IXFGFM,IXLNTM,   &
     &              IXLNTA,IX2CCO,IX2SCO,IX2GM ,IX2BDA,IXRELP,IXJ2SN,   &
     &              IXGMSN,IXPLNF,IXPSRF,IXANTD,IXTARG,                 &
     &              IXSTAP,IXSSTC,IXSSTS,IXSTAV,IXSTL2,                 &
     &              IXSTH2,IXDPSI,IXEPST,IXCOFF,IXTOTL,NXNPCX
      COMMON/SPBIAS/NCTSTA,NXSPB
      COMMON/STATN /NSTA,NSTAE,NMSTA,NCSTA,NSTAV,NSTLV,NSTEL2,          &
     &              NSTEH2,                                             &
     &              NPH2L2,                                             &
     &              NSTNUM, NSTIME, NALLST,KCONAD, NXSTAT
      COMMON/VLBI/NQUA,NADJQ,NADJQV,NQUAB,IYMDV,NVOPT,NXVLBI
      LOCL(MV,I,J) = MAX(I,J) + ((2*MV-MIN(I,J))*(MIN(I,J)-1))/2
!
!     write(6,*)' dbg entered QCONS NPAR ',NCTSTA
!     DO I=1,NCTSTA
!     write(6,*)'dbg ',NBSPLN(1,I),NBSPLN(2,I),NBSPLN(3,I),  &
!    &                 NBSPLN(4,I),NBSPLN(5,I),NSSPLN(1,I),  &
!    &                 NSSPLN(2,I),SIGSP(1,I),SIGSP(2,I),SIGSP(3,I), &
!    &                 SIGSP(4,I), SIGSP(5,I), SIGSP(6,I)
!     ENDDO

!  SORT THE ARRAYS BY STARTING LOCATION
      DO I=1,NCTSTA
      IARRAY(I)=NBSPLN(2,I)
      ENDDO
      CALL BBSORT(INDEX,IARRAY,NCTSTA)
      DO I=1,NCTSTA
      NBSPLN(2,I)=IARRAY(INDEX(I))
      ENDDO
!  SORT THE REST OF THE ARRAYS
      DO I=1,NCTSTA
      IARRAY(I)=NBSPLN(1,I)
      ENDDO
      DO I=1,NCTSTA
      NBSPLN(1,I)=IARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      IARRAY(I)=NBSPLN(3,I)
      ENDDO
      DO I=1,NCTSTA
      NBSPLN(3,I)=IARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      IARRAY(I)=NBSPLN(4,I)
      ENDDO
      DO I=1,NCTSTA
      NBSPLN(4,I)=IARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      IARRAY(I)=NBSPLN(5,I)
      ENDDO
      DO I=1,NCTSTA
      NBSPLN(5,I)=IARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      IARRAY(I)=NSSPLN(1,I)
      ENDDO
      DO I=1,NCTSTA
      NSSPLN(1,I)=IARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      IARRAY(I)=NSSPLN(2,I)
      ENDDO
      DO I=1,NCTSTA
      NSSPLN(2,I)=IARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      SARRAY(I)=SIGSP(1,I)
      ENDDO
      DO I=1,NCTSTA
      SIGSP(1,I)=SARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      SARRAY(I)=SIGSP(2,I)
      ENDDO
      DO I=1,NCTSTA
      SIGSP(2,I)=SARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      SARRAY(I)=SIGSP(3,I)
      ENDDO
      DO I=1,NCTSTA
      SIGSP(3,I)=SARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      SARRAY(I)=SIGSP(4,I)
      ENDDO
      DO I=1,NCTSTA
      SIGSP(4,I)=SARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      SARRAY(I)=SIGSP(5,I)
      ENDDO
      DO I=1,NCTSTA
      SIGSP(5,I)=SARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      SARRAY(I)=SIGSP(6,I)
      ENDDO
      DO I=1,NCTSTA
      SIGSP(6,I)=SARRAY(INDEX(I))
      ENDDO
      DO I=1,NCTSTA
      write(6,*)'dbg SCHEDULING ARRAY ',NBSPLN(1,I),NBSPLN(2,I),       &
     &                 NBSPLN(3,I),NBSPLN(4,I),NBSPLN(5,I),NSSPLN(1,I), &
     &                 NSSPLN(2,I),SIGSP(1,I),SIGSP(2,I),SIGSP(3,I),   &
     &                 SIGSP(4,I), SIGSP(5,I), SIGSP(6,I),I
      ENDDO
! FILL UP THE ZEROES
      DO I=1,NCTSTA
      IF(NBSPLN(3,I).NE.0) THEN
      KK=I
      ENDIF
      ENDDO
      DO I=1,NCTSTA
      IF(NBSPLN(3,I).EQ.0) NBSPLN(3,I)=NBSPLN(3,KK)
      ENDDO
      DO I=1,NCTSTA
      IF(SIGSP(5,I).NE.0.D0) THEN
      KK=I
      ENDIF
      ENDDO
!     DO I=1,NCTSTA
!     write(6,*)' dbg here ',SIGSP(5,I),KK,SIGSP(5,KK)
!     IF(SIGSP(5,I).EQ.0.D0) SIGSP(5,I)=SIGSP(5,KK)
!     ENDDO
! FIND THE NUMBER OF CONSTRAINTS FOR EACH CATEGORY
      N_TROP_DEL=0
      N_CLOCK=0
      N_TROP_N=0
      N_TROP_E=0
      DO I=1,NCTSTA
      IF(NBSPLN(1,I).EQ.304)N_CLOCK=N_CLOCK+1
      IF(NBSPLN(1,I).EQ.501)N_TROP_DEL=N_TROP_DEL+1
      IF(NBSPLN(1,I).EQ.502)N_TROP_N=N_TROP_N+1
      IF(NBSPLN(1,I).EQ.503)N_TROP_E=N_TROP_E+1
      ENDDO
!     WRITE(6,*)' dbg N_CLOCK',N_CLOCK
!     WRITE(6,*)' dbg N_TROP_DEL',N_TROP_DEL
!     WRITE(6,*)' dbg N_TROP_N',N_TROP_N
!     WRITE(6,*)' dbg N_TROP_E',N_TROP_E


       N304=0
       N501=0
       N502=0
       N503=0

      DO 410 J1=1,NCTSTA
         IF(NBSPLN(1,J1).EQ.501) THEN
!     write(6,*)' dbg CONSTRAINTS FOR TROP PATH DELAY '
!
! ----------- Troposheric path delay
         N501=N501+1
!
!             DO 420 J2=1,N_TROP_DEL
                 IND_STA = NBSPLN(2,J1)
          IF(SIGSP(5,J1).EQ.0.D0.AND.N501.LT.N_TROP_DEL)    &
     &    SIGSP(5,J1)=SIGSP(5,J1+1)
          IF(SIGSP(5,J1).EQ.0.D0.AND.N501.EQ.N_TROP_DEL)    &
     &    SIGSP(5,J1)=SIGSP(5,J1-1)
          IF(SIGSP(5,J1).EQ.0.D0.AND.SIGSP(5,J1+1).GT.0.D0)     &
     &    SIGSP(5,J1)=SIGSP(5,J1+1)
                 SIG = SIGSP(2,J1)*SIGSP(5,J1)
                 DO 430 J3=1,NBSPLN(3,J1)-1
                    PIN_CUR = NBSPLN(2,J1)-1+J3
                    PIN_NXT = PIN_CUR +1
                    IF ( PIN_CUR > 0  .AND.  PIN_NXT > 0  .AND. &
     &                   SIG > SIG_MIN ) THEN

!                write(6,*)' dbg 1t',LOCL(NPAR,PIN_CUR,PIN_CUR),NPAR,  &
!    & PIN_CUR,PIN_CUR,1.D0/SIG**2
!                write(6,*)' dbg 2t',LOCL(NPAR,PIN_NXT,PIN_CUR),NPAR,  &
!    & PIN_NXT,PIN_CUR,1.D0/SIG**2
!                write(6,*)' dbg 3t',LOCL(NPAR,PIN_NXT,PIN_NXT),NPAR,  &
!    & PIN_NXT,PIN_NXT,1.D0/SIG**2

!
               NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) + 1.D0/SIG**2
!     write(6,*)'dbg QCONS TROP',LOCL(NPAR,PIN_CUR,PIN_CUR),1.D0/SIG**2
               NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_CUR)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_CUR)) - 1.D0/SIG**2
!     write(6,*)'dbg QCONS TROP',LOCL(NPAR,PIN_NXT,PIN_CUR),1.D0/SIG**2
               NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_NXT)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_NXT)) + 1.D0/SIG**2
!     write(6,*)'dbg QCONS TROP',LOCL(NPAR,PIN_NXT,PIN_NXT),1.D0/SIG**2
                    END IF
 430             CONTINUE
!420          CONTINUE

         ELSE IF(NBSPLN(1,J1).EQ.304) THEN
!          ELSE IF ( VES%CNSPL_TYP(J1) == VES__CLK_TYP ) THEN
!
! ----------- Clock function
         N304=N304+1
!
!             DO 440 J4=1,N_CLOCK
                 IND_STA = NBSPLN(2,J1)
!                IND_CL0 = VES%CLK_PIN(1,0,IND_STA)
!                IND_FSG = VES%CLK_SPL_PIN(1,IND_STA)
! write ( 6, * ) 'QCONS:  ind_sta= ', ind_sta, ' IND_CL0= ', IND_CL0,' IND_FSG =
!
                 SIG = SIGSP(2,J1)*SIGSP(5,J1)
                 DO 450 J5=1,NBSPLN(3,J1)-1
                    PIN_CUR = NBSPLN(2,J1)-1+J5
                    PIN_NXT = PIN_CUR+1
!       write ( 6, * ) 'QCONS:  ind_sta= ', ind_sta, ' PIN_CUR=', &
!    &                  PIN_CUR,' PIN_NXT=',PIN_NXT ! %%%
                    IF ( PIN_CUR > 0  .AND.  PIN_NXT > 0  .AND. &
     &                   SIG > SIG_MIN ) THEN

!                write(6,*)' dbg 1c',LOCL(NPAR,PIN_CUR,PIN_CUR),NPAR,  &
!    & PIN_CUR,PIN_CUR,1.D0/SIG**2
!                write(6,*)' dbg 2c',LOCL(NPAR,PIN_NXT,PIN_CUR),NPAR,  &
!    & PIN_NXT,PIN_CUR,1.D0/SIG**2
!                write(6,*)' dbg 3c',LOCL(NPAR,PIN_NXT,PIN_NXT),NPAR,  &
!    & PIN_NXT,PIN_NXT,1.D0/SIG**2

               NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) + 1.D0/SIG**2
!     write(6,*)'dbg QCONS CLOCK',LOCL(NPAR,PIN_CUR,PIN_CUR),1.D0/SIG**2
               NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_CUR)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_CUR)) - 1.D0/SIG**2
!     write(6,*)'dbg QCONS CLOCK',LOCL(NPAR,PIN_NXT,PIN_CUR),1.D0/SIG**2
               NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_NXT)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_NXT)) + 1.D0/SIG**2
!     write(6,*)'dbg QCONS CLOCK',LOCL(NPAR,PIN_NXT,PIN_NXT),1.D0/SIG**2
                    END IF
 450             CONTINUE
!440          CONTINUE
         ELSE IF(NBSPLN(1,J1).EQ.502) THEN
!
! ----------- North atmospheric gradients
         N502=N502+1
!
!             DO 460 J6=1,N_TROP_N
                 IND_STA = NBSPLN(2,J1)
                 SIG_OFF = SIGSP(1,J1)
          IF(SIGSP(5,J1).EQ.0.D0.AND.N502.LT.N_TROP_N)    &
     &    SIGSP(5,J1)=SIGSP(5,J1+1)
          IF(SIGSP(5,J1).EQ.0.D0.AND.N502.EQ.N_TROP_N)    &
     &    SIGSP(5,J1)=SIGSP(5,J1-1)
!         IF(SIGSP(5,J1).EQ.0.D0.AND.SIGSP(5,J1+1).GT.0.D0)     &
!    &    SIGSP(5,J1)=SIGSP(5,J1+1)
                 SIG_RAT = SIGSP(2,J1)*SIGSP(5,J1)
                 DO 470 J7=1,NBSPLN(3,J1)-1
                    PIN_CUR = NBSPLN(2,J1)-1+J7
                    PIN_NXT = PIN_CUR+1
                    IF ( PIN_CUR > 0  .AND. SIG_OFF > SIG_MIN ) THEN
!
! ---------------------- Constraint on offset
!
!                write(6,*)' dbg 1 ',LOCL(NPAR,PIN_CUR,PIN_CUR),NPAR,  &
!    & PIN_CUR,PIN_CUR,1.D0/SIG_OFF**2,J1
                 NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) = &
     &           NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) + 1.D0/SIG_OFF**2
                    END IF
                    IF ( PIN_CUR > 0  .AND.  PIN_NXT > 0 .AND. &
     &                   SIG_RAT > SIG_MIN ) THEN
!
! ---------------------- Constraint on rate
!
!                write(6,*)' dbg 2 ',LOCL(NPAR,PIN_CUR,PIN_CUR),NPAR,  &
!    & PIN_CUR,PIN_CUR,1.D0/SIG_RAT**2
!                write(6,*)' dbg 3 ',LOCL(NPAR,PIN_NXT,PIN_CUR),NPAR,  &
!    & PIN_NXT,PIN_CUR,1.D0/SIG_RAT**2
!                write(6,*)' dbg 4 ',LOCL(NPAR,PIN_NXT,PIN_NXT),NPAR,  &
!    & PIN_NXT,PIN_NXT,1.D0/SIG_RAT**2
               NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) + 1.D0/SIG_RAT**2
               NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_CUR)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_CUR)) - 1.D0/SIG_RAT**2
               NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_NXT)) = &
     &         NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_NXT)) + 1.D0/SIG_RAT**2
                    END IF
 470             CONTINUE
!
!                PIN_CUR = NBSPLN(3,J1)
                 PIN_CUR = NBSPLN(2,J1)-1+NBSPLN(3,J1)
                 IF ( PIN_CUR > 0  .AND.  SIG_OFF > SIG_MIN ) THEN
!
! ------------------- Constraint on last offset
!
!                write(6,*)' dbg 5 ',LOCL(NPAR,PIN_CUR,PIN_CUR),NPAR,  &
!    & PIN_CUR,PIN_CUR,1.D0/SIG_OFF**2,J6
            NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) = &
     &      NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) + 1.D0/SIG_OFF**2
                 END IF
!460          CONTINUE
         ELSE IF(NBSPLN(1,J1).EQ.503) THEN
!
! ----------- East atmospheric gradients
        N503=N503+1
!
!             DO 480 J8=1,N_TROP_E
              IND_STA = NBSPLN(2,J1)
              SIG_OFF = SIGSP(1,J1)
!              write(6,*)' dbg SIGSP ' , SIGSP(5,J1),N503,N_TROP_E
          IF(SIGSP(5,J1).EQ.0.D0.AND.N503.LT.N_TROP_E)    &
     &    SIGSP(5,J1)=SIGSP(5,J1+1)
          IF(SIGSP(5,J1).EQ.0.D0.AND.N503.EQ.N_TROP_E)    &
     &    SIGSP(5,J1)=SIGSP(5,J1-1)
!         IF(SIGSP(5,J1).EQ.0.D0.AND.SIGSP(5,J1+1).GT.0.D0)     &
!    &    SIGSP(5,J1)=SIGSP(5,J1+1)
              SIG_RAT = SIGSP(2,J1)*SIGSP(5,J1)
!              write(6,*)' dbg SIG_RAT ' , SIG_RAT,SIGSP(2,J1),  &
!    &        SIGSP(5,J1)
                 DO 490 J9=1,NBSPLN(3,J1)-1
                    PIN_CUR = NBSPLN(2,J1)-1+J9
!              write(6,*) ' dbg east PIN_CUR ',PIN_CUR,NBSPLN(2,J1),J1
                    PIN_NXT =  PIN_CUR+1
                    IF ( PIN_CUR > 0 ) THEN
!                write(6,*)' dbg 1e',LOCL(NPAR,PIN_CUR,PIN_CUR),NPAR,  &
!    & PIN_CUR,PIN_CUR,1.D0/SIG_OFF**2
              NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) = &
     &        NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) + 1.D0/SIG_OFF**2
                    END IF
                    IF ( PIN_CUR > 0  .AND.  PIN_NXT > 0 ) THEN
!                write(6,*)' dbg 2e',LOCL(NPAR,PIN_CUR,PIN_CUR),NPAR,  &
!    & PIN_CUR,PIN_CUR,1.D0/SIG_RAT**2
!                write(6,*)' dbg 3e',LOCL(NPAR,PIN_NXT,PIN_CUR),NPAR,  &
!    & PIN_NXT,PIN_CUR,1.D0/SIG_RAT**2
!                write(6,*)' dbg 4e',LOCL(NPAR,PIN_NXT,PIN_NXT),NPAR,  &
!    & PIN_NXT,PIN_NXT,1.D0/SIG_RAT**2

              NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) = &
     &        NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) + 1.D0/SIG_RAT**2
              NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_CUR)) = &
     &        NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_CUR)) - 1.D0/SIG_RAT**2
              NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_NXT)) = &
     &        NOR_MAT(LOCL(NPAR,PIN_NXT,PIN_NXT)) + 1.D0/SIG_RAT**2
                    END IF
 490             CONTINUE
!
                 PIN_CUR = NBSPLN(2,J1)-1+NBSPLN(3,J1)
!                PIN_CUR = NBSPLN(3,J1)
                 IF ( PIN_CUR > 0  .AND.  SIG_OFF > SIG_MIN ) THEN
!                write(6,*)' dbg 5e',LOCL(NPAR,PIN_CUR,PIN_CUR),NPAR,  &
!    & PIN_CUR,PIN_CUR,1.D0/SIG_OFF**2
           NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) = &
     &     NOR_MAT(LOCL(NPAR,PIN_CUR,PIN_CUR)) + 1.D0/SIG_OFF**2
                 END IF
!480          CONTINUE
         END IF
 410  CONTINUE
!
        IF ( LNNT_CNS .OR. &
     &     LNNR_CNS .OR. &
     &     LNNR_SRC      ) THEN
           ALLOCATE ( EQU_CNS(NPAR,3) )
        END IF
!______________________________________________________________________________
!
        IF(LNNT_CNS.AND.DNNT_STA_SIGMA.GT.SIG_MIN ) THEN
!
! -------- net translation constraints for stations
!

           CALL NOUT_R8 ( 3*NPAR, EQU_CNS )
           DO 4100 J10=1,3

             DO 4110  J11=1,NSTA
! ------ Get the index of the J11-thj station in the list of stations
! ------ which coordinats are estimated
!
         IND_STA = IPVAL0(IXSTAP)+(J11-1)*3
!
! -------------- Get IND_PAR -- parameter index
!
                 IF ( IND_STA > 0 ) THEN
!                IND_PAR = IND_PAR-1+J11
                 IND_PAR = IND_STA-1+J10
                 ELSE
                 IND_PAR = 0
                 END IF
                 IF ( LNNT_CNS_USE(J11)  .AND.  &
     &                IND_PAR > 0                    ) THEN
!
                 EQU_CNS(IND_PAR,J10) = 1.0D0
                 END IF

 4110      CONTINUE
        CALL LOW_TRG_UPDATE ( NPAR, NOR_MAT, NOR_VEC, EQU_CNS(1,J10), &
      &                        DNNT_STA_RH(J10), DNNT_STA_SIGMA )

 4100      CONTINUE

        ENDIF
!______________________________________________________________________________
!
        IF(LNNR_CNS ) THEN
!
! -------- net rotation constraints for stations
!

           CALL NOUT_R8 ( 3*NPAR, EQU_CNS )
           DO 4120 J12=1,3


             DO 4130  J13=1,NSTA
! ------ Get the index of the J12-thj station in the list of stations
! ------ which coordinats are estimated
!
            IF( LNNT_CNS_USE(J13)) THEN
! This station is participating  in net rotations -- find indices for the
! three coordinates.
!  first coordinate
            IND_PAR1=IPVAL0(IXSTAP)+(J13-1)*3
            IND_PAR2=IPVAL0(IXSTAP)+(J13-1)*3+1
            IND_PAR3=IPVAL0(IXSTAP)+(J13-1)*3+2

! For this station we need the TRS_COO-RDINATES
            RD = SQRT ( DRCOO_TRS(1,J13)**2 + &
     &                   DRCOO_TRS(2,J13)**2 + &
     &                   DRCOO_TRS(3,J13)**2   )


! Now fill up EQU_CNS
                      IF ( J12 == 1 ) THEN
                           EQU_CNS(IND_PAR2,J12) = &
     &                    -DRCOO_TRS(3,J13)/RD
                           EQU_CNS(IND_PAR3,J12) = &
     &                     DRCOO_TRS(2,J13)/RD
                      ELSE IF ( J12 == 2 ) THEN
                           EQU_CNS(IND_PAR3,J12) = &
     &                    -DRCOO_TRS(1,J13)/RD
                           EQU_CNS(IND_PAR1,J12) = &
     &                     DRCOO_TRS(3,J13)/RD
                      ELSE IF ( J12 == 3 ) THEN
                           EQU_CNS(IND_PAR1,J12) = &
     &                    -DRCOO_TRS(2,J13)/RD
                           EQU_CNS(IND_PAR2,J12) = &
     &                     DRCOO_TRS(1,J13)/RD
                      END IF
            ENDIF

 4130      CONTINUE
       CALL LOW_TRG_UPDATE ( NPAR, NOR_MAT, NOR_VEC, EQU_CNS(1,J12), &
     &                      DNNR_STA_RH(J12), DNNR_STA_SIGMA )

 4120      CONTINUE

        ENDIF
!
        IF(LNNR_SRC) THEN
          CALL NOUT_R8 ( 3*NPAR, EQU_CNS )
           DO 4150 J15=1,3
              DO 4160 J16=1,NQUAB
            IF( LNNT_CNS_USE(J13)) THEN
! This station is participating  in net rotations -- find indices for the
! three coordinates.
!  first coordinate
                IND_SOU1=IPVAL0(IXVLBI)+(J3-1)*2
                IND_SOU2=IPVAL0(IXVLBI)+(J3-1)*2+1
                      IF ( J15 == 1 ) THEN
                         EQU_CNS(IND_SOU1,J15) = &
     &                   -QUAINF(8,J16)* &
     &                    QUAINF(7,J16)* &
     &                    QUAINF(6,J16)
                         EQU_CNS(IND_SOU2,J15) = &
     &                    QUAINF(5,J16)
                       ELSE IF ( J15 == 2 ) THEN
                         EQU_CNS(IND_SOU1,J15) = &
     &                   -QUAINF(8,J16)* &
     &                    QUAINF(7,J16)* &
     &                    QUAINF(6,J16)
                         EQU_CNS(IND_SOU2,J15) = &
     &                   -QUAINF(6,J16)
                        ELSE IF ( J15 == 3 ) THEN
                         EQU_CNS(IND_SOU1,J15) = &
     &                    QUAINF(8,J16)
                         EQU_CNS(IND_SOU2,J15) = 0.0D0
                       END IF
        END IF

 4160         CONTINUE
       CALL LOW_TRG_UPDATE ( NPAR, NOR_MAT, NOR_VEC, EQU_CNS(1,J15), &
     &                       DNNR_SRC_RH(J15), DNNR_SRC_SIGMA )
 4150      CONTINUE

        ENDIF
!______________________________________________________________________________

!
!     CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  QCONS
