!$ASORT
       SUBROUTINE ASORT(PNAME,IPTRUA,ISATID,STACC,LREDGA,NREDGA,RTGRGA,&
                       IREDGA,ISTRTA,WTAACC,CTMACC,IDIM1,IDIM4,IDIMT,KI)
!********1*********2*********3*********4*********5*********6*********7**
! ASORT
!
!
! FUNCTION: OUTPUTS TO UNIT 6 GENERAL ACCELERATION PARAMETER INFORMATION
!           REGARDING TIME TAG CONSTRAINTS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   PNAME   I     A    PARAMETER NAME ARRAY
!   IPTRUA  I     A    POINTERS FROM UNADJUSTED TO ADJUSTED PARAMETERS
!   ISATID  I     A    SATELLITE ID
!   STACC   I     A    ACCELERATION TIME PERIODS
!   LREDGA  I     A    FLAGS FOR CONSTRAINTS APPLICATION (IDIR,ICON,ISAT
!   NREDGA  I     A    NUMBER OF CONTINUOUS TIME PERIODS (ISET,IDIR,ICON
!                      ISAT)
!   RTGRGA  O     A    ARRAY OF TIMES
!   IREDGA  0     A    ARRAY OF POINTERS TO ADJUSTED PARAMETERS
!   ISTRTA  I     A    STARTING LOCATION OF CONTINUOUS PERIODS
!   WTAACC  I     A    ARRAY OF WEIGHTS FOR ACCELERATION CONSTRAINTS
!   CTMACC  I     A    ARRAY OF CORRELATION TIMES FOR ACCELERARTION
!                      CONSTRAINTS
!   IDIM1   I     S    FIRST DIMENSION OF NREDGA
!   IDIM4   I     S    FOURTH  DIMENSION OF NREDGA
!   IDIMT   I     S    FIRST DIMENSION OF RTGRGA
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z),LOGICAL(L)
      SAVE
      CHARACTER*8 GA9P,HOLD8
      COMMON/CNIARC/NSATA,NSATG,NSETA,NEQNG,NMORDR,NMORDV,NSORDR,       &
     &              NSORDV,NSUMX,NXDDOT,NSUMPX,NPXDDT,NXBACK,NXI,       &
     &              NXILIM,NPXPF,NINTIM,NSATOB,NXBCKP,NXTIMB,           &
     &              NOBSBK,NXTPMS,NXCNIA
      COMMON/KNTSTS/KNTACC(9,40),MKNTAC,KNTDRG,KNTSLR,MAXNGA,MAXNDR,    &
     &              MAXNSR,NXKNTS
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
      COMMON/SETIOR/IORDRG,IORSRD,IORGA,IPDDRG,IPDSRD,IPDGA
      COMMON/STARTT/ESSTRT,FSSTRT
      COMMON/STARTV/SSTRTC(200)

      CHARACTER(8)      :: PNAME
      DIMENSION PNAME(2,1),IPTRUA(1),ISATID(1),STACC(1)
      DIMENSION FSEC(1),IYMD(1),IHM(1),SEC(1)
      DIMENSION LREDGA(3,3,1)
      DIMENSION NREDGA(IDIM1,3,3,IDIM4)
      DIMENSION ISTRTA(IDIM1,3,3,IDIM4)
      DIMENSION RTGRGA(IDIMT,IDIM1,3,3,IDIM4)
      DIMENSION IREDGA(IDIMT,IDIM1,3,3,IDIM4)
      DIMENSION WTAACC(IDIM1,3,3,IDIM4)
      DIMENSION CTMACC(IDIM1,3,3,IDIM4)

! ILOCSC IS LOCAL
! IT HOLDS THE STARTING LOCATION FOR GA IPTRUA FOR EACH S/C
! MAY NEED TO INCREASE IT OR MAKE IT DYNAMIC IN THE FUTURE
      INTEGER, parameter :: ilocsc_dim = 200
      DIMENSION ILOCSC( ilocsc_dim )


      DATA GA9P/'GA 9P   '/
      DATA FSEC/0.D0/
      CHARACTER(8)      :: BLANK
      CHARACTER(8)      :: WRD2
      DATA BLANK/'        '/
      EQUIVALENCE (HOLD8,HOLDR)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
!     do 5511 I=1,20
!     write(6,*)' dbg esstrt ',SSTRTC(I),I
!5511  continue


! INITIALIZE ILOCSC TO ZERO

      DO  I = 1, ilocsc_dim
      ILOCSC(I)=0
      ENDDO
!
! DEBUG >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
! write(6,*)' dbg nsata',nsata
!     do 100 i=1,3
!     do 100 j=1,3
!     do 100 k=1,nsata
!     write(6,*)' ISATID ',ISATID(k)
!     IF(LREDGA(I,J,K)) THEN
!     write(6,*)' dbg LREDGA',LREDGA(i,j,k),i,j,k
!     do 200 ij=1,mkntac
!     if(nredga(ij,i,j,k).gt.0) then
!     write(6,*)' dbg NREDGA',NREDGA(IJ,i,j,k),ij,i,j,k
!     write(6,*)' dbg ISTRTA',ISTRTA(IJ,i,j,k),ij,i,j,k
!     write(6,*)' dbg WTAACC',wtaacc(IJ,i,j,k),ij,i,j,k
!     write(6,*)' dbg CTMACC',CTMACC(IJ,i,j,k),ij,i,j,k
!     endif
!200   continue
!     ENDIF
!100   CONTINUE
!
! DEBUG <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


! FIND THE STARTING LOCATION FOR EACH SATELLITE IN IPTRUA
! LOAD IN LOCAL ARRAY ILOCSC


       J=KI
       IEC=IPVAL(IXBISA)
       DO 1001 I=1,IEC
       IF(J.GT.NSATA) GOTO 1001
    !&OLDR=PNAME(1,I)
       HOLD8=PNAME(1,I)
       IF(HOLD8(1:5).NE.GA9P(1:5)) GO TO 1001
    !&OLDR=PNAME(2,I)
       HOLD8=PNAME(2,I)
       IF(IPTRUA(I).LE.0) GO TO 1001
       WRD2=BLANK
       CALL NUMASC(ISATID(J),WRD2,28,.TRUE.)
       IF( PNAME(2,I) .EQ. WRD2 ) THEN
        !WRITE(6,34567) J,INDX,I,PNAME(1,I),PNAME(2,I),IPTRUA(I)
        ILOCSC(J)=I
        GOTO 1002
       ENDIF
      1001 ENDDO


      1002 CONTINUE

      WRITE(6,6000)
!orig WRITE(6,34569)


! LOOP 1000 FOR SATELLITE

!DO 1000 KI=1,NSATA

      TT1=SSTRTC(KI)
      INDXS=ILOCSC(KI)
      DO  I=1,3
      IF(I.EQ.1)IADD=-2
      IF(I.EQ.2)IADD=0
      IF(I.EQ.3)IADD=2
      DO  J=1,3
        IF( LREDGA(I,J,KI) ) THEN
            IK0=1
            IK=IK0
            DO  JJ=1,MKNTAC
                IF( NREDGA(JJ,I,J,KI).GT.0) THEN
                    !write(6,*)'asort: nredga ',nredga(jj,i,j,ki),jj,i,j,
                    NALL=NREDGA(JJ,I,J,KI)
                    INDXB=ISTRTA(JJ,I,J,KI)
                    !write(6,*)'asort: INDXB ',indxb
                    INDXT=INDXB
                    INDXBB=(INDXB/9)+0.05
                    IL=ILOCSC(KI)+INDXBB*9+I+J+IADD

                    WRITE(6,34569)
                    WRITE(6,34570)

                    DO  IJ=1,NALL

                        IF( INDXT .EQ. 1 ) THEN
                            T0=TT1
                        ELSE
                            T0=STACC(INDXT-9)
                        ENDIF !   INDXT .EQ. 1

                        T1=STACC(INDXT)

                        !write(6,*)'asort: t0 t1 ', t0,t1,indxt-9,indxt

                        TREDGA=(T0/2.D0)+(T1/2.D0)
                        IREDGA(IJ,IK,I,J,KI)=IPTRUA(IL)

                        !write(6,*)'asort: iptrua(il)',iptrua(il),IL

                        RTGRGA(IJ,IK,I,J,KI)=TREDGA

                        !WRITE(6,*)'asort: IREDGA ',iredga(ij,ik,i,j,ki),
                        !write(6,*)'asort: times ',t0,t1
                        !WRITE(6,*)'asort: RTGRGA ',rtgrga(ij,ik,i,j,ki),

                        ITAG=RTGRGA(IJ,IK,I,J,KI)
                        CALL YMDHMS(ITAG,FSEC,IYMD,IHM,SEC,1)
                        ITAG=SEC(1)
                        IHMS=IHM(1)*100+ITAG

                        !WRITE(6,*)' FOR SATELLITE ',KI,' AND GA PARAMETE
                        WRITE(6,'(A,1x,I8,1x,A,2(1x,I7))')&
                         'FOR SATELLITE',KI, 'AND GA PARAMETER',I,J
                        WRITE(6,34571) IREDGA(IJ,IK,I,J,KI),IYMD(1),IHMS
                        WRITE(6,6000)

                        IL=IL+9
                        INDXT=INDXT+9
                    ENDDO ! IJ

                    IK=IK+1

                ENDIF

            ENDDO ! JJ

        ENDIF !   LREDGA(I,J,KI)

        ENDDO ! J

        ENDDO ! I


!1000  CONTINUE
!
! DEBUG
!
!     write(6,*)' dbg stacc ',IPDGA
!     do 5050 ji=1,IPDGA
!     write(6,*)' dbg ',stacc(ji),ji
!5050  continue
!
! DEBUG
!
      WRITE(6,6000)
      WRITE(6,6000)
      WRITE(6,6000)
      RETURN
 6000 FORMAT(' ')
34567 FORMAT('  ',3I5,3X,A8,3X,A8,I10)
34569 FORMAT('  THE FOLLOWING ADJUSTED PARAMETER NUMBERS WITH ')
34570 FORMAT('  TIME TAGS WILL BE CONSTRAINED TOGETHER:')
34571 FORMAT('  ADJUSTED PARM NUM:',I8,' TIME TAG ',I8,3X,I6)
      END
