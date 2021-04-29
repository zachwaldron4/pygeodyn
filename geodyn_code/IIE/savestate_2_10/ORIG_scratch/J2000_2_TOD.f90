      SUBROUTINE J2000_2_TOD(MJDSEC,FSEC,AA,NM,NDIM,II,XA,VA)
!***********************************************************************
! FUNCTION: TRANSFORM MEAN OF J2000 COORDINATE SYSTEM TO
!           TRUE OF DATE SYSTEM. THIS IS NECESSARY WHEN
!           THE INTEGRATION IS DONE IN THE MEAN OF J200 SYSTEM
!           AND ONE WANTS OUTPUT IN THE TRUE OF DATE SYSTEM
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      COMMON/ARCPAR/MAXSEL,NSEL,MAXDEL,NDEL,MAXMET,NMETDT,NSATID,       &
     &              IATDEN,ISATEQ,ITRMAX,ITRMIN,ITRMX2,                 &
     &              MJDSRF,MREFSY,NTDDR,NBTOTL,IDRAGM,IMRNUT,           &
     &              NXARCP
      COMMON/CORA03/KRFMT ,KROTMT,KSRTCH,KCFSC ,KTHG  ,KCOSTG,KSINTG,   &
     &              KDPSI ,KEPST ,KEPSM ,KETA  ,KZTA  ,KTHTA ,KEQN  ,   &
     &              KDPSR ,KSL   ,KH2   ,KL2   ,KXPUT ,KYPUT ,KUT   ,   &
     &              KSPCRD,KSPSIG,KSTAIN,KSXYZ ,KSPWT ,KDXSDP,KDPSDP,   &
     &              KXLOCV,KSTNAM,KA1UT ,KPLTIN,KPLTVL,KPLTDV,KPUTBH,   &
     &              KABCOF,KSPEED,KANGFC,KSCROL,KSTVEL,KSTVDV,KTIMVL,   &
     &              KSTEL2,KSTEH2,KSL2DV,KSH2DV,                        &
     &              KAPRES, KASCAL, KASOFF,KXDOTP,KSAVST,               &
     &              KANGT , KSPDT , KCONAM,KGRDAN,KUANG ,KFAMP ,        &
     &              KOFACT, KOTSGN,KWRKRS,KANFSD,KSPDSD,KPRMFS,KSCRFR,  &
     &              KCOSAS,KSINAS,KDXTID,KALCOF,KSTANF,KNUTIN,KNUTMD,   &
     &              KPDPSI,KPEPST,KDXDNU,KDPSIE,KEPSTE,NXCA03
      COMMON/CORA07/KELEVT,KSRFEL,KTINT,KFSCSD,KXTIM,KFSDM,             &
     & KSGM1,KSGM2,KNOISE,KEPHMS,NXCA07
      COMMON/CORI07/KSMSA1,KSMSA2,KSMSA3,KSMST1,KSMST2,KSMST3,KSMTYP,   &
     &              KSMT1,KSMT2,KMSDM,KIOUT,KISUPE,NXCI07
      COMMON/CRDDIM/NDCRD2,NDCRD3,NDCRD4,NDCRD5,NDCRD6,NDCRD7,          &
     &              NDCRD8,NDCRD9,NXCRDM
      COMMON/IBODPT/IBDCF(999),IBDSF(999),IBDGM(999),IBDAE(999),    &
     &              IBDPF(999),                                     &
     &              ICBDCF,ICBDSF,ICBDGM,ICBDAE,ICBDPF,             &
     &              ITBDCF,ITBDSF,ITBDGM,ITBDAE,ITBDPF,NXBDPT
      COMMON/IEPHM2/ISUPL,ICBODY,ISEQB(2),MAXDEG,ITOTSE,IREPL(988), &
     &              NXEPH2
      COMMON/LCBODY/LEARTH,LMOON,LMARS
      COMMON/LEPHM2/LXEPHM
      DIMENSION AA(1),II(1),X1(1),X2(1),FSEC(NM)
      DIMENSION XA(3), XT(3), VA(3),DUM(3),DUM1(3,2,1)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF (ICBDGM .EQ. 3) THEN
        CALL BUFEAR(MJDSEC,FSEC(1),MJDSEC,FSEC(NM),1,MJDR,FSECR,LRECAL,&
     &            AA,II)
        CALL PRECSS(MJDSEC,FSEC,AA(KETA),AA(KZTA),AA(KTHTA),           &
     &            AA(KSRTCH),NM)
        CALL NUVECT(MJDSEC,FSEC,AA(KDPSI),AA(KEPST),AA(KEPSM),         &
     &            NM,AA(KSRTCH))
        CALL NPVECT(MJDSEC,FSEC,AA(KDPSI),AA(KEPST),AA(KEPSM),         &
     &            AA(KETA),AA(KTHTA),AA(KZTA),NM,AA(KSRTCH),           &
     &            AA(KROTMT),AA(KEQN),NDCRD2,.FALSE.,                  &
     &            AA(KPDPSI),AA(KPEPST),AA(KNUTIN),AA(KNUTMD),.FALSE., &
     &            AA(KDXDNU),AA(KDPSIE),AA(KEPSTE),DUM,DUM1)
      ELSE
        CALL BUFXTR(MJDSEC,FSEC(1),MJDSEC,FSEC(NM),1,MJDR,FSECR,LRECAL,&
     &            AA)
        IF(LXEPHM) THEN
          DO IJ=1,ICBODY
            CALL BUFSUP(AA,MJDSEC,FSEC(1),MJDSEC,FSEC(NM),             &
     &                  II(KISUPE), AA(KEPHMS),IJ,1)

          ENDDO
        ENDIF
! NEW SUBROUTINE FOR NUTATION CALCULATION ONLY FOR MARS
        IF(LMARS.AND.IMRNUT.EQ.1) THEN
          CALL PRMCSS(MJDSEC,FSEC,AA(KETA),AA(KZTA),AA(KTHTA),         &
     &            AA(KSRTCH),NM)
          CALL NUVMCT(MJDSEC,FSEC,AA(KDPSI),AA(KEPST),AA(KEPSM),       &
     &            NM,AA(KSRTCH))
          CALL NPVMCT(MJDSEC,FSEC,AA(KDPSI),AA(KEPST),AA(KEPSM),       &
     &            AA(KETA),AA(KTHTA),AA(KZTA),NM,AA(KSRTCH),           &
     &            AA(KROTMT),AA(KEQN),NDCRD2,.FALSE.)
          CALL MM2EJ2(MJDSEC,FSEC,AA(KROTMT),NDCRD2,AA(KSRTCH),        &
     &                 .FALSE.)
!
        ELSE
          CALL PRXCSS(MJDSEC,FSEC,AA(KETA),AA(KZTA),AA(KTHTA),         &
     &            AA(KSRTCH),NM,AA(KDPSI),AA(KEPST),AA(KEPSM),AA,1)
          CALL NUVXCT(MJDSEC,FSEC,AA(KDPSI),AA(KEPST),AA(KEPSM),       &
     &            NM,AA(KSRTCH))
          CALL NPVXCT(MJDSEC,FSEC,AA(KDPSI),AA(KEPST),AA(KEPSM),       &
     &            AA(KETA),AA(KTHTA),AA(KZTA),NM,AA(KSRTCH),           &
     &            AA(KROTMT),AA(KEQN),NDCRD2,.FALSE.)
        ENDIF

      ENDIF

!     SAVE THE ROTATION MATRIX
      ROT1 = AA(KROTMT)
      ROT2 = AA(KROTMT+NDCRD2)
      ROT3 = AA(KROTMT+NDCRD3)
      ROT4 = AA(KROTMT+NDCRD4)
      ROT5 = AA(KROTMT+NDCRD5)
      ROT6 = AA(KROTMT+NDCRD6)
      ROT7 = AA(KROTMT+NDCRD7)
      ROT8 = AA(KROTMT+NDCRD8)
      ROT9 = AA(KROTMT+NDCRD9)
!     TRANSFORM THE COORDINATES
      XT(1) = ROT1*XA(1)+ROT4*XA(2)+ROT7*XA(3)
      XT(2) = ROT2*XA(1)+ROT5*XA(2)+ROT8*XA(3)
      XT(3) = ROT3*XA(1)+ROT6*XA(2)+ROT9*XA(3)
      XA(1) = XT(1)
      XA(2) = XT(2)
      XA(3) = XT(3)
!     TRANSFORM THE VELOCITIES
      XT(1) = ROT1*VA(1)+ROT4*VA(2)+ROT7*VA(3)
      XT(2) = ROT2*VA(1)+ROT5*VA(2)+ROT8*VA(3)
      XT(3) = ROT3*VA(1)+ROT6*VA(2)+ROT9*VA(3)
      VA(1) = XT(1)
      VA(2) = XT(2)
      VA(3) = XT(3)

      END SUBROUTINE J2000_2_TOD
