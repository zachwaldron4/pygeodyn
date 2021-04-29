      SUBROUTINE CGCOR(LNPNM,NP,NM,RESID,PARMV0,PARMVC,PMPA1,PMPA2)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

       DIMENSION RESID(NM),PARMV0(NP),PARMVC(NP)
       DIMENSION PMPA1(NP,NM),PMPA2(NM,NP)

      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH

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


!-------------------------------------------------------------------------


       IF(NPVAL0(IXFAGM).LE.0) RETURN



       IPT0=IPVAL0(IXFAGM) -1
       JPT0=IPVAL0(IXFAGM)-1

       NO=NPVAL0(IXFAGM)

!write(6,'(//A)') ' '
!do  j = 1, NP
!    write(6,'(A,1x,I8,1x,E15.7)') &
!          'cgcor: J, PMPA1(j,1) ',  J, PMPA1(j,1)
!enddo

!write(6,'(/A,3(1x,I8) )') 'cgcor: NP, NM         ', NP, NM
!write(6,'(A,3(1x,I8) )')  'cgcor: IPT0, JPT0, NO ', IPT0, JPT0, NO
!write(6,'(A,4x,L1)')       'cgcor: LNPNM ', LNPNM

      delta_resid = 0.0D0

       IF( LNPNM ) THEN

       do  J=1,NO
        do  I=1,NM

            !if( i == 1 )then
            !    write(6,'(A,1x,I8,1x, E15.7 )') &
            !          'cgcor: bef I, RESID(I) ', I, RESID(I)
            !endif !    i == 1

            if( j == 1 .or. j == 3 .or. j == 5 )then

                RESID(I) = RESID(I) -  PMPA1(IPT0+J,I)*  PARMVC(J+JPT0)
                !PMPA1(IPT0+J,I)* ( 1.0d0 + PARMVC(J+JPT0) )
                !PMPA1(IPT0+J,I)* ( 1.0d0 + (PARMVC(J+JPT0) -PARMV0(J+JPT

            else

                RESID(I) = RESID(I) - PMPA1(IPT0+J,I)*  PARMVC(J+JPT0)
            endif

            if( i == 1  )then

                if( j == 1 .or. j == 3 .or. j == 5 )then

                    delta_resid = delta_resid + &
                       PMPA1(IPT0+J,I)*   PARMVC(J+JPT0)

                       !PMPA1(IPT0+J,I)*(1.0d0+PARMVC(J+JPT0) )
                       !PMPA1(IPT0+J,I)*(1.0d0+(PARMVC(J+JPT0)-PARMV0(J+J
                       !PMPA1(IPT0+J,I)* PARMVC(J+JPT0) !-PARMV0(J+JPT0))

                else

                    delta_resid = delta_resid + &
                       PMPA1(IPT0+J,I)*  PARMVC(J+JPT0)

                endif !   j == 1 .or. j == 3 .or. j == 5

                !write(6,'(A,1x,I8,1x, E15.7 )') &
                !      'cgcor: aft I, RESID(I) ', I, RESID(I)
            endif !    i == 1

            !if( i == 1   )then
            !    write(6,'(A,2(1x,I3),3(1x, E15.7) )') &
            !          'cgcor: J, I, PMPA1, PARMVC, PARMV0 ', &
            !                  J, I, PMPA1(IPT0+J,I), &
            !                        PARMVC(J+JPT0), PARMV0(J+JPT0)
            !endif !    i == 1

        enddo  ! I
       enddo  ! J

      ELSE

       do  J=1,NO
        do  I=1,NM

            !if( i == 1  )then
            !    write(6,'(A,1x,I8,1x, E15.7 )') &
            !          'cgcor:2 bef I, RESID(I) ', I, RESID(I)
            !endif

            if( j == 1 .or. j == 3 .or. j==5 )then

                RESID(I) = RESID(I) - PMPA2(I,IPT0+J)*  PARMVC(J+JPT0)
                !PMPA2(I,IPT0+J)*  PARMVC(J+JPT0) !-PARMV0(J+JPT0)))
                !PMPA2(I,IPT0+J)* ( 1.0d0 + PARMVC(J+JPT0) )!-PARMV0(J+JP
                !PMPA2(I,IPT0+J)* ( 1.0d0 + ( PARMVC(J+JPT0) -PARMV0(J+JP
            else

                RESID(I) = RESID(I) - PMPA2(I,IPT0+J)*  PARMVC(J+JPT0)

            endif !   j == 1 .or. j == 3 .or. j==5

            if( i == 1  )then

                !write(6,'(A,1x,I8,1x, E15.7 )') &
                !      'cgcor:2 aft I, RESID(I) ', I, RESID(I)

                if( j == 1 .or. j == 3 .or. j==5 )then
                    delta_resid = delta_resid + &
                    PMPA2(I,IPT0+J)*  PARMVC(J+JPT0) !-PARMV0(J+JPT0)))
                    !PMPA2(I,IPT0+J)*(1.0d0 + PARMVC(J+JPT0) )!-PARMV0(J+
                    !PMPA2(I,IPT0+J)*(1.0d0 + ( PARMVC(J+JPT0) -PARMV0(J+
                    !PMPA2(I,IPT0+J)*PARMVC(J+JPT0)  ! -PARMV0(J+JPT0))

                else

                    delta_resid = delta_resid + &
                       PMPA2(I,IPT0+J)*  PARMVC(J+JPT0)

                endif !   j == 1 .or. j == 3 .or. j==5

            endif

            !if( i == 1 )then
            !    write(6,'(A,2(1x,I3),3(1x, E15.7) )') &
            !          'cgcor:2 J, I, PMPA2, PARMVC, PARMV0 ', &
            !                   J, I, PMPA2(I, IPT0+J), &
            !                        PARMVC(J+JPT0), PARMV0(J+JPT0)
            !endif ! i == 1

        enddo   ! I

      enddo    ! J

      ENDIF

!write(6,'(A,1x, E15.7/ )') 'cgcor: aft delta_resid ', delta_resid


      RETURN

      END
