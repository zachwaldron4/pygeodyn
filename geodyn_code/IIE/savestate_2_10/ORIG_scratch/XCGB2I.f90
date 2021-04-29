

!XCGB2I
      SUBROUTINE XCGB2I(LEXTQ,XYZCGB,XYZCGI,MJDSEC,FSEC,ISBJ20,REAQAT,  &
     &                  MJDSEA,FSSCEA,RINTEA,ISATID,ISATCG,MJDSCG,      &
     &                  II,AA )
!********1*********2*********3*********4*********5*********6*********7**
! XCGB2I            Feb 2011              0000.0    PGMR - C. DENG
!
! FUNCTION: CONVERT CoM COORDINATES IN SATELLITE BODY-CENETERED FRAME TO
!           TRUE OF REFERENCE FRAME
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LEXTQ    I    S    IF EXTERNAL QUATERNIONS IS REQUIRED FOR THIS SATELLITE
!   XYZCGB   I    A    CGMASS COORDINATES IN SATELLITE BODY-CENTERED FRAME
!   XYZCGI   O    A    CGMASS COORDINATES IN TURE OF REFERENCE DATE FRAME
!   MJDSEC   I    S    INTEGRAL MODIFIED JULIAN SECONDS
!   FSEC     I    S    FRACTIONAL SECOND
!   ISBJ20   I    S    POINTER TO SBF TO J2000 QUATERNIONS FOR THIS SATELLITE
!   REAQAT   I    A    QUATERNION INFORMATION ARRAY
!   MJDSEA   I    S    QUATERNION START TIME FOR THIS SET
!   FSSCEA   I    S    QUATERNION FSEC START TIME FOR THIS SET
!   RINTEA   I    S    QUATERNION TIME INTERVAL FOR THIS SET
!   ISATID   I    S    SATELLITE ID NUMBER
!   ISATCG   I    A    SATELLITE ID FOR CGMASS SATELLITES
!   MJDSCG   I    A    MODIFIED JULIAN SECONDS FOR CGMASS CARDS
!   II       I    A    INTEGER ARRAY
!   AA       I    A    REAL ARRAY
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/CORA04/KABIAS,KABSTR,KABSTP,KCBIAS,KCBSTR,KCBSTP,          &
     &       KRNM  ,KRKM  ,KRKJ  ,KRIJ  ,KRKPM ,                        &
     &       KRRNM ,KRRKM ,KRRKJ ,KRRIJ ,KRRKPM,                        &
     &       KURNM ,KURKM ,KURKJ ,KURIJ ,KURKPM,                        &
     &       KPRRNM,KPRRKM,KPRRKJ,KPRRIJ,KPRRKP,                        &
     &       KPMPXI,KPMPXE,KRA   ,KRELV ,KUHAT ,KWORK ,KTMPCR,KU    ,   &
     &       KS0   ,KS1   ,KXTP  ,KPXSXP,KPXDXP,KXUTDT,KRPOLE,KPXPOL,   &
     &       KXDPOL,KOBBUF,KOBSC ,KRESID,KSIGMA,KRATIO,KELEVS,          &
     &       KPMPA ,KPXSLV,KTPRTL,                                      &
     &       KFRQOF,KXYZOF,KFRQST,KFRQSA,KSTDLY,KSADLY,KXYZCG,KSCMRA,   &
     &       KEBVAL,KEBSIG,KEBNAM,KBTWB ,KTBTWA,KBTWD ,KPMPE ,          &
     &       KFSEB1,KFSEB2,KSTAMT,KPXSPA,KDIAGN,KOBOUT,KGRLT1,KGRLT2,   &
     &       KGRLT3,KGRLT4,KGRLT5,KPSTAT,KRSUNS,KCHEMR,KCHMOR,KEPOSR,   &
     &       KCHEMT,KCHMOT,KEPOST,KCHCBS,KCPOSS,KSCRTM,KXSTT ,KXSTR ,   &
     &       KXSS  ,KRANGT,KRANGR,KCHB1 ,KCHB2 ,KCHBV1,KCHBV2,KCHEB ,   &
     &       KSSTFQ,KSSTCC,KSSTSS,KSSTWT,KRLCOR,KWVLBI,KQUINF,KBRTS ,   &
     &       KBRTSV,KLRARC,KXEPBF,KXEPBR,KPRCBD,KPRTBD,KXPMPA,KXL,      &
     &       KXSIG ,KXEDTS,KXALO1,KXALO2,KXYOF2,KDLATF,KDLATS,KDLONF,   &
     &       KDLONS,KPF   ,KPS   ,                                      &
     &       KXOBSV,KXOBSW,KXEDSW,                                      &
     &       KXSIGW,KPSF  ,KDNUM ,KCNUM ,KFCOR ,KCOSAR,KSINAR,KSABIA,   &
     &       KSBTM1,KSBTM2,KYAWBS,KVLOUV,KACMAG,KOBSTR,                 &
     &       KPRL1 ,KPRL2, KRL1  ,KT1SE ,KTCP  ,                        &
     &       KRATDR,KFQT1S,KFQT1E,KT1STR,KFTTSE,KFRSTR,KFRRAT,KSV1  ,   &
     &       KSV2  ,KTSLOV,                                             &
     &       KGLGR1,KGLGR2,KGLFR1,KGLFR2,                               &
     &       KARGR1,KARGR2,KARFR1,KARFR2,                               &
     &       KRDS1L,KFT1AV,KDFQP ,KFREQ1,KFREQ3,KSAVD1,KSAVD2,          &
     &       KANTOU,KFM3CF,KF2CF,KTMG,KLTMG,KX2TIM,KX2OBS,KXRNDX,KX2SCR,&
     &       KALTWV,KXXBM ,KX2PAR,KATIME,KPMPAT,KPMATT,KX2PAT,          &
     &       KPXEXI,KPXEPA,KPV,KPXEP2,KX2COF,KACOF2,KACOF3,KBCOF,KBCOF2,&
     &       KDDDA ,KX2AUX,KX2VPR,KX2VPA,KEXTRA,KVARAY,KATROT,KATPER,   &
     &       KLTAR ,KXHOLD,KANTBL,KPHC  ,KOFDRV,KGNAME,KGRSIZ,KGRCNT,   &
     &       KGRDAT,KACCDT,KCTBTI,KCTBWE,KCTCTM,KANTUV,KANTOR,KW1PU ,   &
     &       KPYSQH,KSIGSP,KXYOF3,KXVTM1,KXDIST,KXDST0,KTMSE ,KDSDP ,   &
     &       KEXCST,KEXCDT,KEXCGX,KEXCGY,KEXCGZ,KIMNDX,KIMOBS,KIMTIM,   &
     &       KIMSAT,KM2VPA,KDCDD ,KDWNWT,KDPOR ,KC2PAR,KBOUNC,KBPART,   &
     &       NXCA04
      COMMON/CORI04/KNMP  ,KINDP ,KNDPAR,KNPVCT,KISATN,KISET ,KIANTO,   &
     &              KISATO,KIANTD,KISATD,KISTAD,KISATC,KMJDCG,KISTAT,   &
     &              KMJDEB,KMJDBN,KNDBIN,KNWTDB,KIXPAR,KNDBUF,KSSTNM,   &
     &              KSSTNA,KMBSAT,KMBSTA,KXKEY ,KXVKEY,KXFLAG,KIPNTF,   &
     &              KIPNTS,KNCON ,KKF   ,KIDATB,KNSTLV,KSLVID,KLLBIA,   &
     &              KTMRA1,KTMRA2,KIND1 ,KTARID,KATARD,KYAWID,KXOBLK,   &
     &              KDSCWV,KATRSQ,KATSAT,KKVLAS,KPARTP,KLTMSC,KLTASC,   &
     &              KCTBST,KSTBST,KANCUT,KANGPS,KANTYP,                 &
     &              KANTOF,                                             &
     &              KYSAT, KMBDEG,                                      &
     &              KMBNOD,KMBSST,KBSPLN,KSSPLN,KXIOBS,KIDLAS,KIAVP ,   &
     &              KXNAVP,KNEXCG,KIDEXC,KNREXC,KTELEO,KIKEY ,KIMKEY,   &
     &              KIMBLK,KIMPRT,KCN110,KCN111,KC110,KC111,KTDSAT,     &
     &              KTDANT,NXCI04
      COMMON/CREFMT/REFMT(9)
      COMMON/DELOFF/NOFFST,NSATDL,NSTADL,NCGMAS,NXDELO
      COMMON/EXATGI/MEASAT,MEAFIL,MEAMEM,MEAPLA,MEAANT,NXEAGI
      DIMENSION AA(1), II(1)
      DIMENSION XYZCGB(1), XYZCGI(3)
      DIMENSION ISATCG(1), MJDSCG(1)
      DIMENSION REAQAT(MEAMEM,4)
!.... Local variables
      DIMENSION XYZCG(3),SBFROT(3,3), SBFQAT(4)
      DIMENSION ROT(3,3)
!
!.... Initialize with zeroes
      XYZCG(1)=0.D0
      XYZCG(2)=0.D0
      XYZCG(3)=0.D0
!
      IF(LEXTQ) THEN
!........ Find the index for the satellite in the cgmass array
          INDX=0
          DO I=1,NCGMAS
              IF(ISATCG(I)==ISATID.AND.(MJDSEC+FSEC)<MJDSCG(I)) THEN
                  INDX=I
                  EXIT
              ENDIF
          ENDDO
          IF(INDX > 0) THEN
!............ Load the CoM coordinates from CGMASS card
              INDX=INDX-1
              XYZCG(1)=XYZCG(1)+XYZCGB(INDX*3  )
              XYZCG(2)=XYZCG(2)+XYZCGB(INDX*3+1)
              XYZCG(3)=XYZCG(3)+XYZCGB(INDX*3+2)
          ENDIF
!........ Check if external CGMASS file is available for this satellite
          CALL FNDNUM(ISATID,II(KIDEXC),II(KNEXCG),INDX)
          IF(INDX > 0) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ADD CGMASS COORDINATES
! NOTE THERE ARE TWO KINDS OF CGMASS COORDINATES. THE FIRST ONE
! IS THE CGMASS COOR SPECIFIED IN CGMASS CARD. THE OTHER ONE
! IS THE TIME DEPENDENT CGMASS COOR FROM EXTERNAL FILE. THE
! FIRST COOR HAS ALREADY BEEN APPLIED. HERE ALL ADD THE TIME
! DEPENDENT CGMASS COOR READ FROM EXTERNAL FILE.
! THE FOLLOWING IS DONE ONLY WHEN EXTERNAL CGMASS COORDINATES
! IS REQUIRED FOR THIS SATELLITE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!............ Compute the pointers for this satellite
              IPTCGX=KEXCGX
              IPTCGY=KEXCGY
              IPTCGZ=KEXCGZ
              DO I=1,INDX-1
                  IPTCGX=IPTCGX+II(KNREXC+I-1)
                  IPTCGY=IPTCGY+II(KNREXC+I-1)
                  IPTCGZ=IPTCGZ+II(KNREXC+I-1)
              ENDDO
!............ Linear interpolation
              TCGDIF=MJDSEC+FSEC-AA(KEXCST+INDX-1)
              INDCG=INT(TCGDIF/AA(KEXCDT+INDX-1))
              TCGDIF=MOD(TCGDIF,AA(KEXCDT+INDX-1))
              TCGDIF=TCGDIF/AA(KEXCDT+INDX-1)
!
              ECGCRX=AA(IPTCGX+INDCG)+(AA(IPTCGX+INDCG+1)-   &
     &                           AA(IPTCGX+INDCG))*TCGDIF
              ECGCRY=AA(IPTCGY+INDCG)+(AA(IPTCGY+INDCG+1)-   &
     &                           AA(IPTCGY+INDCG))*TCGDIF
              ECGCRZ=AA(IPTCGZ+INDCG)+(AA(IPTCGZ+INDCG+1)-   &
     &                           AA(IPTCGZ+INDCG))*TCGDIF
              XYZCG(1) = XYZCG(1) + ECGCRX
              XYZCG(2) = XYZCG(2) + ECGCRX
              XYZCG(3) = XYZCG(3) + ECGCRX
          ENDIF
!
!........ Compute time difference between current time and start of
!........ quarternion set
          EADIFF=DBLE(MJDSEC-MJDSEA)
          EADIFF=EADIFF+FSEC-FSSCEA
          IF(EADIFF < 0.D0) THEN
              WRITE(6,*) 'ABNORMAL TERMINATION IN XCGB2I'
              WRITE(6,*) 'INTERPOLATION TIME IS EARLIER THAN THE ',    &
     &                   'START OF QUATERNION'
              STOP
          ENDIF
!........ Compute pointer to quaternion preceding the current time
          IPNT1=INT(EADIFF/RINTEA)+1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     print*,mjdsea+fsscea+(ipnt1-1)*rintea,mjdsec+fsec, &
!    &       mjdsea+fsscea+(ipnt1-0)*rintea
!         print*,reaqat(1,1),reaqat(1,2),reaqat(1,3),reaqat(1,4)
!         print*,reaqat(2,1),reaqat(2,2),reaqat(2,3),reaqat(2,4)
!         print*,reaqat(3,1),reaqat(3,2),reaqat(3,3),reaqat(3,4)
!         print*,reaqat(4,1),reaqat(4,2),reaqat(4,3),reaqat(4,4)
!         print*,reaqat(5,1),reaqat(5,2),reaqat(5,3),reaqat(5,4)
!         print*,isbj20
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!........ IQPT1 is calculated from isbj20+ipnt1 by adding 1 not minus 1 because
!........ the first two records of reaqat are headers.
          IQPT1=ISBJ20+IPNT1+1
          IQPT2=IQPT1+1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         print*,'r1:',(reaqat(1,kk),kk=1,4)
!         print*,'r2:',(reaqat(2,kk),kk=1,4)
!         print*,'r3:',(reaqat(3,kk),kk=1,4)
!         print*,'d1:',(reaqat(iqpt1,kk),kk=1,4)
!         print*,'d2:',(reaqat(iqpt1+1,kk),kk=1,4)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!........ Check the quaternion boundaries
          TMP=REAQAT(IQPT1,1)-0.1D0
          ITMP1=TMP
          TMP=REAQAT(IQPT2,1)-0.1D0
          ITMP2=TMP
          IF((ITMP1.EQ.-9999999).OR.(ITMP2.EQ.-9999999)) THEN
              WRITE(6,*) 'ONE OF THE QUATERNION BOUNDARIES FOR ',       &
     &           'INTERPOLATION IS MARKED AS 9999999.9 IN XCGB2I.'
              STOP
          ENDIF
!........ Do the linear interpolation here
          DT=DBLE(MJDSEC-MJDSEA)
          DT=DT+FSEC-FSSCEA-DBLE(IPNT1-1)*RINTEA
          DT=DT/RINTEA
          DO I=1,4
              SBFQAT(I)=REAQAT(IQPT1,I)+DT*(REAQAT(IQPT2,I)-            &
     &                  REAQAT(IQPT1,I))
          ENDDO
!........ test to see if quaternion magnitude is close to one
          QMAG=SBFQAT(1)*SBFQAT(1)+SBFQAT(2)*SBFQAT(2)+                 &
     &         SBFQAT(3)*SBFQAT(3)+SBFQAT(4)*SBFQAT(4)
          QDIF=ABS(QMAG-1.D0)
          IF(QDIF.GT.1.D-3) THEN
          WRITE(6,97000) REAQAT(IQPT1,1),REAQAT(IQPT1,2),               &
     &                   REAQAT(IQPT1,3),REAQAT(IQPT1,4),               &
     &                   REAQAT(IQPT2,1),REAQAT(IQPT2,2),               &
     &                   REAQAT(IQPT2,3),REAQAT(IQPT2,4),               &
     &                   SBFQAT(1),SBFQAT(2),SBFQAT(3),SBFQAT(4),       &
     &                   QMAG
97000 FORMAT(1X,'WARNING FROM XCGB2I: INTERPOLATED BODY-FIXED',         &
     &          ' QUATERNION MAGNITUDE',/,' IS DIFFERENT FROM ',        &
     &          ' ONE BY AT LEAST .001',/,' QUATERNION FOR ',           &
     &          ' INTERPOLATION BOUNDARY 1 IS: ',/,4D24.16,/,           &
     &          ' QUATERNION FOR INTERPOLATION BOUNDARY 2 IS: ',        &
     &          /,4D24.16,/,'THE INTERPOLATED QUATERNION IS: ',         &
     &          /,4D24.16,/,'THE MAGNITUDE IS: ',D24.16,/,              &
     &          'THE QUATERNION WILL BE NORMALIZED')
          ENDIF
!........ Normalize
          QMAG=SQRT(QMAG)
          DO I=1,4
              SBFQAT(I)=SBFQAT(I)/QMAG
          ENDDO
          CALL QATROT(SBFQAT,ROT)
!........ SBFROT is from SBF to mj2000, REFMT is from mj2000 to tor.
!........ We need to multiply these two matrixes.
          DO I=1,3
              SBFROT(1,I) = REFMT(1)*ROT(1,I) +                         &
     &                      REFMT(2)*ROT(2,I) +                         &
     &                      REFMT(3)*ROT(3,I)
              SBFROT(2,I) = REFMT(4)*ROT(1,I) +                         &
     &                      REFMT(5)*ROT(2,I) +                         &
     &                      REFMT(6)*ROT(3,I)
              SBFROT(3,I) = REFMT(7)*ROT(1,I) +                         &
     &                      REFMT(8)*ROT(2,I) +                         &
     &                      REFMT(9)*ROT(3,I)
          ENDDO
          DO I=1,3
              XYZCGI(I) = SBFROT(I,1)*XYZCG(1) + SBFROT(I,2)*XYZCG(2) + &
     &                    SBFROT(I,3)*XYZCG(3)
          ENDDO
!           print*, xyzcg
!           print*, xyzcgi
      ELSE
!....... Currently internal model is not implemented here
!....... Print error message and stop iie if this happens
        WRITE(6,*) 'EXTERNAL QUATERNIONS REQUIRED IN XCGB2I!'
        WRITE(6,*) 'PLEASE CONTACT GEODYN PROGRAMMERS IF YOU WANT TO ', &
     &             'USE INTERNAL ATTITUDE MODEL AND WANT OUTPUT IN ',   &
     &             'CENTER OF FIGURE FRAME '
        STOP
      ENDIF

      RETURN
      END SUBROUTINE XCGB2I
