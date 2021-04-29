!$INTROR
      SUBROUTINE INTROR(MJDSM,FSECM,S,B,NM,CIPV,CIVV,IORDER,H,INDH,NMH, &
     &    NINTVL,SUMXR,XRDDOT,NH,NSTEPS,XI,MAXM,ISQ,NAST)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      COMMON/XPCNT /NXTRAC,NXFLAG,NXALLC,NCPPER,NMINRT,NDPOR,NXXTRA
      DIMENSION S(NM),B(NM),CIPV(NM,IORDER),CIVV(NM,IORDER),            &
     &          INDH(NINTVL),NMH(NINTVL),SUMXR(NDPOR,2,NH,NAST,*)
!...Begin TJS
!...  DIMENSION XRDDOT(156,NSTEPS),XI(MAXM,156)
      DIMENSION XRDDOT(NDPOR,NSTEPS,NAST,*),XI(MAXM,NDPOR,NAST),        &
     &          FSECM(NM)
!...End TJS

!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      CALL COEFV(S,B,NM,IORDER,CIPV,CIVV)
!
      IOL2=IORDER-2
      IOL1=IOL2+1
      DO 1000 IAST=1,NAST
      NM1=1
      DO INTVL=1,NINTVL
         NM2=NMH(INTVL)
         INDEX=INDH(INTVL)
!
! (IOL1-1+INDEX) is the most advanced integration time associated with
!  this measurement.  Subtract 3 from this and you have the integration
!  step which preceeds the measurment.
!
!...Begin TJS
         DO N=1,NDPOR
!...End TJS
!
            DO NMI=NM1,NM2
               B(NMI)=SUMXR(N,2,INDEX,IAST,ISQ)
            END DO
!
            DO K=1,IOL1
               KK=IOL1-K+INDEX
!
               DO NMI=NM1,NM2
                  B(NMI)=B(NMI)+CIVV(NMI,K)*XRDDOT(N,KK,IAST,ISQ)
               END DO
            END DO
!
            DO NMI=NM1,NM2
               XI(NMI,N,IAST)=B(NMI)*H
            END DO
!
         END DO
!...Begin TJS DEBUG
!        IONE=1
!        DO NMI=NM1,NM2
!           RMJD=(DBLE(MJDSM)+FSECM(NMI))/86400.D0
!           WRITE(69,9258) IONE,RMJD,(XI(NMI,J),J=1,NDPOR)
!        END DO
!9258    FORMAT(1X,I1,229(1X,E22.15))
!...End TJS DEBUG
         NM1=NM2+1
      END DO
 1000 CONTINUE
!
      RETURN
      END
