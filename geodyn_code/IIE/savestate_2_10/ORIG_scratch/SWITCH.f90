!$SWITCH
      SUBROUTINE SWITCH(PMPA,NDIM1,NDIM2,LNPNM)
!SDOC*******************************************************************
!
!   PURPOSE:    CONVERT THE MEASUREMENT PARTIALS OF THE STANDARD
!               CARTESIAN INITIAL STATE VECTOR PARAMTERS OF TWO
!               SATELLITES INTO MEASURMENT PARTIALS FOR SPHERICAL
!               BASELINE PARAMTERS.
!
!   ARGUMENTS:  PMPA   - FULL ARRAY OF MEASUREMENT PARTIALS FOR
!                        A BLOCK OF OBSERVATIONS. CONTAINS PARTIALS
!                        FOR ALL ADJUSTING PARAMETERS AND ALL
!                        MEASUREMENTS IN THE BLOCK. UPON INPUT PMPA
!                        HAS PARTIALS FOR STANDARD CARTESIAN INITIAL
!                        STATE VECTOR PARAMETERS. ON OUTPUT IT CONTAINS
!                        PARTIALS FOR MIDPOINT AND BASELINE PARAMETERS
!               NDIM1  - FIRST DIMENSION OF PMPA
!               NDIM2  - SECOND DIMENSION OF PMPA
!               LNPNM  - TRUE IF NDIM1=NUMBER OF ADJUSTING PARAMTERS
!
!   NOTE                 IN ITS PRESENT FORM THIS ROITINE ASSUMES
!                        THAT THE TWO SATELLITES FORMING THE BASELINE
!                        ARE SATS #1 AND # 2
!
!
!EDOC*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      COMMON/BIAOR/NAMBB,NCYCB,NAMBBH
      COMMON/BSPHRE/DXMDN(3,3,2),DXBDN(3,3,2)
      DIMENSION PMPA(NDIM1,NDIM2)
      DIMENSION PSC(6),PSS(12)
      DATA HALF/.5D0/
      IF(LNPNM) THEN
        DO J=1,NDIM2
!
          PSC(1)=PMPA(7+NAMBB,J)+PMPA(1+NAMBB,J)
          PSC(2)=PMPA(8+NAMBB,J)+PMPA(2+NAMBB,J)
          PSC(3)=PMPA(9+NAMBB,J)+PMPA(3+NAMBB,J)
          PSC(4)=PMPA(10+NAMBB,J)+PMPA(4+NAMBB,J)
          PSC(5)=PMPA(11+NAMBB,J)+PMPA(5+NAMBB,J)
          PSC(6)=PMPA(12+NAMBB,J)+PMPA(6+NAMBB,J)
!
          PSS(1)=PSC(1)*DXMDN(1,1,1)+PSC(2)*DXMDN(1,2,1)                &
     &          +PSC(3)*DXMDN(1,3,1)
          PSS(2)=PSC(1)*DXMDN(2,1,1)+PSC(2)*DXMDN(2,2,1)                &
     &          +PSC(3)*DXMDN(2,3,1)
          PSS(3)=PSC(1)*DXMDN(3,1,1)+PSC(2)*DXMDN(3,2,1)                &
     &          +PSC(3)*DXMDN(3,3,1)
          PSS(4)=PSC(4)*DXMDN(1,1,2)+PSC(5)*DXMDN(1,2,2)                &
     &           +PSC(6)*DXMDN(1,3,2)
          PSS(5)=PSC(4)*DXMDN(2,1,2)+PSC(5)*DXMDN(2,2,2)                &
     &           +PSC(6)*DXMDN(2,3,2)
          PSS(6)=PSC(4)*DXMDN(3,1,2)+PSC(5)*DXMDN(3,2,2)                &
     &           +PSC(6)*DXMDN(3,3,2)
!
          PSC(1)=HALF*(PMPA(7+NAMBB,J)-PMPA(1+NAMBB,J))
          PSC(2)=HALF*(PMPA(8+NAMBB,J)-PMPA(2+NAMBB,J))
          PSC(3)=HALF*(PMPA(9+NAMBB,J)-PMPA(3+NAMBB,J))
          PSC(4)=HALF*(PMPA(10+NAMBB,J)-PMPA(4+NAMBB,J))
          PSC(5)=HALF*(PMPA(11+NAMBB,J)-PMPA(5+NAMBB,J))
          PSC(6)=HALF*(PMPA(12+NAMBB,J)-PMPA(6+NAMBB,J))
!
          PSS(7) =PSC(1)*DXBDN(1,1,1)+PSC(2)*DXBDN(1,2,1)               &
     &           +PSC(3)*DXBDN(1,3,1)
          PSS(8) =PSC(1)*DXBDN(2,1,1)+PSC(2)*DXBDN(2,2,1)               &
     &           +PSC(3)*DXBDN(2,3,1)
          PSS(9) =PSC(1)*DXBDN(3,1,1)+PSC(2)*DXBDN(3,2,1)               &
     &           +PSC(3)*DXBDN(3,3,1)
          PSS(10)=PSC(4)*DXBDN(1,1,2)+PSC(5)*DXBDN(1,2,2)               &
     &           +PSC(6)*DXBDN(1,3,2)
          PSS(11)=PSC(4)*DXBDN(2,1,2)+PSC(5)*DXBDN(2,2,2)               &
     &           +PSC(6)*DXBDN(2,3,2)
          PSS(12)=PSC(4)*DXBDN(3,1,2)+PSC(5)*DXBDN(3,2,2)               &
     &           +PSC(6)*DXBDN(3,3,2)
!
          DO I=1,12
            PMPA(I+NAMBB,J)=PSS(I)
          ENDDO
        ENDDO
      ELSE
        DO J=1,NDIM1
!
          PSC(1)=PMPA(J,7+NAMBB)+PMPA(J,1+NAMBB)
          PSC(2)=PMPA(J,8+NAMBB)+PMPA(J,2+NAMBB)
          PSC(3)=PMPA(J,9+NAMBB)+PMPA(J,3+NAMBB)
          PSC(4)=PMPA(J,10+NAMBB)+PMPA(J,4+NAMBB)
          PSC(5)=PMPA(J,11+NAMBB)+PMPA(J,5+NAMBB)
          PSC(6)=PMPA(J,12+NAMBB)+PMPA(J,6+NAMBB)
!
          PSS(1)=PSC(1)*DXMDN(1,1,1)+PSC(2)*DXMDN(1,2,1)                &
     &          +PSC(3)*DXMDN(1,3,1)
          PSS(2)=PSC(1)*DXMDN(2,1,1)+PSC(2)*DXMDN(2,2,1)                &
     &          +PSC(3)*DXMDN(2,3,1)
          PSS(3)=PSC(1)*DXMDN(3,1,1)+PSC(2)*DXMDN(3,2,1)                &
     &          +PSC(3)*DXMDN(3,3,1)
          PSS(4)=PSC(4)*DXMDN(1,1,2)+PSC(5)*DXMDN(1,2,2)                &
     &           +PSC(6)*DXMDN(1,3,2)
          PSS(5)=PSC(4)*DXMDN(2,1,2)+PSC(5)*DXMDN(2,2,2)                &
     &           +PSC(6)*DXMDN(2,3,2)
          PSS(6)=PSC(4)*DXMDN(3,1,2)+PSC(5)*DXMDN(3,2,2)                &
     &           +PSC(6)*DXMDN(3,3,2)
!
          PSC(1)=HALF*(PMPA(J,7+NAMBB)-PMPA(J,1+NAMBB))
          PSC(2)=HALF*(PMPA(J,8+NAMBB)-PMPA(J,2+NAMBB))
          PSC(3)=HALF*(PMPA(J,9+NAMBB)-PMPA(J,3+NAMBB))
          PSC(4)=HALF*(PMPA(J,10+NAMBB)-PMPA(J,4+NAMBB))
          PSC(5)=HALF*(PMPA(J,11+NAMBB)-PMPA(J,5+NAMBB))
          PSC(6)=HALF*(PMPA(J,12+NAMBB)-PMPA(J,6+NAMBB))
!
          PSS(7) =PSC(1)*DXBDN(1,1,1)+PSC(2)*DXBDN(1,2,1)               &
     &           +PSC(3)*DXBDN(1,3,1)
          PSS(8) =PSC(1)*DXBDN(2,1,1)+PSC(2)*DXBDN(2,2,1)               &
     &           +PSC(3)*DXBDN(2,3,1)
          PSS(9) =PSC(1)*DXBDN(3,1,1)+PSC(2)*DXBDN(3,2,1)               &
     &           +PSC(3)*DXBDN(3,3,1)
          PSS(10)=PSC(4)*DXBDN(1,1,2)+PSC(5)*DXBDN(1,2,2)               &
     &           +PSC(6)*DXBDN(1,3,2)
          PSS(11)=PSC(4)*DXBDN(2,1,2)+PSC(5)*DXBDN(2,2,2)               &
     &           +PSC(6)*DXBDN(2,3,2)
          PSS(12)=PSC(4)*DXBDN(3,1,2)+PSC(5)*DXBDN(3,2,2)               &
     &           +PSC(6)*DXBDN(3,3,2)
!
          DO I=1,12
            PMPA(J,I+NAMBB)=PSS(I)
          ENDDO
        ENDDO
      ENDIF
      RETURN
      END
