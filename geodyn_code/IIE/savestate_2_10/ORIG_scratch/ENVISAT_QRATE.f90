
!$ ENVISAT_QRATE
      SUBROUTINE ENVISAT_QRATE(x_sysm_n,y_sysm_n,z_sysm_n,busQRATE,MJD)
      IMPLICIT NONE
      DOUBLE PRECISION x_sysm_n(0:2),y_sysm_n(0:2),z_sysm_n(0:2)
      DOUBLE PRECISION BOL(0:2),EOL(0:2),delta(0:2)
      DOUBLE PRECISION qrate(0:2),busQRATE(0:2)
      DOUBLE PRECISION startMJD,endMJD
      DOUBLE PRECISION MJD,secs_in_5_years,t,fraction
      PARAMETER(startMJD=52334.047222222D0)!start of qrate period
      PARAMETER(endMJD=54160.047222222D0)!end of qrate period
      PARAMETER(secs_in_5_years=5.D0*365.25D0*86400.0D0)
!     BFS output, beginning of life
      DATA BOL/6.0367D-011,-9.00979D-011,-1.33039D-010/
!     BFS output, end of life
      DATA EOL/5.89249D-011,-1.10155D-010,-1.76266D-010/
!     Accelerations in the BFS due to Bus heat venting, in m/s^2,
!     using a s/c mass of 8040.0Kg.
!     These have been multiplied by -1.0, as originally they represent
!     the net power output vector scaled to take account of the s/c mass,
!     but the recoil acceleration due to this force will actually be in
!     the opposite direction
!     delta=EOL-BOL
      DATA delta /-1.44210D-12,-2.00571D-11,-4.32270D-11/

!     normally: qrate = BOL + fraction*delta (where t=5 years in secs/elapsed
!     time in secs since BOL)
!     if current epoch is prior to BOL, set qrate=BOL
!     if current epoch is later than EOL, set qrate=EOL
      IF(MJD.LT.startMJD)THEN
          qrate(0)=BOL(0)
          qrate(1)=BOL(1)
          qrate(2)=BOL(2)
      ELSEIF(MJD.GT.endMJD)THEN
          qrate(0)=EOL(0)
          qrate(1)=EOL(1)
          qrate(2)=EOL(2)
      ELSE
          t=(MJD-startMJD)*86400.0D0
          fraction=t/secs_in_5_years
          qrate(0)=BOL(0)+fraction*delta(0)
          qrate(1)=BOL(1)+fraction*delta(1)
          qrate(2)=BOL(2)+fraction*delta(2)
      ENDIF

!     'project' qrate onto ECI_TOD BFS axes, as qrate is a vector in BFS.
!     Scale the ECI BFS vectors by the qrate and sum the X,Y,Z ECI_TOD
!     contributions from each qrate scaled BFS axis
          busQRATE(0)=qrate(0)*x_sysm_n(0)+qrate(0)*y_sysm_n(0)+   &
     &     qrate(0)*z_sysm_n(0)
          busQRATE(1)=qrate(1)*x_sysm_n(1)+qrate(1)*y_sysm_n(1)+   &
     &     qrate(1)*z_sysm_n(1)
          busQRATE(2)=qrate(2)*x_sysm_n(2)+qrate(2)*y_sysm_n(2)+   &
     &     qrate(2)*z_sysm_n(2)
!     busQRATE now gives final ms^-2 accelerations in ECI_TOD!

      RETURN
      END
