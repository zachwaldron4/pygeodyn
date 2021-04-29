!$EXTIME
      SUBROUTINE EXTIME(IYIMID,IHIMIS,IY,IM,ID,IH,IMIN,ISEC)
!*********************************************************************SA
!                                              D. Pavlis
!
!   FUNCTION         EXTRACT IY IM ID IH IMIN ISEC FROM IYMD IHMS
!
!   INPUT PARAMETER  IYIMID - REFERENCE DATE
!                    IHIMIS - REFERENCE TIME
!
!   OUTPUT PARAMETER SEC - IY  IM  ID  IH  IMIN  ISEC
!
!   RESTRICTIONS     the DATE is interpreted as follows:
!                        if IY <  50, the year is 2000-2049
!                        if IY >= 50, the year is 1950-1999
!
!                    only works for years 1950 - 2049
!
!********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), LOGICAL(L)
      SAVE
!
!
!**********************************************************************
! START OF EXECUTABLE CODE ********************************************
!**********************************************************************
!
!      write(6,*) 'ymdtis: IYIMID,IHIMIS ', IYIMID,IHIMIS
!
      IY = MOD( IYIMID/10000, 100 )
!      write(6,*) 'ymdtis: IY',IY
      IM=MOD(IYIMID,10000)
!      write(6,*) 'ymdtis: IM',IM
      IM=IM/100
!        write(6,*)' dbg 1 EXTIME ', IM
      ID=MOD(IYIMID,100)
      IH=IHIMIS/10000
      IMIN=IHIMIS-IH*10000
      IMIN=IMIN/100
      ISEC=MOD(IHIMIS,100)
      RETURN
      END
