!     Code transformed by the GEODYN code Pre-Processor(
!$getdate.option
      PROGRAM getdate
!***********************************************************************
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      COMMON/DUMMY/DUMMY
!$GPP END COMMONS
      character*3 month
      character*2 day
      character*2 year
      OPEN(9,FILE='ftn9',FORM='FORMATTED',STATUS='OLD')
      OPEN(10,FILE='ftn10',FORM='FORMATTED',STATUS='NEW')
      read(9,1000)month,day,year
      if(day(1:1).eq.' ')day(1:1)='0'
      write(10,2000)month,day,year
      write(6,2100)month,day,year
1000  format(4x,a3,1x,a2,16x,a2)
2000  format('TITLE',14x,a3,a2,a2)
2100  format(' getdate : TITLE',14x,a3,a2,a2)
9999  CONTINUE
      END
