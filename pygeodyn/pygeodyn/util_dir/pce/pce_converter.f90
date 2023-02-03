!********1*********2*********3*********4*********5*********6*********7**
! pce_converter.f90     Jan 2023 - Modified by Zach Waldron
!                                - Original author assumed to be D. Rowlands
!
! FUNCTION:    (1)  Constructs the PCE data in the G2B Format for use as 
!                   tracking data in GEODYN.
! 
! PARAMETERS:   
!
!    Name             I/O Method      Description of Parameters
!   ---------------  ------------    --------------------------------
!   SATID_char        environ var
!   PATH_UTIL_PCE     environ var
!   PATH_pcemake_in   environ var
!   PATH_pcemake_out  environ var
!   TRAJ.txt          read text file
!
! LOCALS:
!     MJDS       Time in integral seconds from GEODYN Ref. Time
!      FSEC      Fractional remaining seconds
!      FSECX
!      IYMD
!      IHM
!      SEC
!      NLOG
!      IPTO
!      BFOUT
!      PRE9
!      PRE8
!      VLIGHT
!      WRD5
!      WRD8
!      SIGMA
!
! COMMENTS:
!   (1) Compile this program so that integers have 8 BYTES (64 BITS)
!
!   (2) Write the prepared inputs to a txt file to be read by fortran code
!         TRAJ.txt for input to PCE_converter.f
!         Requirements:
!          - File titled TRAJ.txt
!          - Each line of TRAJ.txt has 1 integer and 4 float points values 
!              Col #     Fmt        Description    
!              ------  --------     -----------
!                 1     int(64)     MJD seconds in GPS system
!                 2    float(64)    fractional seconds + GPS_offset_secs_utc
!                 3    float(64)    X   (eci-j2000)
!                 4    float(64)    Y   (eci-j2000)
!                 5    float(64)    Z   (eci-j2000)
!
!       The integer and the first floating point word form the time tag of  
!       the record.
!       The first float is the precise time/correction for time tag
!       The last three words are the X, Y and Z coordinates of the satellite
!            in the J2000 coordinate system.

!********1*********2*********3*********4*********5*********6*********7**
!
! 
      IMPLICIT REAL*8 (A-H,O-Z), LOGICAL (L)                                    
!   
      PARAMETER(MAXBLK=100)
!
      DIMENSION MJDS(MAXBLK),FSEC(MAXBLK),X(MAXBLK,3)
      DIMENSION BFOUT(200,10)                                                  
      DIMENSION BUFW(10)
      DIMENSION FSECX(1),IYMD(1),IHM(1),SEC(1)
      DATA NLOG/0/
      DATA IPTO/0/
      DATA BFOUT/2000*0.D0/
      DATA PRE9/16777214.D0/
      DATA PRE8/16777215.D0/
      DATA VLIGHT/299792458.D0/
      DATA WRD5/0.000003D0/
      DATA WRD8/0.00023D0/
      DATA SIGMA/1.D0/
!      
      CHARACTER(len=7) :: SATID_char
      REAL(8) :: SATID
      integer(8) :: i_SATID
      CHARACTER(len=255) :: PATH_UTIL_PCE
      CHARACTER(len=255) :: PATH_pcemake_in
      CHARACTER(len=255) :: PATH_pcemake_out
!
! Stage 1: Prep ---------------------------------------------------------------
!
      CALL get_environment_variable("in_SATID", SATID_char)    
      read(SATID_char,'(I7)') i_SATID
      ! i_SATID = ICHAR()
      SATID = REAL(i_SATID)  
      write(6,*) "i_SATID =    ", i_SATID
      write(6,*) "SATID_char = ", SATID_char
      write(6,*) "SATID =      ", SATID


! Import Options via environment variables
      CALL get_environment_variable("PATH_UTIL_PCE", PATH_UTIL_PCE)      
      CALL get_environment_variable("PATH_pcemake_in", PATH_pcemake_in)      
      CALL get_environment_variable("PATH_pcemake_out", PATH_pcemake_out)      

      write(6,*) "input_file = ", trim(PATH_pcemake_in)
      write(6,*) "output file = ", trim(PATH_pcemake_out)

      ! if the input data exists, open it into Unit 10
      INQUIRE(EXIST=LEXIST,FILE=trim(PATH_pcemake_in))
      IF(LEXIST) THEN
         ! csv data must be opened as formatted file.
         OPEN(UNIT=10,FILE=trim(PATH_pcemake_in),FORM='FORMATTED',STATUS='OLD')
      ELSE
         WRITE(6,*) 'Error in pce_converter.f-- file not found:'
         WRITE(6,*) '   file not found:',trim(PATH_pcemake_in)
         STOP
      ENDIF
       
! Remove any already constructed output PCE datasets before running this 
      OPEN(UNIT=40,FILE=trim(PATH_pcemake_out),FORM='UNFORMATTED',STATUS='NEW')
      

 1000 CONTINUE    
! Stage 2: Read data loaded at Unit 10-----------------------------------------
!          Read in a chunk og MAXBLK points from the file loaded at unit 10
!          Load the data into HOLDING ARRAYS
!            MAXBLK is the block size for the G2B format
      NM=0
      LEND=.FALSE.
      DO 1100 I=1,MAXBLK
      READ(10,*,END=1200) MJDS(I),FSEC(I),X(I,1),X(I,2),X(I,3)
      NM=NM+1
 1100 CONTINUE                                                                  
 1200 CONTINUE                                                                  
      IF(NM.LT.MAXBLK) LEND=.TRUE.
      IF(NM.EQ.0) GO TO 3000
!
!  Save the loaded chunk of MAXOB points into G2B format
!     Do this for X, Y and Z
      DO 2000 IMEAS=1,3
      if(IMEAS.eq.1)then  
      !    WRITE(6,*) 'MJDS -',DFLOAT(MJDS(1))
      !    WRITE(6,*) 'FSEC -',FSEC(1)
      !    WRITE(6,*) 'mjds calc -',DFLOAT(MJDS(NM)-MJDS(1))+FSEC(NM)-FSEC(1)
      endif
      BUFW(1)=DFLOAT(MJDS(1))
      BUFW(2)=FSEC(1)
      BUFW(3)=DFLOAT(MJDS(NM)-MJDS(1))+FSEC(NM)-FSEC(1)
      BUFW(4)=VLIGHT
      BUFW(5)=WRD5+DFLOAT(IMEAS)
      BUFW(7)=DFLOAT(NM)
      BUFW(8)=WRD8   ! auxillary records
      BUFW(9)=PRE9
      BUFW(10)=-9000000.D0
!
! Stage 3: Save loaded arrays to Unit 40 --------------------------------------
!
      CALL BUFOUT(IPTO,BUFW,BFOUT)
!
      DO K=1,10
       BUFW(K)=0.D0
      ENDDO
      BUFW(5)=2000.D0
      BUFW(8)=SATID
      BUFW(9)=PRE8
      BUFW(10)=-8000000.D0
      CALL BUFOUT(IPTO,BUFW,BFOUT)
!
      DO 1400 I=1,NM
      DO K=1,10
       BUFW(K)=0.D0
      ENDDO
      if(I.eq.1)then  
!          WRITE(6,*) 'X -',X(I,IMEAS)
      endif
      BUFW(1)=X(I,IMEAS)
      BUFW(6)=DFLOAT(MJDS(I)-MJDS(1))+FSEC(I)-FSEC(1)
      BUFW(7)=SIGMA
      
      CALL BUFOUT(IPTO,BUFW,BFOUT)
 1400 CONTINUE
!
!
 2000 CONTINUE
      IF(.NOT.LEND) GO TO 1000                                                                
!
!
!
 3000 CONTINUE                                                                  
!

      IF(IPTO.GT.0) WRITE(40) BFOUT
      CLOSE(40)

      STOP
      END
      
      
      SUBROUTINE BUFOUT(IPTO,BUFI,BUFO)
      IMPLICIT REAL*8(A-H,O-Z),LOGICAL(L)
      DIMENSION BUFI(10)
      DIMENSION BUFO(200,10)
      DATA NBLKO/0/
!
      DO 100 I=1,1
      IPTO=IPTO+1
      IF(IPTO.EQ.201) THEN
        NBLKO=NBLKO+1
        WRITE(6,78912) NBLKO
!         WRITE(6,*) ' ----------------------- '
78912   FORMAT(' BLOCK # ',I6' WRITTEN')
        WRITE(40) BUFO
!        WRITE(41,*) BUFO

        DO J=1,2000
          BUFO(J,1)=0.D0
        ENDDO
        IPTO=1
      ENDIF
      DO J=1,10
        BUFO(IPTO,J)=BUFI(J)
      ENDDO
 100  CONTINUE
      RETURN
      END
      


