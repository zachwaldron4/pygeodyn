C   
      IMPLICIT REAL*8 (A-H,O-Z), LOGICAL (L)                                    
C
C   COMPILE THISPROGRAM SO THAT INTEGERS HAVE 8 BYTES (64 BITS)
C
      PARAMETER(MAXOBS=2000)
      PARAMETER(MAXBLK=100)
C
C
C
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
      DATA SATID/1807001.D0/
      DATA SIGMA/1.D0/
      
      CHARACTER(len = 60) path
      CHARACTER(len = 27) :: k1="/data/geodyn_proj/pygeodyn/"
      CHARACTER(len = 36) :: k2
      CHARACTER(len = 69) localpath
      CHARACTER(len = 23) options_filename
      CHARACTER(len = 92) options_file
      CHARACTER(len = 20) in_filename
      CHARACTER(len = 20) out_filename
      CHARACTER(len = 60) sys_call
      CHARACTER(len = 70) in_file
      CHARACTER(len = 70) out_file

      character*256 :: CTMP
      character*256, allocatable :: MY_ARRAY(:)
      integer :: I = 0, IERR = 0, NUM_LINES = 0

C     

C Define path and name of the options file created in python
      
      k2 = "pygeodyn_develop/util_preprocessing/"
      localpath = k1//TRIM(k2)
      options_filename = "options_fortrancode.txt"
      options_file = TRIM(localpath)//options_filename
      
C      WRITE(6,*) options_file

      open (121, file=options_file,FORM='FORMATTED', status='old')

C        ! 2. Get number of lines
        do while (IERR == 0)
          NUM_LINES = NUM_LINES + 1
          read(121,*,iostat=IERR) CTMP
        end do
        NUM_LINES = NUM_LINES - 1
C        write(*,'(A,I0)') "Number of lines = ", NUM_LINES

C       ! 3. Allocate array of strings
        allocate(MY_ARRAY(NUM_LINES))

C       ! 4. Read the file content
        rewind(121)
        do I = 1, NUM_LINES
          read(121,'(A)') MY_ARRAY(I)
        end do

C        ! 5. Print array to standard output
C        do I = 1,size(MY_ARRAY,1)
C          write(6,*) trim(MY_ARRAY(I))
C        end do

C        deallocate(MY_ARRAY)
        close (121) 

      path = trim(MY_ARRAY(1))
      in_filename = trim(MY_ARRAY(2))
      out_filename = trim(MY_ARRAY(3))
      in_file = TRIM(path)//TRIM(in_filename)
      out_file =  TRIM(path)//TRIM(out_filename)
       
       write(6,*) "input_file = ", in_file
       write(6,*) "output file = ", out_file

      INQUIRE(EXIST=LEXIST,FILE=in_file)
      IF(LEXIST) THEN
       OPEN(UNIT=10,FILE=in_file,FORM='FORMATTED',STATUS='OLD')
      ELSE
C         WRITE(6,*) 'Error-- file not found:',in_file
         STOP
      ENDIF
C      
      sys_call = 'rm ' // out_file
      INQUIRE(EXIST=LEXIST,FILE=out_file)
      IF(LEXIST) CALL SYSTEM(sys_call)
      OPEN(UNIT=40,FILE=out_file,FORM='UNFORMATTED',STATUS='NEW')
      
C
 1000 CONTINUE                                                                  
C
C READ IN A CHUNK OF MAXBLK POINTS FROM SEQUENTIAL FILE
C LOAD THEM INTO HOLDING ARRAYS
C
      NM=0
      LEND=.FALSE.
      DO 1100 I=1,MAXBLK
      READ(10,*,END=1200) MJDS(I),FSEC(I),X(I,1),X(I,2),X(I,3)
      NM=NM+1
 1100 CONTINUE                                                                  
 1200 CONTINUE                                                                  
      IF(NM.LT.MAXBLK) LEND=.TRUE.
      IF(NM.EQ.0) GO TO 3000
C
C  PUT THE CHUNK OF MAXOB POINTS INTO G2B FORMAT
C  DO THIS FOR X, Y AND Z
C
      DO 2000 IMEAS=1,3
      if(IMEAS.eq.1)then  
C          WRITE(6,*) 'MJDS -',DFLOAT(MJDS(1))
C          WRITE(6,*) 'FSEC -',FSEC(1)
C          WRITE(6,*) 'mjds calc -',DFLOAT(MJDS(NM)-MJDS(1))+FSEC(NM)-FSEC(1)
      endif
      BUFW(1)=DFLOAT(MJDS(1))
      BUFW(2)=FSEC(1)
      BUFW(3)=DFLOAT(MJDS(NM)-MJDS(1))+FSEC(NM)-FSEC(1)
      BUFW(4)=VLIGHT
      BUFW(5)=WRD5+DFLOAT(IMEAS)
      BUFW(7)=DFLOAT(NM)
      BUFW(8)=WRD8
      BUFW(9)=PRE9
      BUFW(10)=-9000000.D0
C
      CALL BUFOUT(IPTO,BUFW,BFOUT)
C
      DO K=1,10
       BUFW(K)=0.D0
      ENDDO
      BUFW(5)=2000.D0
      BUFW(8)=SATID
      BUFW(9)=PRE8
      BUFW(10)=-8000000.D0
      CALL BUFOUT(IPTO,BUFW,BFOUT)
C
C
      DO 1400 I=1,NM
      DO K=1,10
       BUFW(K)=0.D0
      ENDDO
      if(I.eq.1)then  
C          WRITE(6,*) 'X -',X(I,IMEAS)
      endif
      BUFW(1)=X(I,IMEAS)
      BUFW(6)=DFLOAT(MJDS(I)-MJDS(1))+FSEC(I)-FSEC(1)
      BUFW(7)=SIGMA
      
      CALL BUFOUT(IPTO,BUFW,BFOUT)
 1400 CONTINUE
C
C
 2000 CONTINUE
      IF(.NOT.LEND) GO TO 1000                                                                
C
C
C
 3000 CONTINUE                                                                  
C
C
C
      IF(IPTO.GT.0) WRITE(40) BFOUT
C      IF(IPTO.GT.0) WRITE(41,*) BFOUT


      CLOSE(40)

      STOP
      END
      
      
      SUBROUTINE BUFOUT(IPTO,BUFI,BUFO)
      IMPLICIT REAL*8(A-H,O-Z),LOGICAL(L)
      DIMENSION BUFI(10)
      DIMENSION BUFO(200,10)
      DATA NBLKO/0/
C
      DO 100 I=1,1
      IPTO=IPTO+1
      IF(IPTO.EQ.201) THEN
        NBLKO=NBLKO+1
        WRITE(6,78912) NBLKO
C         WRITE(6,*) ' ----------------------- '
78912   FORMAT(' BLOCK # ',I6' WRITTEN')
        WRITE(40) BUFO
C        WRITE(41,*) BUFO

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
      


