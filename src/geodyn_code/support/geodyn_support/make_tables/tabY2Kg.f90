PROGRAM MAIN 
!$MAIN                                                                  
!***********************************************************************
! NAME              MAIN                                                
! VERSION 7902.0    DATE 79/02/01       PGMR - BILL EDDY                
! VERSION 9104.0    DATE 91/04/01       PGMR - SCOTT B. LUTHCKE         
! VERSION 9105.0    DATE                PGMR - SCOTT B. LUTHCKE         
! VERSION 9105.0    DATE 91/06/03       PGMR - DENISE WILLIAMS          
! VERSION 9202.0    DATE 92/02/04       PGMR - CRAIG JOHNSON            
! VERSION 9207.0    DATE 92/07/28       PGMR - DENISE MOORE             
! VERSION 9210.0    DATE 92/11/02                                       
! VERSION 9301.0    DATE 93/01/11       PGMR - DENISE MOORE             
! VERSION 9302.0    DATE 93/01/21       PGMR - SCOTT B. LUTHCKE         
! VERSION 0110.0    DATE 01/10/03       PGMR - TERRY WILLIAMS           

! FUNCTION          MAIN PROGRAM. CONTROLS CALLS TO SUBROUTINES TO READ 
!                   AND UPDATE A1-UTC,A1-UT1,POLAR MOTION,SOLAR FLUX,AND
!                   MAGNETIC FLUX TABLES                                
! INPUT UNITS                                                           
!                   INTP    5  STANDARD INPUT UNIT USED FOR OPTION CARDS
!                   OLDMST 11  SEQUENTIAL CARD IMAGE FILE CONTAINING    
!                              OLD MASTER FILE                          
!                   UPDATE 12  SEQUENTIAL CARD IMAGE FILE CONTAINING    
!                              UPDATES TO OLD MASTER FILE               
! OUTPUT UNITS                                                          
!                   OUTP    6  STANDARD OUTPUT FILE                     
!                   PUNCH   7  PUNCH FILE FOR UPDATES TO OLD MASTER FILE
!                   NEWMST 13  SEQUENTIAL CARD IMAGE FILE ON WHICH NEW  
!                              MASTER FILE IS WRITTEN                   
!                   NEWGDN 14  SEQUENTIAL BINARY FILE ON WHICH NEW      
!                              GEODYN FILE IS WRITTEN                   
!                    PNCH  15  SEQUENTIAL CARD IMAGE FILE USED FOR A    
!                              SCRATCH FILE                             
!                   UNTKP3 16  3-HOUR K-INDICES UPDATE CARDS            

!***********************************************************************
IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
SAVE 
CHARACTER A 
CHARACTER*1 NUMA1 
CHARACTER*80 A80, ACARD 
CHARACTER*4 FILE,XNAME1 
CHARACTER MASTER*6,SECNAM*7,SECOVR*6,XNMA6*7 
CHARACTER*8 CDNAME,GDNTBL,BLANK,XNMA8,HDRTLE,XTITLE 
      LOGICAL POLESW,LNOBIH 
      LOGICAL BIHNEW,OVRIDE,PRNTMS,PRNTGD,SLFXPR 
      LOGICAL OVRFLW,LUOPT,LFLXPRED 
      INTEGER IDUM2,IDUM3 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      INTEGER FA1UTC,FPOLE,FFLUXS,FFLXAP,FFLXKP8,FFLXKP 
      INTEGER FFXAPP,FFXKP8P,FFXKPP 
      INTEGER SFDATE,SSDATE,SFPEND,SFPBEG 

      DIMENSION A(80) 
      DIMENSION NUMA1(10) 
      DIMENSION SECNAM(6), IDATES(16)  ,IDATPR(16) 
      DIMENSION DUMMY(100000),IDUM2(200000),IDUM3(200000) 
      DIMENSION CDNAME(7),LUOPT(6) 
      DIMENSION SECOVR(2),NOVR(2) 
      DIMENSION IUDATES(16), INDATES(16), LFLXPRED(4), IFLXPRED(10) 

      COMMON/BADINP/MNTHBD(12,2),NBFLUX(3) 
      COMMON/OVRFLW/OVRFLW(2) 
      COMMON/CARDS /NMCDS(6),NUPCDS(6),NSKIP(6) 
      COMMON/CTIME/DAYREF(11),IYREF 
      COMMON/DAYMTH/NDAYS(12) 
      COMMON/EXDATE/IEXYMD,IEXHMS 
      COMMON/HEADER/FA1UTC,LA1UTC,FPOLE,LPOLE,FFLUXS,LFLUXS,            &
     &              FFLXAP,LFLXAP,FFLXKP8,LFLXKP8,FFLXKP,LFLXKP,        &
     &              NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP             
      COMMON/KPOINT/INDD1,INDD2,INDD3 
      COMMON/OPTION/OVRIDE,PRNTMS,PRNTGD,BIHNEW,SLFXPR,LNOBIH 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      COMMON/SOLARD/SFDATE,SSDATE,NSSSTD,NSSNM,NSUNV 
      COMMON/MONTHS/MONTH(13,2) 
      COMMON/EYMD/IPDUPD,IPDNMT 
!$GPP END COMMONS                                                       
      EQUIVALENCE (A(1),XNMA6) 
      EQUIVALENCE (IDATES(1),FA1UTC) 
      EQUIVALENCE(DUMMY(1),IDUM2(1)) 
      EQUIVALENCE (A(1),A80) 

      DATA NOPTS/8/ 
      DATA MAXDIM/100000/ 
      DATA MAXDM2/200000/ 
      DATA MACSCL/2/ 
      DATA MASTER,FILE/'MASTER','FILE'/,NSECT/5/,IDUM/0/ 
      DATA CDNAME/'OVERRIDE','PRNTMAST','PRNTGDYN','BIHOLD  ',          &
     &            'SOLFLXPR','NOBIHREF','TITLE   '/                     
      DATA SECNAM/'A1UTC  ','POLE   ','FLUXS  ','FLUXAP ','FLUXKP8',    &
     &            'FLUXKP '/                                            
      DATA GDNTBL/'GDYNTBLE'/ 
      DATA BLANK/'        '/ 
      DATA NUMA1/'0','1','2','3','4','5','6','7','8','9'/ 

!--------------------------------------------------------------------------                                                                       

! OPEN UNITS WITH PROPER ATTRIBUTES                                     


!     INTP    4  STANDARD INPUT UNIT USED FOR OPTION CARDS
      OPEN(UNIT=INTP,FILE='fort.4',                                     &
     & STATUS='OLD',FORM='FORMATTED',BLANK='ZERO')                      

!     OLDMST 11  SEQUENTIAL CARD IMAGE FILE CONTAINING OLD MASTER FILE                          
      OPEN(UNIT=OLDMST,FILE='fort.11',                                  &
     & STATUS='OLD',FORM='FORMATTED',BLANK='ZERO')                      

!     UPDATE 12  SEQUENTIAL CARD IMAGE FILE CONTAINING UPDATES 
!                TO OLD MASTER FILE               
      OPEN(UNIT=UPDATE,FILE='fort.12',                                  &
     & STATUS='OLD',FORM='FORMATTED',BLANK='ZERO')                      

!     NEWMST 13  SEQUENTIAL CARD IMAGE FILE FOR NEW MASTER FILE 
      OPEN(UNIT=NEWMST,FILE='fort.13',                                  &
     & STATUS='UNKNOWN',FORM='FORMATTED',BLANK='ZERO')                      

!     NEWGDN 14  SEQUENTIAL BINARY FILE FOR NEW GEODYN BIH FILE 
      OPEN(UNIT=NEWGDN,FILE='fort.14',                                  &
     & STATUS='UNKNOWN',FORM='UNFORMATTED')                      


! CALL SYSTIM TO GET DATE AND TIME OF THIS RUN                          

      CALL SYSTIM 

! WRITE PROGRAM TITLE                                                   
      WRITE(OUTP,33000) IEXYMD,IEXHMS 
! READ CARD INPUT STREAM FOR OPTIONS                                    
      HDRTLE=BLANK 
   10 CONTINUE 
      READ(INTP,10000,END=300) XNMA8,IDATE,IDATE2,XTITLE 
!12/00IF(IDATE.GT.0.AND.IDATE.LT.500101) IDATE=IDATE+1000000            
!12/00IF(IDATE2.GT.0.AND.IDATE2.LT.500101) IDATE2=IDATE2+1000000        
      IF(IDATE.GT.0.AND.IDATE.LT.5001) IDATE=IDATE+10000 
      IF(IDATE2.GT.0.AND.IDATE2.LT.5001) IDATE2=IDATE2+10000 
      DO 50 I=1,NOPTS 
      IF(XNMA8.EQ.CDNAME(I)) GO TO 60 
   50 END DO 
      WRITE(OUTP,20050) XNMA8 
      STOP 5 
   60 CONTINUE 
      GO TO (70,80,90,100,110,120,130),I 
! OVERRIDE OPTION                                                       
   70 CONTINUE 
      OVRIDE=.TRUE. 
      GO TO 10 
! PRINT MASTER FILE                                                     
   80 CONTINUE 
      PRNTMS=.TRUE. 
      GO TO 10 
! PRINT GEODYN WORKING FILE                                             
   90 CONTINUE 
      PRNTGD=.TRUE. 
      GO TO 10 
! BIH COORDINATE SYSTEM FLAG                                            
  100 CONTINUE 
      BIHNEW=.FALSE. 
      GO TO 10 
! SOLAR FLUX PREDICTION OPTION                                          
  110 CONTINUE 
      SLFXPR=.TRUE. 
      IF(IDATE.GT.0) GOTO 115 
      SFPEND=0 
      SFDATE=0 
      GO TO 10 
  115 CONTINUE 
! SET END DATE FOR SOLAR FLUX PREDICTION IN FORM YYMMDD                 
      IY = IDATE/100 
      IM = IDATE-IY*100 
      ID = NDAYS(IM) 
      IF(IM.EQ.2.AND.MOD(IY,4).EQ.0) ID = 29 
      SFPEND = IY*10000+IM*100+ID 
      SFDATE = IDATE 
      GO TO 10 
  120 CONTINUE 
      LNOBIH=.TRUE. 
      GOTO 10 
! HEADER TITLE (WEI)                                                    
  130 CONTINUE 
      HDRTLE=XTITLE 
      GOTO 10 

  300 CONTINUE 
      WRITE(OUTP,30000) OVRIDE,PRNTMS,PRNTGD,LNOBIH,BIHNEW,SLFXPR,      &
     &                  HDRTLE                                          
      NSSSTD = 6801 
      SSDATE = SFDATE 

! READ OLD MASTER FILE; SET START/STOP TIMES AND CHECK FOR OUT OF SEQUEN
! RECORDS                                                               

      READ(OLDMST,'(A80)',END=350) ACARD 
  350 CONTINUE 
      IF(ACARD(1:11).NE.'MASTER FILE') THEN 
         WRITE(OUTP,20000) 
         STOP 10 
      ENDIF 

! READ OLD MASTER FILE FOR START AND END DATE OF EACH SECTION AND TEST  
! FOR SEQUENCE NUMBERS OUT OF ORDER                                     

  400 CONTINUE 
         READ(OLDMST,10200,END=1000) XNMA6,NCDS 

         DO 410 I=1,NSECT 
            IF(XNMA6.EQ.SECNAM(I)) GO TO 500 
  410    CONTINUE 
         WRITE(OUTP,20100) XNMA6 
         STOP 20 

  500    CONTINUE 

         ISECT = I 
         NMCDS(ISECT) = NCDS 
         POLESW = .FALSE. 
         IF(ISECT.EQ.2) POLESW = .TRUE. 

         READ(OLDMST,'(I8,64X,I8)',END=900) IYMD,ISEQ 
         IF(IYMD.LT.50010100) IYMD=IYMD+100000000 
         IF(.NOT.POLESW) IYMD = IYMD/100 

! SET START DATE                                                        

      IDATES(ISECT*2-1)=IYMD 
      IYMDP=IYMD 
      NOSKIP=NMCDS(ISECT) 

         DO 600 I=2,NOSKIP 
            READ(OLDMST,'(I8,64X,I8)',END=900) IYMD,ISEQ 
            IF(IYMD.LT.50010100) IYMD=IYMD+100000000 
            IF(.NOT.POLESW) IYMD = IYMD/100 

            IF(ISEQ.EQ.I .AND. IYMD.GT.IYMDP) THEN 

               IF(POLESW) THEN 
                  IYMD1=IYMDP/100 
                  IYMD2=IYMD/100 
                  IHMS1=(IYMDP - IYMD1*100) * 10000 
                  IHMS2=(IYMD  - IYMD2*100) * 10000 
                  CALL DIFF(IYMD1,IHMS1,IYMD2,IHMS2,IDAY,ISEC) 
                  NHOURS=IDAY*24 + ISEC/3600 
                  IF(I.EQ.2) INTVAL=NHOURS 
                  IF(NHOURS.NE.INTVAL) GOTO 9100 
               ENDIF 

            ELSE 
               IF(IYMD.GT.1000000)IYMD=IYMD-1000000 
               WRITE(OUTP,20200) SECNAM(ISECT),IYMD,ISEQ 
               STOP 30 
            ENDIF 

            IYMDP=IYMD 
  600    CONTINUE 

! SET END DATE FOR EACH SECTION                                         

         IF(ISECT.LE.2) THEN 
            IDATES(ISECT*2)=IYMD 
         ELSE 
! SET END DATE FOR FLUX DATA; NEED TO GET LAST DAY OF THE MONTH         
            IY=IYMD/10000 
            IM=(IYMD-IY*10000)/100 
            IDAY=NDAYS(IM) 
            IF(IM.EQ.2 .AND. MOD(IY,4).EQ.0) IDAY=29 
            IDATES(ISECT*2)=(IYMD/100)*100+IDAY 
         ENDIF 

      GO TO 400 

  900 CONTINUE 
      WRITE(OUTP,20400) SECNAM(ISECT),ISEQ 
      STOP 40 
 1000 CONTINUE 
      REWIND OLDMST 

! END DATES OF FILE AS FOUND ON OLD MASTER FILE                         

      NSFDAT = 0 
      NTEMPS = 0 
      NSSDAT = 0 
      WRITE(OUTP,34100) 
      DO 1001 I=1,10 
      POLESW=.FALSE. 
      IF(I.EQ.3.OR.I.EQ.4.) POLESW=.TRUE. 
      IF(IDATES(I).GT.100000000) THEN 
        IDATPR(I)=IDATES(I)-100000000 
      ELSE IF(.NOT.POLESW.AND.IDATES(I).GT.1000000) THEN 
        IDATPR(I)=IDATES(I)-1000000 
      ELSE 
        IDATPR(I)=IDATES(I) 
      ENDIF 
 1001 END DO 
      WRITE(OUTP,34000) (IDATPR(I),I=1,6),NSFDAT,(IDATPR(I),I=7,10),    &
     &                  NTEMPS,NSSDAT                                   
! SAVE POLE A1-UT1 END DATE                                             
      IDATEP=IDATES(4) 

! READ UPDATE FILE,DETERMINE SECTIONS BEING UPDATED,COUNT NUMBER        
! OF UPDATES IN EACH SECTION, DETERMINE NEW END DATES                   

      NSECT=6 
      ICNT=0 

! INITIALIZE ARRAYS                                                     

      DO 1050 I = 1, NSECT 
         LUOPT(I) = .FALSE. 
         IUDATES(I*2-1) = 0 
         IUDATES(I*2) = 0 
 1050 END DO 

      WRITE(OUTP,30100) UPDATE 
 1100 CONTINUE 

         READ(UPDATE,'(A80)',END=1400) ACARD 
         ICNT = ICNT + 1 
         WRITE(OUTP,'(1X,I5,1X,A80)') ICNT,ACARD 

         IF(ACARD(1:1).EQ.' ') ACARD(1:1)='0' 
         IF(LGE(ACARD(1:1),'0') .AND. LLE(ACARD(1:1),'9')) THEN 
            NUPCDS(ISECT) = NUPCDS(ISECT) + 1 
            IF(ISECT.EQ.2) THEN 
!                POLE                                                   
!              READ(ACARD,'(I8)') IUDATES(ISECT*2)                      
!               IF(IUDATES(ISECT*2).LE.991231)                          
!    1         IUDATES(ISECT*2)=IUDATES(ISECT*2)*100                    
!              IF(IUDATES(ISECT*2).LT.50010100)                         
!    1         IUDATES(ISECT*2)=IUDATES(ISECT*2)+100000000              
!                modified 3/27/00                                       
               READ(ACARD,'(I6)') IUDATES(ISECT*2) 
               READ(ACARD(7:8),'(I2)') IUHRS 
               IF(IUDATES(ISECT*2).LT.500101)                           &
     &         IUDATES(ISECT*2)=IUDATES(ISECT*2)+1000000                
               IUDATES(ISECT*2)=IUDATES(ISECT*2)*100 + IUHRS 
            ELSE 
               READ(ACARD,'(I6)') IUDATES(ISECT*2) 
               IF(IUDATES(ISECT*2).LT.500101)                           &
     &         IUDATES(ISECT*2)=IUDATES(ISECT*2)+1000000                
            ENDIF 
            IF(NUPCDS(ISECT).EQ.1) IUDATES(ISECT*2-1) = IUDATES(ISECT*2) 
         ELSE 
            DO 1200 I = 1, NSECT 
               ISECT = I 
               IF(ACARD(1:7).EQ.SECNAM(I)) GO TO 1300 
 1200       CONTINUE 
            WRITE(OUTP,20500) ACARD(1:7) 
            STOP 50 
         ENDIF 
         GO TO 1100 

 1300    CONTINUE 
         LUOPT(ISECT) = .TRUE. 
         NSKIP(ISECT) = ICNT - 1 
         NUPCDS(ISECT) = 0 
      GO TO 1100 

 1400 CONTINUE 
      REWIND UPDATE 

! CHECK START TIMES OF UPDATES TO ENSURE CONTINUITY IN THE TABLES       

! CHECK POLE UPDATES                                                    

      IF(LUOPT(2)) THEN 
         IF(IUDATES(3).GT.IDATES(4)) THEN 
            IYMD1 = IDATES(4)/100 
            IYMD2 = IUDATES(3)/100 
            IHMS1 = (IDATES(4) - IYMD1*100)*10000 
            IHMS2 = (IUDATES(3) - IYMD2*100)*10000 
            CALL DIFF(IYMD1,IHMS1,IYMD2,IHMS2,IDAY,ISEC) 
            NHOURS = IDAY*24 + ISEC/3600 
            IF(NHOURS.NE.INTVAL) THEN 
            IF(IYMD1.GT.1000000)IYMD1=IYMD1-1000000 
            IF(IYMD2.GT.1000000)IYMD2=IYMD2-1000000 
               WRITE(OUTP,40000) SECNAM(2) 
               WRITE(OUTP,*) ' NHOURS,INTVAL,IDATE,IUDATE ',            &
     &         NHOURS,INTVAL,IYMD1,IYMD2                                
               STOP 51 
            ENDIF 
         ENDIF 
      ENDIF 

! CHECK FLUX SECTIONS                                                   

      DO 1500 I = 3, NSECT 
         IF(LUOPT(I)) THEN 
            IF(IUDATES(I*2-1).GT.IDATES(I*2)) THEN 
               IYMD1 = IDATES(I*2) 
               IYMD2 = IUDATES(I*2-1) 
               IHMS1 = 0 
               IHMS2 = 0 
               CALL DIFF(IYMD1,IHMS1,IYMD2,IHMS2,IDAY,ISEC) 
               NHOURS = IDAY*24 + ISEC/3600 
               IF(NHOURS.NE.24) THEN 
                 IF(IYMD1.GT.1000000)IYMD1=IYMD1-1000000 
                 IF(IYMD2.GT.1000000)IYMD2=IYMD2-1000000 
                 WRITE(OUTP,40000) SECNAM(I) 
                 WRITE(OUTP,*) ' NHOURS,IDAY,IDATE,IUDATE ',            &
     &           NHOURS,IDAY,IYMD1,IYMD2                                
                  STOP 51 
               ENDIF 
            ENDIF 
         ENDIF 
 1500 END DO 

! SET NEW END DATES FOR OUTPUT FILES                                    

      DO 1600 I = 1, NSECT 
         IF(LUOPT(I)) THEN 
            ISTOP = IDATES(I*2) 
            ISTOPU = IUDATES(I*2) 
            write(6,*)'idates,iudates,i',idates(I*2),iudates(I*2),I 
            IF(ISTOPU.GT.ISTOP) THEN 
               ISTOP = ISTOPU 
               IF(I.GE.3) THEN 
                  IYM = ISTOP/100 
                  IMO = MOD(IYM,100) 
                  IYR = IYM/100 
                  IDAY = NDAYS(IMO) 
                  IF(IMO.EQ.2 .AND. MOD(IYR,4).EQ.0) IDAY = 29 
                  ISTOP = IYM*100 + IDAY 
               ENDIF 
            ENDIF 
            IDATES(I*2) = ISTOP 
         ENDIF 
 1600 END DO 

      FFLXKP=FFLXAP 
      LFLXKP=LFLXAP 
      INSAV = IDATES(6) 
      IF(SLFXPR) THEN 
         IF(SFDATE.LE.0) THEN 
            SFDATE=IDATES(6)/100 
            SSDATE=IDATES(6)/100 
            SFPEND=IDATES(6) 
         ELSE 
            NTEMP = NSSSTD*100+1 
            NTEMPY = SFDATE/100 
            NTEMPM = SFDATE-NTEMPY*100 
            NTEMPD = NDAYS(NTEMPM) 
            IF(NTEMPM.EQ.2 .AND. MOD(NTEMPY,4).EQ.0)NTEMPD=29 
            NTEMP1 = SFDATE*100+NTEMPD 
            IDATES(6) = SFPEND 
            IHOLDD = INSAV/100 
            CALL NEXTDT(SFPBEG,IHOLDD) 
         ENDIF 
      ELSE 
         SFPBEG=0 
         NTEMP=0 
         NTEMP1=0 
      ENDIF 
      IF(SFPBEG.GT.1000000)SFPBEG=SFPBEG-1000000 
      ISFPBG = SFPBEG/100 

! WRITE HEADER RECORD FOR GEODYN WORKING FILE                           

      NKPDAY=1 
      NUTC=NMCDS(1)+NUPCDS(1) 
      IF(BIHNEW) THEN 
         IBIH=2 
      ELSE 
         IBIH=1 
      ENDIF 

      DO 1601 I=1,10 
      POLESW=.FALSE. 
      IF(I.EQ.3.OR.I.EQ.4.) POLESW=.TRUE. 
      IF(IDATES(I).GT.100000000) THEN 
        IDATPR(I)=IDATES(I)-100000000 
      ELSE IF(.NOT.POLESW.AND.IDATES(I).GT.1000000) THEN 
        IDATPR(I)=IDATES(I)-1000000 
      ELSE 
        IDATPR(I)=IDATES(I) 
      ENDIF 
 1601 END DO 
      WRITE(OUTP,34200) 
      WRITE(OUTP,34000) (IDATPR(I),I=1,6),SFPBEG,(IDATPR(I),I=7,10),    &
     &                  NTEMP,NTEMP1                                    
      IF(FFLXKP.GT.1000000)THEN 
      FFXKPP=FFLXKP-1000000 
      ELSE 
      FFXKPP=FFLXKP 
      ENDIF 
      IF(LFLXKP.GT.1000000)THEN 
      LFXKPP=LFLXKP-1000000 
      ELSE 
      LFXKPP=LFLXKP 
      ENDIF 
      IF(FFLXKP.GT.1000000)THEN 
      FFXKP8P=FFLXKP8-1000000 
      ELSE 
      FFXKP8P=FFLXKP8 
      ENDIF 
      IF(LFLXKP.GT.1000000)THEN 
      LFXKP8P=LFLXKP8-1000000 
      ELSE 
      LFXKP8P=LFLXKP8 
      ENDIF 
      WRITE(NEWGDN) GDNTBL,(IDATPR(I),I=1,8),FFXKPP,LFXKPP,             &
     & NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP,NUTC,NKPDAY,IBIH,        &
     & IEXYMD,IEXHMS,ISFPBG,FFXKP8P,LFXKP8P,HDRTLE,INTVAL,              &
     & (IDUM,I=1,19)                                                    
!cCHP\      WRITE(6,*)'dbg *** GDNTBL,(IDATPR(I),I=1,8),FFXKPP,LFXKPP,  
!cCHP\     1 NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP,NUTC,NKPDAY,IBIH,  
!cCHP\     2 IEXYMD,IEXHMS,ISFPBG,FFXKP8P,LFXKP8P,HDRTLE,INTVAL,        
!cCHP\     3 (IDUM,I=1,19)'                                             
!cCHP\      WRITE(6,*)GDNTBL,(IDATPR(I),I=1,8),FFXKPP,LFXKPP,           
!cCHP\     1 NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP,NUTC,NKPDAY,IBIH,  
!cCHP\     2 IEXYMD,IEXHMS,ISFPBG,FFXKP8P,LFXKP8P,HDRTLE,INTVAL,        
!cCHP\     3 (IDUM,I=1,19)                                              
      IDATES(6) = INSAV 

! WRITE HEADER RECORD FOR NEW MASTER TAPE                               

      WRITE(NEWMST,10100) MASTER ,FILE 
      REWIND UPDATE 
      REWIND OLDMST 
! READ OLD MASTER FILE HEADER RECORD                                    
      READ(OLDMST,10100) XNMA6 
! READ OLD MASTER FILE AND UPDATE FILE FOR A1-UTC VALUES                
      NVAR=NMCDS(1)+NUPCDS(1)+1 
      NDIM=MAXDIM-3*NVAR 
      INDD1=NDIM+1 
      INDD2=INDD1+NVAR 
      INDD3=INDD2+NVAR 
      NWDS=3*NVAR*MACSCL 
      MAXDIM=MAXDIM*MACSCL 
      NWDS1=NWDS 
      IF(NWDS1.GT.MAXDIM) GO TO 9000 
      CALL RA1UTC(DUMMY(INDD1),DUMMY(INDD2),DUMMY(INDD3),NVAR,NUTC) 

! CALCULATE DAYS FROM 660101 TO END DATE OF A1UT1 TABLES                
! AND DAYS FROM BEG. OF A1UT1 TABLES TO 660101                          

      IY2=IDATEP/100 
      IH2=(IDATEP - IY2*100)*10000 
      CALL DIFF(660101,0,IY2,IH2,IDAY,ISECS) 
      DAYE=IDAY  + ISECS/86400.0D0 
      IY1=IDATES(3)/100 
      IH1=(IDATES(3) - IY1*100)*10000 
      CALL DIFF(IY1,IH1,IY2,IH2,IDAY,ISECS) 
      NHOURS=IDAY*24 + ISECS/3600 
      NA1UT1=NHOURS/INTVAL  + 1 

      CALL DIFF(IY1,IH1,660101,0,IDAY,ISECS) 
      DAYB66=IDAY  + ISECS/86400.0D0 

! READ OLD MASTER FILE AND UPDATE FILE FOR POLE/A1UT1 VALUES            

      NVAR=(NMCDS(2)+NUPCDS(2)) 
      INDX1=1 
      INDX2=INDX1+NVAR*MACSCL 
      INDX3=INDX2+NVAR 
      INDX4=INDX3+NVAR 
      INDX5=INDX4+NVAR 
      NWDS1=NVAR*6+NWDS 
      IF(NWDS1.GT.MAXDIM) GO TO 9000 

      CALL RPOLE(IDUM2(INDX1),IDUM2(INDX2),IDUM2(INDX3),IDUM2(INDX4),   &
     &          IDUM2(INDX5),INTVAL,NA1UT1,NVAR,NUTC,DAYE,DAYB66,DUMMY) 

! TEST TO VERIFY THAT DATE OF FINAL POLE VALUE OF UPDATE FILE           
! EQUALS DATE OF FINAL POLE VALUE OF NEW MASTER FILE                    

      IF(OVRIDE.OR.(IPDUPD.EQ.0)) GO TO 1650 
         IF(IPDUPD.NE.IPDNMT) THEN 
            IF(IPDUPD.GT.100000000)IPDUPD=IPDUPD-100000000 
            IF(IPDNMT.GT.100000000)IPDNMT=IPDNMT-100000000 
            WRITE(OUTP,35200) IPDUPD,IPDNMT 
            STOP 3 
         ENDIF 
 1650 CONTINUE 

! READ OLD MASTER FILE AND UPDATE FILE FOR FLUX VALUES                  

      IY1=FFLUXS/10000 
      IY2=LFLUXS/10000 
      IM1=(FFLUXS-IY1*10000)/100 
      IM2=(LFLUXS-IY2*10000)/100 
      NMNTHS=(IY2-IY1)*12+IM2-IM1+1 
      NVAR=NMNTHS*32 
      NWDS2=NVAR+NWDS 
      NWDS1=MAX(NWDS2,NWDS1) 
      IF(NWDS1.GT.MAXDIM) GO TO 9000 

! SOLAR FLUX                                                            

      ITYPE=1 
      CALL RFLUX(IDUM2,NVAR,IDUM3,MAXDM2,ITYPE) 

! MAGNETIC FLUX AP,KP                                                   

      ITYPE=2 
      CALL RFLUX(IDUM2,NVAR,IDUM3,MAXDM2,ITYPE) 

! CALL SUNSPT SUBROUTINE TO WRITE OUT SUNSPOT DATA                      

      IF(SLFXPR) CALL SUNSPT(IDUM3) 
      WRITE(OUTP,31000) MAXDIM,NWDS1 

! READ 3 HOURLY KP VALUES & OUTPUT ON BINARY FILE(NEWGDN)               

      CALL RDKP3(LUOPT(5),NSKIP(5),NUPCDS(5)) 

! PRINT NEW MASTER TAPE                                                 

      IF(.NOT.PRNTMS) GO TO 2000 
         REWIND NEWMST 
         WRITE(OUTP,20600) 
 1900    CONTINUE 
            READ(NEWMST,20610,END=2000) A 
            WRITE(OUTP,20620) A 
         GO TO 1900 
 2000 CONTINUE 

! PRINT NEW BINARY FILE                                                 

      IF(PRNTGD) CALL PGDYN 

      DO 2300 I=1,2 
         NOVR(I)=0 
         NOUT=NBFLUX(I) 
         INM=2+I 
         IF(NOUT.GT.12) THEN 
            NOVR(I)=NOUT 
            NOUT=12 
            SECOVR(I)=SECNAM(INM) 
         ENDIF 
         IF(NOUT.GT.0) WRITE(OUTP,35000) SECNAM(INM),                   &
     &                                   (MNTHBD(J,I),J=1,NOUT)         
 2300 END DO 

      DO 2400 I=1,2 
         IF(OVRFLW(I)) WRITE(OUTP,35100) NOVR(I),SECOVR(I) 
 2400 END DO 
      WRITE(OUTP,36000) 
      CALL EXIT 

! ERROR EXITS                                                           

! NEED TO INCREASE CORE                                                 
 9000 WRITE(OUTP,32000) NWDS1 
      write(6,*)'nwds1',nwds1 
      STOP 16 

      IF(IYMD.GT.1000000)IYMD=IYMD-1000000 
      IF(IYMDP.GT.1000000)IYMDP=IYMDP-1000000 
 9100 WRITE(OUTP,32100) IYMD,INTVAL,IYMDP 
      STOP 16 

10000 FORMAT(A8,2X,I4,1X,I4,A8) 
10100 FORMAT(A6,1X,A4) 
10200 FORMAT(A7,3X,I5) 
10300 FORMAT(I6,66X,I8) 
10320 FORMAT(I8,64X,I8) 
10350 FORMAT(I6) 
10360 FORMAT(I8) 
10400 FORMAT(A4) 
10600 FORMAT(A6,4X,D15.8) 
10800 FORMAT(I6,33X,I6,27X,I8) 
20000 FORMAT(1X,10(1H*),' MASTER FILE CARD NOT FOUND ON UNIT 1,EXECUTION&
     & TERMINATING ',10(1H*))                                           
20050 FORMAT(1X,'INVALID OPTION CARD (',A8,')EXECUTION TERMINATING') 
20100 FORMAT(1X,' CARD ',A6,' IS NOT A VALID SECTION NAME IN MASTER FILE&
     & DECK')                                                           
20200 FORMAT(5X,'CARD OUT OF ORDER IN SECTION ',A6,' FOR YYMMDDHH= ',I8,&
     &' AND SEQUENCE NUMBER = ',I8,' EXECUTION TERMINATING')            
20400 FORMAT(5X,'UNEXPECTED END OF FILE ENCOUNTERED READING CARDS IN SEC&
     &TION ',A6,' LAST SEQUENCE NUMBER READ =',I6)                      
20500 FORMAT(1X,' CARD ',A8, 'IS NOT A VALID SECTION NAME IN UPDATE DECK&
     &')                                                                
20600 FORMAT(1H1,23X,'LISTING OF NEW GEODYN MASTER FILE'/) 
20610 FORMAT(80A1) 
20620 FORMAT(1X,80A1) 
30000 FORMAT(//13X,7X,'OPTIONS',6X,'T=ON,F=OFF',2X/                     &
     &    13X,'  OVERRIDE OPTION = ',L5/                                &
     &   13X,'PRINT MASTER FILE = ',L5/                                 &
     &   13X,'PRINT GEODYN FILE = ',L5/                                 &
     &   14X,'NO BIH REFERENCE = ',L5/                                  &
     &   5X,'NEW BIH COORDINATE SYSTEM = ',L5/                          &
     &   5X,'   FOR XP AND UT1 DATA      '/                             &
     &   9X,'SOLAR FLUX PREDICTION = ',L5/                              &
     &   12X,10H  TITLE =',A8,1H')                                      
30100 FORMAT(// 50(1H*),'LISTING OF UPDATES ON UNIT ',I2,51(1H*)/) 
31000 FORMAT(/130(1H*)//5X,'MAXIMUM NUMBER OF WORDS ALLOCATED FOR TEMPOR&
     &ARY ARRAYS =',I8/ 5X,'MAXIMUM NUMBER OF WORDS USED FOR TEMPORARY A&
     &RRAYS      =',I6)                                                 
32000 FORMAT(5X,'INSUFFICIENT CORE AVAILABLE. INCREASE DIMENSIONON DUMMY&
     & ARRAY IN MAIN PROGRAM AND',/,' CHANGE DATA INITIALIZED VALUE OF  &
     & MAXDIM TO AT LEAST',I8,'/2')                                     
32100 FORMAT(1X,' DATA AT ',I8,' IS NOT THE INTERVAL ',I8,' AWAY FROM', &
     & ' DATA AT ',I8,/,' PROGRAM TERMINATED AT 9100 OF SUBROUTINE MAIN'&
     &)                                                                 
33000 FORMAT(//45X,46(1H*)/45X,46(1H*)/                                 &
     &       45X,2H**,11X,'TABLE UPDATE PROGRAM',11X,2H**/              &
     &       45X,2H**,19X,'FOR',20X,2H**/                               &
     &       45X,2H**,' A1-UTC,A1-UT1,POLAR MOTION AND FLUX DATA ',2H**/&
     &        45X,2H**,14X,'VERSION 0110.0',14X,2H**/                   &
     &        45X,46(1H*)/45X,46(1H*)///                                &
     &        45X,' DATE AND TIME FOR THIS RUN WAS ',I6,1X,I6 //)       
33100 FORMAT(/129(1H*)/ 43(1H*),'PUNCHED CARDS TO BE ADDED TO OLD MASTER&
     & FILE',43(1H*)/ 130(1H*))                                         
34000 FORMAT(1H0,12X,'A1-UTC',9X,'POLE/A1-UT1/EOPS',13X,'FLUXS',        &
     & 15X,'FLUXAP',14X,'FLUXKP',14X,'SUNSPOT'/                         &
     & 5X,1(5X,'START  END',5X),4X,1(' START    END      '),2X,         &
     & 'START   END   PRED',2X,                                         &
     & 3(5X,'START  END',5X)/                                           &
     & 5X,1(4X,I6,1X,I6,3X),1(2X,I8,2X,I8),                             &
     & 4X,3(I6,1X),4X,3(I6,1X,I6,7X))                                   
34100 FORMAT(//130(1H*)//52X,'DATES FROM OLD MASTER FILE') 
34200 FORMAT(//130(1H*)//52X,'DATES AFTER UPDATES ADDED ') 
35000 FORMAT(//4(/120(1H*))//40X,'WARNING - PLEASE CHECK UPDATES IN SECT&
     &ION ',    A8/10X,'COMPUTED AVERAGE MONTHLY VALUES DO NOT AGREE WIT&
     &H INPUT AVERAGE MONTHLY VALUES FOR THE FOLLOWING MONTHS ',/       &
     & 20X,12I7)                                                        
35100 FORMAT(//4(/120(1H*))//8X,'WARNING - ',I3,' MONTHS FOR WHICH THE C&
     &OMPUTED AVERAGE DID NOT AGREE WITH THE INPUT AVERAGE WERE FOUND IN&
     &',/19X,'SECTION ',A8,'ALTHOUGH ONLY THE FIRST 12 WERE LISTED.  PLE&
     &ASE CHECK UPDATES FOR THE REST.')                                 
35200 FORMAT(//4(/120(1H*))//1X,'EXECUTION TERMINATED - DATE OF FINAL PO&
     &LE VALUE OF UPDATE FILE (',I6,') DOES NOT',/1X,'MATCH DATE OF FINA&
     &L POLE VALUE OF NEW MASTER FILE (',I6,'). POSSIBLE EXPLANATION: MI&
     &SSING DATA IN POLE UPDATES.')                                     
36000 FORMAT(//30X,'*** NORMAL END OF JOB ***') 
40000 FORMAT(' DISCONTINUOUS UPDATE IN SECTION ',A7/                    &
     &       ' EXECUTION TERMINATING ')                                 

      END                                           
!$BLKDAT                                                                

      BLOCK DATA 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      LOGICAL BIHNEW,OVRIDE,PRNTMS,PRNTGD,SLFXPR,LNOBIH 
      INTEGER FA1UTC,FPOLE,FFLUXS,FFLXAP,FFLXKP8,FFLXKP 
      !INTEGER FFXAPP,FFXKP8P,FFXKPP 
      INTEGER OLDMST,UPDATE,OUTP,PNCH,ISCRTCH 
      COMMON/BADINP/MNTHBD(12,2),NBFLUX(3) 
      COMMON/CARDS /NMCDS(6),NUPCDS(6),NSKIP(6) 
      COMMON/CTIME/DAYREF(11),IYREF 
      COMMON/DAYMTH/NDAYS(12) 
      COMMON/EXDATE/ IEXYMD,IEXHMS 
      COMMON/HEADER/FA1UTC,LA1UTC,FPOLE,LPOLE,FFLUXS,LFLUXS,            &
     &              FFLXAP,LFLXAP,FFLXKP8,LFLXKP8,FFLXKP,LFLXKP,        &
     &              NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP             
      COMMON/MONTHS/MONTH(13,2) 
      COMMON/OPTION/OVRIDE,PRNTMS,PRNTGD,BIHNEW,SLFXPR,LNOBIH 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      COMMON/SOLARD/SFDATE,SSDATE,NSSSTD,NSSNM,NSUNV 
      DATA MNTHBD,NBFLUX/24*0,3*0/ 
      DATA NMCDS,NUPCDS,NSKIP/18*0/ 
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/ 
      DATA IEXYMD,IEXHMS/2*0/ 
      DATA FA1UTC,LA1UTC,FPOLE,LPOLE,FFLUXS,LFLUXS,FFLXAP,LFLXAP,       &
     &     FFLXKP8,LFLXKP8,FFLXKP,LFLXKP/12*0/                          
      DATA NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP/24,24,96,96,96,96/ 
      DATA MONTH/0,31,60,91,121,152,182,213,244,274,305,335,366,        &
     &           0,31,59,90,120,151,181,212,243,273,304,334,365/        
      DATA OVRIDE,PRNTMS,PRNTGD,BIHNEW,SLFXPR,LNOBIH/3*.FALSE.,         &
     &     .TRUE.,.FALSE.,.FALSE./                                      
      DATA OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH           &
     &/11,12,13,4,6,14,15,16/                                           
      END                                           
!$ADDYMD                                                                
      SUBROUTINE ADDYMD(IYMD,IDAY) 
!********************************************************************   
! NAME              ADDYMD                                              

! PURPOSE           TO ADD OR SUBTRACT DAYS FROM A DATE IN THE FORM     
!                   YYMMDD AND TO PROVIDE THE USER WITH THE NEW DATE    

! CALLING SEQUENCE  CALL ADDYMD(IYMD,IDAY)                              

!    SYMBOL  TYPE   DESCRIPTION                                         

!    IYMD     I     INPUT AND OUTPUT - SIX DIGIT DATE IN THE FORM       
!                                      YYMMDD                           

!    IDAY     I     INPUT - NUMBER OF DAYS TO BE ADDED OR SUBTRACTED    
!                           FROM INPUT TAPE                             

! SUBROUTINES USED  NONE                                                

! COMMON BLOCKS     MONTHS                                              

! INPUT FILES       NONE                                                

! OUTPUT FILES      NONE                                                

! RESTRICTIONS      CANNOT PROCESS MULTIPLE CENTURIES                   



      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      COMMON/MONTHS/MONTH(13,2) 
      ISUB(IY)=MIN(MOD(IY,4),1)+1 
! SEPARATE YEAR/MONTH/DAY                                               
      IY = IYMD/10000 
      IK = IY*10000 
      IM = (IYMD-IK)/100 
      ID = IYMD-IK-IM*100 
      LY=ISUB(IY) 
! COMPUTE ELAPSED DAYS FROM JANUARY 0.0 OF CENTURY                      
      ID=(IY-1)*36525/100+MONTH(IM,LY)+ID+IDAY 
! COMPUTE NEW YEAR/MONTH/DAY                                            
      IY=(ID-1)*100/36525+1 
      ID=ID-36525*(IY-1)/100 
      LY=ISUB(IY) 
      IF(LY.EQ.1.OR.ID.LT.366) GO TO 5 
      IY=IY+1 
      ID=ID-365 
      LY=ISUB(IY) 
    5 DO 10 I=1,12 
      IF(ID.LE.MONTH(I+1,LY)) GO TO 20 
   10 END DO 
! PACK NEW YYMMDD                                                       
   20 IYMD=IY*10000+I*100+ID-MONTH(I,LY) 
      RETURN 
      END                                           
!$CLEAR                                                                 
      SUBROUTINE CLEAR(IARRAY,NDIM) 
!*************************************************************          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      DIMENSION IARRAY(1) 
      DO 100 J=1,NDIM 
      IARRAY(J)=0 
  100 END DO 
      RETURN 
      END                                           
!$DATE2                                                                 
      SUBROUTINE DATE2(DAYNR,IYMD,IHM,SEC) 
!****************************************************************       
! NAME              DATE2                                               

! PURPOSE           TO CONVERT DAYS ELAPSED FROM JAN 0.0 OF THE ARC     
!                   REFERENCE YEAR INTO A 3 WORD DATE OF THE FORM :     
!                   YYMMDD HHMM SEC                                     

! CALLING SEQUENCE  CALL DATES(DAYNR,IYMD,IHM,SEC)                      

!    SYMBOL  TYPE   DESCRIPTION                                         

!    DAYNR   DP     INPUT - DAYS ELAPSED FROM JAN 0.0 OF THE REFERENCE  
!                           YEAR                                        

!    IYMD    I      OUTPUT - YEAR, MONTH, DAY IN THE FORM OF YYMMDD     

!    IHM     I      OUTPUT - HOUR, MINUTE IN THE FORM OF HHMM           

!    SEC     R      OUTPUT - SECONDS                                    

! SUBROUTINES USED  ADDYMD     TDIF                                     

! COMMON BLOCKS     CTIME                                               

! INPUT FILES       NONE                                                

! OUTPUT FILES      NONE                                                




      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      COMMON/CTIME/DAYREF(11),IYREF 
!GEN DFLOAT                                                             
! CONVERT TO UTC TIME SYSTEM                                            
!     DAY=DAYNR+(TDIF(3,4,DAYNR)+0.5D-6)/8.64D4                         
!     GO TO 100                                                         
!     ENTRY DATE2(DAYNR,IYMD,IHM,SEC)                                   
      DAY=DAYNR+0.5D-6/8.64D4 
  100 IDAY=DAY-1.0D0 
      IYMD=IYREF*10000+101 
! CALCULATE YEAR/MONTH/DAY OF INTEREST                                  
      CALL ADDYMD(IYMD,IDAY) 
! CALCULATE NUMBER OF SECONDS REMAINING                                 
      S=8.64D4*(DAY-DBLE(IDAY+1)) 
      ISEC=S 
! CONVERT TO HOUR/MINUTE FORMAT                                         
      IHM=40*(ISEC/3600)+ISEC/60 
! REMAINING SECONDS                                                     
      SEC=S-DBLE(60*(ISEC/60))-0.5D-6 
      RETURN 
      END                                           
!$DIFF                                                                  
      SUBROUTINE DIFF(IYMD1,IHMS1,IYMD2,IHMS2,IDAY,ISEC) 
!*******************************************************************    
! NAME              DIFF                                                

! PURPOSE           TO CALCULATE THE DIFFERENCE BETWEEN ANY TWO TIME    
!                   POINTS IN THE 20TH CENTURY                          

! CALLING SEQUENCE  CALL DIFF(IYMD1,IHMS1,IYMD2,IHMS2,IDAY,ISEC)        

!    SYMBOL  TYPE   DESCRIPTION                                         

!    IYMD1   I      INPUT - DATE IN FORM YYMMDD                         

!    IHMS1   I      INPUT - TIME ON IYMD1 IN FORM HHMMSS                

!    IYMD2   I      INPUT - SECOND DATE IN FORM YYMMDD                  

!    IHMS2   I      INPUT - TIME ON IYMD2 IN FORM HHMMSS                

!    IDAY2   I      OUTPUT - ELAPSED FULL DAY DIFFERENCE                
!                            IDAY IS NEGATIVE IF IYMD2,IHMS2 IS THE     
!                            EARLIER TIME                               

!                            ISEC HAS THE SAME SIGN CONVENTION AS IDAY  

! SUBROUTINES USED  NONE                                                
!    ISEC    I      OUTPUT - REMAINDER OF DIFFERENCE IN SECONDS         

! COMMON BLOCKS     MONTHS                                              

! INPUT FILES       NONE                                                

! OUTPUT FILES      NONE                                                




      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      COMMON/MONTHS/MONTH(13,2) 
      ISUB(IY)=MIN(MOD(IY,4),1)+1 
! CHECK FOR A DIFFERENCE OF LESS THAN ONE DAY                           
      ISEC=0 
      IF(IYMD1.EQ.IYMD2) GOTO 4000 
! SEPARATE IYMD1 AND IYMD2 INTO THREE WORD EACH                         
      IY1=IYMD1/10000 
      ID1=IYMD1-IY1*10000 
      IM1=ID1/100 
      ID1=ID1-IM1*100 
      IY2=IYMD2/10000 
      ID2=IYMD2-IY2*10000 
      IM2=ID2/100 
      ID2=ID2-IM2*100 
! COMPUTE THE ELAPSED DAY SINCE JAN 0,1900                              
      L1=ISUB(IY1) 
      IYEAR1=36525*(IY1-1)/100+MONTH(IM1,L1)+ID1 
      L2=ISUB(IY2) 
      IYEAR2=36525*(IY2-1)/100+MONTH(IM2,L2)+ID2 
! CONVERT ELAPSED DAYS INTO ELAPSED SECONDS                             
      ISEC=(IYEAR2-IYEAR1)*86400 
! CALCULATE ELAPSED SECONDS INTO EACH DAY                               
 4000 ISEC1=IHMS1-40*(IHMS1/100)-2400*(IHMS1/10000) 
      ISEC2=IHMS2-40*(IHMS2/100)-2400*(IHMS2/10000) 
! SUBTRACT THE TWO ELAPSED SECONDS VALUES                               
      ISEC=ISEC+ISEC2-ISEC1 
! COMPUTE IDAY                                                          
      IDAY=ISEC/86400 
! COMPUTE ISEC                                                          
      ISEC=ISEC-IDAY*86400 
      RETURN 
      END                                           
!$FLUXKP                                                                
      SUBROUTINE FLUXKP(FLUX) 
!***********************************************************************
! NAME              FLUXKP                                              
! VERSION 7902      DATE 01/15/79       PGMR - BILL EDDY                

! FUNCTION          CALCULATE MAGNETIC FLUX KP VALUE GIVEN THE AP VALUE 
!                   BY CUBIC INTERPOLATION OF THE AP TO KP TABLES       

! INPUT PARAMETERS                                                      
!                   FLUX    - MAGNETIC FLUX AP VALUE                    
! OUTPUT PARAMETERS                                                     
!                   FLUX    - MAGNETIC FLUX KP VALUE *100               
! REFERENCE                                                             
!                   GEOMAGNETIC INDICES BY GORDON ROSTOKER              
!                   REVIEW OF GEOPHYSICS AND SPACE SCIENCE              
!                   VOL 10 NO. 4  NOVEMBER 1972                         
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      LOGICAL NOTFST 
      INTEGER FLUX 
      DOUBLE PRECISION KP 
      DIMENSION AP(28),KP(28) 
      DATA NAP/28/,NOTFST/.FALSE./ 
      DATA AP/  0.0D0,  2.0D0,  3.0D0,  4.0D0,  5.0D0,  6.0D0,  7.0D0,  &
     &          9.0D0, 12.0D0, 15.0D0, 18.0D0, 22.0D0, 27.0D0, 32.0D0,  &
     &         39.0D0, 48.0D0, 56.0D0, 67.0D0, 80.0D0, 94.0D0,111.0D0,  &
     &        132.0D0,154.0D0,179.0D0,207.0D0,236.0D0,300.0D0,400.0D0/  
      IF(NOTFST) GO TO 200 
      NOTFST=.TRUE. 
! LOAD KP ARRAY                                                         
      T3=1.0D0/3.0D0 
      SUM=0.0D0 
      DO 100 I=1,28 
      KP(I)=SUM 
      SUM=SUM+T3 
  100 END DO 
  200 CONTINUE 
      FLUXAP=FLUX 
! FIND CLOSES VALUE TO FLUXAP IN AP ARRAY                               
      DO 500 I=1,NAP 
      IF(FLUXAP.GT.AP(I)) GO TO 500 
      J=I 
      GO TO 600 
  500 END DO 
! IF AP GT 400 SET KP = 9                                               
      FLUX=900 
      RETURN 
  600 CONTINUE 
! LAGRANGE CUBIC INTERPOLATION                                          
      IF(J.GE.3) GO TO 810 
      J0=1 
      J1=2 
      J2=3 
      J3=4 
      GO TO 830 
  810 IF(J.LE.NAP-1) GO TO 820 
      J3=NAP 
      J2=NAP-1 
      J1=NAP-2 
      J0=NAP-3 
      GO TO 830 
  820 J0=J-2 
      J1=J-1 
      J2=J 
      J3=J+1 
  830 AP0=FLUXAP-AP(J0) 
      AP1=FLUXAP-AP(J1) 
      AP2=FLUXAP-AP(J2) 
      AP3=FLUXAP-AP(J3) 
      AP01=AP(J0)-AP(J1) 
      AP02=AP(J0)-AP(J2) 
      AP03=AP(J0)-AP(J3) 
      AP12=AP(J1)-AP(J2) 
      AP13=AP(J1)-AP(J3) 
      AP23=AP(J2)-AP(J3) 
      FLUXM    =KP(J0)*AP1*AP2*AP3/(AP01*AP02*AP03)+                    &
     &          KP(J1)*AP0*AP2*AP3/(-AP01*AP12*AP13)+                   &
     &          KP(J2)*AP0*AP1*AP3/(AP02*AP12*AP23)+                    &
     &          KP(J3)*AP0*AP1*AP2/(-AP03*AP13*AP23)                    
! GEODYN EXPECTS KP*100                                                 
      FLUX=FLUXM*100.0D0+1.0D-3 
      RETURN 
      END                                           
!$FLXNRM                                                                
      SUBROUTINE FLUXNORM(IYMD,IFLUX) 

!   ******************************************************************* 
!   *                                                                 * 
!   *   PURPOSE:                                                      * 
!   *            TO NORMALIZE F10.7 CM. SOLAR FLUX TO 1 AU.           * 
!   *                                                                 * 
!   *       ALOGRITHM FROM GEODYN-IIS ROUTINE FLXAVG                  * 
!   *                                                                 * 
!   ******************************************************************* 

      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 

      DATA DEGRAD/0.01745329519943296D0/ 

!   ******************************************************************* 
!   *                                                                 * 
!   *              START OF EXECUTABLE CODE                           * 
!   *                                                                 * 
!   ******************************************************************* 

      FLUX = IFLUX/10 

      CALL MODJUL(DMJD,IYMD,0.0D0,0.0D0) 

      T = DMJD - 15020.0D0 
      TJ = T/365.25D2 

      XM = 358.47583D0 + T*0.985600267D0 - (TJ*TJ)*1.5D-4 - TJ*3.0D-6 

      E = 1.675104D-2 - TJ*4.18D-5 - (TJ*TJ)*1.26D-7 

      XM = MOD(XM,360.0D0)*DEGRAD 
      EC1 = XM 

      DO 10 J = 1, 5 
         EC = XM + E*SIN(EC1) 
         IF(ABS(EC-EC1).LE.1.0D-5) GO TO 20 
         EC1 = EC 
   10 END DO 

   20 CONTINUE 

      AU = 1.0D0 - E*COS(EC) 
      FLUX = FLUX*AU**2 
      IFLUX = FLUX*10 

      RETURN 
      END                                           
!$FLXPRD                                                                
      SUBROUTINE FLXPRD( TBLAST, FLXLST, TSTART, TSTOP, TINTVL,         &
     &    FLXARR, SSNARR, NDIM )                                        
!***************************************************************        
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      INTEGER FLXARR,SSNARR 
      DIMENSION FLXARR( NDIM ), SSNARR(NDIM) 

!---------------------------------------------------------              
!     ....TBLAST = TIME OF LAST POINT IN REAL FLUX TABLES               
!     ....FLXLST = VALUE OF LAST FLUX IN REAL FLUX TABLES               
!     ....TSTART = START TIME FOR PREDICTIONS                           
!     ....TSTOP  = STOP TIME FOR PREDICTIONS                            
!     ....TINTVL = TIME INTERVAL BETWEEN PREDICTED POINTS               
!     ....FLXARR = ARRAY FOR STORING PREDICTED FLUXES                   
!     ....SSNARR = ARRAY FOR STORING PREDICTED SUN SPOT DATA            
!     ....NDIM   = DIMENSION OF FLXARR ARRAY                            
!---------------------------------------------------------              

!     ....CHECK INPUT VALUES                                            

      IF( TSTART .LT. TBLAST ) GO TO 991 
      IF( TSTOP .LT. TSTART )  GO TO 992 
      IF( TINTVL .LE. 0 )      GO TO 993 
      IBIG = ( TSTOP - TSTART ) / TINTVL 
      IF ( IBIG .GT. NDIM )       GO TO 994 

!---------------------------------------------------------------------- 

!     ....CALL TABINT TO GET THE PREDICTED FLUX FOR THE START TIME      

      TIN = TSTART 
      I = 1 
      CALL TABINT( TIN, FLXINT, SSNINT ) 
!      WRITE(6,6000) TIN,FLXINT,SSNINT                                  
!6000  FORMAT(' FLXPRD after tab:TIN,FLXINT,SSNINT ',3D20.10)           

!     ....CALL SCALFX TO COMPUTE THE SCALE FACTOR BETWEEN THE CALCULATED
!     ....FLUX AND THE LAST FLUX IN THE REAL FLUX TABLES,               
!     ....AND TO APPLY THE SCALE TO FLXINT TO MATCH LAST VALUE IN TABLES

      CALL SCALFX( .TRUE., TIN, FLXLST, FLXINT, SSNINT, SCALE ,TBLAST) 
!      WRITE(6,6100) TIN,FLXINT,SCALE,SSNINT                            
!6100  FORMAT(' FLXPRD scale:TIN,FLXINT,SCALE,SSNINT ',4D20.10)         
      NFLXIN = FLXINT+0.5D0 
      NSSNIN = SSNINT+0.5D0 
      FLXARR(I) = NFLXIN 
      SSNARR(I) = NSSNIN 
      I = I + 1 
      TIN = TIN + TINTVL 
      IF( TIN .GT. TSTOP ) GO TO 150 

!---------------------------------------------------------------------- 

!     ....NOW START THE LOOP FOR THE REST OF THE FLUXES                 

  100 CONTINUE 

!     ....CALL TABINT TO GET THE FLUX FOR THIS TIME                     

      CALL TABINT( TIN, FLXINT, SSNINT ) 
!      WRITE(6,6000) TIN,FLXINT,SSNINT                                  

!     ....CALL SCALFX TO SCALE FLXINT TO MATCH LAST VALUE IN TABLES     
!     ....SCALFX DOES NOT RECALCULATE SCALE (EXCEPT FOR MODIFYING       
!     ....THE SCALE BY DELTA AFTER 2 YEARS)                             

      CALL SCALFX( .FALSE., TIN, FLXLST, FLXINT, SSNINT, SCALE,TBLAST) 
!      WRITE(6,6100) TIN,FLXINT,SCALE,SSNINT                            

      NFLXIN = FLXINT+0.5D0 
      NSSNIN = SSNINT+0.5D0 
      FLXARR(I) = NFLXIN 
      SSNARR(I) = NSSNIN 
      I = I + 1 
      TIN = TIN + TINTVL 
      IF( TIN .GT. TSTOP ) GO TO 150 
      GO TO 100 
  150 CONTINUE 
      RETURN 

!---------------------------------------------------------------------- 

!     ....ERROR EXITS                                                   

  991 WRITE(6,1991) 
 1991 FORMAT(///' REQUESTED START TIME EARLIER THAN END OF TIME FOR ',  &
     &   'SOLAR FLUX TABLES.'/' PROGRAM STOPPING IN FLXPRD'///)         
      STOP 99 

  992 WRITE(6,1992) 
 1992 FORMAT(///' REQUESTED START TIME EARLIER REQUESTED STOP TIME'/    &
     &   ' PROGRAM STOPPING IN FLXPRD'///)                              
      STOP 99 

  993 WRITE(6,1993) 
 1993 FORMAT(///' REQUESTED TIME INTERVAL IS LESS THAN ZERO '/          &
     &   ' PROGRAM STOPPING IN FLXPRD'///)                              
      STOP 99 

  994 WRITE(6,1994) 
 1994 FORMAT(///' REQUESTED START, STOP TIMES AND INTERVAL WILL OVERFLOW&
     & THE SIZE OF THE OUTPUT SOLAR FLUX TABLES.'/                      &
     &   ' PROGRAM STOPPING IN FLXPRD'///)                              
      STOP 99 
      END                                           
!$GREGOR                                                                
         SUBROUTINE GREGOR (TIME,IYMD,IHM,SEC) 
!---------------------------------------------------------------------- 
!     ....GREGOR CONVERTS MODIFIED JULIAN DATE (TIME)                   
!     ....TO CALENDAR DATE (IYMD, IHM, SEC)                             

!     ....TIME  =  MJD                                                  
!     ....IYMD  =  YEAR, MONTH AND DAY (YYMMDD)                         
!     ....IHM   =  HOURS AND MINUTES  (HHMM)                            
!     ....SEC   =  SECONDS                                              
!---------------------------------------------------------------------- 

      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      COMMON/MONTHS/MONTH(13,2) 
!GEN DFLOAT                                                             

         IDAYS=TIME-15384.0D0 
         IY=IDAYS*100/36525 
         IDAYS=IDAYS-36525*IY/100 
         IY=IY+1 
         IF (IDAYS.NE.0) GO TO 5 
         IY=IY-1 
         IDAYS=366 

    5    IYIND=2 
         IF (MOD(IY,4).EQ.0.AND.MOD(IY,400).NE.0) IYIND=1 
         DO 10 I=2,13 
         IF(MONTH(I,IYIND) .GE.IDAYS) GO TO 20 
   10    CONTINUE 
         I=13 
   20    IM=I-1 
         ID=IDAYS-MONTH(IM,IYIND) 
         IYMD=10000*IY+100*IM+ID 

         SEC=MOD(TIME,1.0D0)*86400.0D0 
         IMIN=SEC/60.0D0 
         SEC=MOD(SEC,60.0D0) 
         IH=IMIN/60 
         IMIN=MOD(IMIN,60) 
         IHM=100*IH+IMIN 

         RETURN 

!---------------------------------------------------------------------- 
         ENTRY MODJUL (TIME,IYMD,IHM,SEC) 

!---------------------------------------------------------------------- 
!     ....MODJUL CONVERTS CALENDAR DATE (IYMD, IHM, SEC)                
!     ....TO MODIFIED JULIAN DATE (TIME)                                

!     ....TIME  =  MJD                                                  
!     ....IYMD  =  YEAR, MONTH AND DAY (YYMMDD)                         
!     ....IHM   =  HOURS AND MINUTES  (HHMM)                            
!     ....SEC   =  SECONDS                                              
!---------------------------------------------------------------------- 

         ID=MOD(IYMD,100) 
         IK=IYMD/100 
         IM=MOD(IK,100) 
         IY=IK/100 
         IYIND=2 
         IF (MOD(IY,4).EQ.0.AND.MOD(IY,400).NE.0) IYIND=1 
         ID=(IY-1)*36525/100+MONTH(IM,IYIND)+ID 
         IMIN=(IHM/100)*60+MOD(IHM,100) 
         TIME=DBLE(ID)+(DBLE(60*IMIN)+SEC)/8.64D+4 
         TIME=TIME+15384.0D0 
         RETURN 
      END                                           
!$NEXTDT                                                                
      SUBROUTINE NEXTDT(NEWDT,OLDDT) 
!************************************************************           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      INTEGER NEWDT,OLDDT,NM,NY 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
!            WRITE(OUTP,10001)                                          
!10001       FORMAT(1H0,'ENTERED SUBROUTINE NEXTDT')                    
      NY=OLDDT/100 
      NM=OLDDT-NY*100 
      IF(NM.NE.12)THEN 
          NM=NM+1 
      ELSE 
          NM=1 
          NY=NY+1 
      ENDIF 
      NEWDT=NY*10000+NM*100+1 
!           WRITE(OUTP,10002) NEWDT                                     
!10002      FORMAT(1H0,'FIRST DATE OF NEXT MONTH IS:',2X,I6)            
      RETURN 
      END                                           
!$PGDYN                                                                 
      SUBROUTINE PGDYN 
!***********************************************************************
! VERSION 7902.0    PGMR - BILL EDDY                                    

! FUNCTION          READ AND PRINT THE BINARY GEODYN WORKING FILE       
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      LOGICAL BIHNEW,OVRIDE,PRNTMS,PRNTGD,SLFXPR,LNOBIH 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      DIMENSION BUF(24),IBUF(48),IBUF2(96) 
      COMMON/OPTION/OVRIDE,PRNTMS,PRNTGD,BIHNEW,SLFXPR,LNOBIH 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      EQUIVALENCE (BUF(1),IBUF(1),IBUF2(1)) 
      EQUIVALENCE (CBUF,IBUF(27)) 
      REWIND NEWGDN 
! READ HEADER RECORD                                                    
      READ(NEWGDN,END=9000) IBUF 
      WRITE(OUTP,10000) 
      WRITE(OUTP,10100) (IBUF(I),I=3,12) 
      WRITE(OUTP,10200) (IBUF(I),I=13,18) 
      WRITE(OUTP,10800) BUF(1),(IBUF(J),J=3,26),CBUF,                   &
     &(IBUF(J),J=28,48)                                                 
! READ A1-UTC HEADER                                                    
      READ(NEWGDN,END=9000) IBUF 
      NREC=IBUF(3) 
      WRITE(OUTP,10300) BUF(1),IBUF(3),(IBUF(J),J=4,48) 
! READ A1-UTC DATA                                                      
      DO 200 J=1,NREC 
      READ(NEWGDN,END=9000) BUF 
      WRITE(OUTP,10400) BUF 
  200 END DO 
! READ A1-UT1 HEADER                                                    
      READ(NEWGDN,END=9000) IBUF 
      NREC=IBUF(3) 
      WRITE(OUTP,10300) BUF(1),IBUF(3),(IBUF(J),J=4,48) 
! READ A1-UT1 DATA                                                      
      DO 300 J=1,NREC 
      READ(NEWGDN,END=9000) BUF 
      WRITE(OUTP,10500) BUF 
  300 END DO 
! READ POLE HEADER                                                      
      READ(NEWGDN,END=9000) IBUF 
      NREC=IBUF(3) 
      WRITE(OUTP,10300) BUF(1),IBUF(3),(IBUF(J),J=4,48) 
! READ POLE DATA                                                        
      DO 400 J=1,NREC 
      READ(NEWGDN,END=9000) IBUF2 
      WRITE(OUTP,10600) IBUF2 
  400 END DO 
! READ EOP HEADER                                                       
      READ(NEWGDN,END=9000) IBUF 
      NREC=IBUF(3) 
      WRITE(OUTP,10300) BUF(1),IBUF(3),(IBUF(J),J=4,48) 
! READ EOP DATA                                                         
      DO 440 J=1,NREC 
      READ(NEWGDN,END=9000) IBUF2 
      WRITE(OUTP,10600) IBUF2 
  440 END DO 
      IK=3 
      IF(SLFXPR) IK=4 
      DO 1000 K=1,IK 
! READ FLUX HEADER AND SUN SPOT HEADER                                  
      READ(NEWGDN,END=9000) IBUF 
      NREC=IBUF(3) 
      WRITE(OUTP,10300) BUF(1),IBUF(3),(IBUF(J),J=4,48) 
! READ FLUX DATA AND SUN SPOT DATA                                      
      DO 500 J=1,NREC 
      READ(NEWGDN,END=9000) IBUF2 
      WRITE(OUTP,10700) IBUF2 
  500 END DO 
 1000 END DO 
! READ 3 HOURLY KP HEADER                                               
      READ(NEWGDN,END=9000) IBUF 
      NREC=IBUF(3) 
      WRITE(OUTP,10300) BUF(1),IBUF(3),(IBUF(J),J=4,48) 
! READ 3 HOURLY KP DATA                                                 
      DO 2000 J=1,NREC 
      READ(NEWGDN,END=9000) IBUF2 
      WRITE(OUTP,10600) IBUF2 
 2000 END DO 
 2500 CONTINUE 
      RETURN 
! END OF FILE ENCOUNTERED                                               
 9000 CONTINUE 
      WRITE(OUTP,20000) 
      STOP 13 
10000 FORMAT(1H1 // 31X,'GEODYN WORKING FILE') 
10100 FORMAT(10X,4X,'A1-UTC',6X,'POLE/A1-UT1',7X,'FLUXS',9X,'FLUXAP',9X,&
     &'FLUXKP'/10X,' START  END     START    END      ',3(' START  END  &
     &  ')/10X,I6,1X,I6,2X,I8,1X,I8,2X,3(I6,1X,I6,2X))                  
10200 FORMAT(/20X,'NUMBER OF VALUES STORED PER RECORD FOR EACH SECTION'/&
     &10X,' A1-UTC ',' A1-UT1 ','  POLE  ','FLUXS  ',' FLUXAP ',' FLUXKP&
     & '/ 8X,6(2X,I4,2X))                                               
10300 FORMAT(// 1X,A8,2I4,I6,43I2) 
10400 FORMAT(5X,D16.9,D15.7,D25.16,10X,D16.9,D15.7,D25.16) 
10500 FORMAT(1X,8D15.6) 
10600 FORMAT(1X,10I12) 
10700 FORMAT(1X,16I5) 
10800 FORMAT(/1X,A8,2I7,2I9,6I7,9I3,2I7/1X,3I8,A10,I12,20I4) 
20000 FORMAT(// 10X,'UNEXPECTED END OF FILE ENCOUNTERED ON GEODYN WORKIN&
     &G FILE. EXECUTION TERMINATING')                                   
      END                                           
!$POLELR                                                                
      SUBROUTINE POLELR(INTVAL) 
!********************************************************************   
! VERSION 8803.0    PGMR - BILL EDDY &  WEI XIA                         

! FUNCTION          LINEAR INTERPOLATION OF POLAR MOTION COEFFICIENT    
!           INTVAL - BIH POLE/A1UT1/EOP INTERVAL IN HOURS               
!  NOTE: INTERPOLATION FOR EVERY HOUR                                   
!        AS OF 3/18/91 THIS ROUTINE IS NOT CALLED.  THE DIMENSIONS      
!        OF XP,YP,XINTP, AND YINTP SHOULD BE ADJUSTED IF THIS ROUTINE   
!        IS EVER USED.                                                  
!********************************************************************** 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      INTEGER IBUF2 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      DIMENSION BUF(24),IBUF(48),IBUF2(96) 
      DIMENSION XP(4000),YP(4000),XINTP(20000),YINTP(20000) 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      EQUIVALENCE (BUF(1),IBUF(1),IBUF2(1)) 
!GEN DFLOAT                                                             
      REWIND NEWGDN 
! READ POLE HEADER                                                      
      READ(NEWGDN,END=9000) IBUF 
      NREC=IBUF(3) 
      WRITE(OUTP,10300) BUF(1),IBUF(3),(IBUF(J),J=4,48) 
! READ POLE DATA                                                        
      DO 200 J=1,NREC 
         READ(NEWGDN,END=9000) IBUF2 
! LINEAR INTERPOLATION FOR THE POLAR MOTION                             
         DO 100 I=1,48 
            XP(I+(J-1)*48)=IBUF2(2*(I-1)+1) 
            YP(I+(J-1)*48)=IBUF2(2*I) 
  100    CONTINUE 
  200 END DO 
      DO 400 I=1,48*NREC 
         SLOPEX=(XP(I+1)-XP(I))/INTVAL 
         SLOPEY=(YP(I+1)-YP(I))/INTVAL 
         DO 300 J=1,INTVAL 
            IND=J+(I-1)*INTVAL 
            XINTP(IND)=SLOPEX*DBLE(J)+XP(I) 
            YINTP(IND)=SLOPEY*DBLE(J)+YP(I) 
  300    CONTINUE 
  400 END DO 
! DEBUG                                                                 
          PRINT 10000,(XP(I),I=1,10),(YP(I),I=1,10) 
          PRINT 11000,(XINTP(I),I=1,10),(XINTP(I),I=11,20) 
          PRINT 11000,(XINTP(I),I=21,30),(XINTP(I),I=31,40) 
          PRINT 12000,(XINTP(I),I=41,50) 
          PRINT 11000,(YINTP(I),I=1,10),(YINTP(I),I=11,20) 
          PRINT 11000,(YINTP(I),I=21,30),(YINTP(I),I=31,40) 
          PRINT 12000,(YINTP(I),I=41,50) 
10000 FORMAT(1X,'XP,YP,XINTP,YINTP '/1X,10F13.2/1X,10F13.2/) 
11000 FORMAT(1X,10F13.2/1X,10F13.2) 
12000 FORMAT(1X,10F13.2/) 
! DEBUG                                                                 
      RETURN 
! END OF FILE ENCOUNTERED                                               
 9000 CONTINUE 
      WRITE(OUTP,20000) 
      STOP 13 
10300 FORMAT(// 1X,A8,2I4,I6,43I2) 
20000 FORMAT(// 10X,'UNEXPECTED END OF FILE ENCOUNTERED',               &
     &       ' EXECUTION TERMINATING')                                  
      END                                           
!$RA1UTC                                                                
      SUBROUTINE RA1UTC(A1UTC,A1UTCR,A1UTCD,MAXDIM,NMASTS) 
!***********************************************************************
! NAME              RA1UTC                                              
! VERSION 7902.0    DATE 79/02/01       PGMR - BILL EDDY                

! FUNCTION          MERGES THE A1-UTC DATA FROM THE UPDATE FILE WITH THE
!                   A1-UTC DATA FROM THE OLD MASTER FILE AND OUTPUTS THE
!                   A1-UTC DATA ON THE NEW MASTER FILE AND THE GEODYN   
!                   WORKING FILE. ALSO OUTPUTS IN MASTER FILE FORMAT    
!                   THE A1-UTC CARDS TO BE ADDED TO THE OLD MASTER CARD 
!                   DECK AS A RESULT OF THE NEW UPDATES                 
! INPUT PARAMETERS                                                      
!                   A1UTC     ARRAY USED FOR A1-UTC DATA                
!                   A1UTCR    ARRAY USED FOR A1-UTC RATE DATA           
!                   A1UTCD    ARRAY USED FOR A1-UTC TIME(IN DAYS FROM   
!                             660101) DATA                              
!                   MAXDIM    MAXIMUM NUMBER OF DOUBLE WORDS AVAILABLE  
!                             FOR A1UTC ARRAYS                          
! OUTPUT PARAMETERS                                                     
!                   NMASTS    NUMBER OF A1-UTC VALUES IN TABLE AFTER    
!                             UPDATING                                  
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      LOGICAL BIHNEW,OVRIDE,PRNTMS,PRNTGD,SLFXPR 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      INTEGER FA1UTC,FPOLE,FFLUXS,FFLXAP,FFLXKP,FFLXKP8 
      INTEGER FFXAPP,FFXKP8P,FFXKPP 
      CHARACTER*6 XMF,UPDF,XNMA6 
      CHARACTER*8 HEADR1 
      DIMENSION A1UTC(1),A1UTCR(1),A1UTCD(1),XMF(2),UPDF(2) 
      COMMON/CARDS /NMCDS(6),NUPCDS(6),NSKIP(6) 
      COMMON/CTIME/DAYREF(11),IYREF 
      COMMON/HEADER/FA1UTC,LA1UTC,FPOLE,LPOLE,FFLUXS,LFLUXS,            &
     &              FFLXAP,LFLXAP,FFLXKP8,LFLXKP8,FFLXKP,LFLXKP,        &
     &              NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP             
      COMMON/OPTION/OVRIDE,PRNTMS,PRNTGD,BIHNEW,SLFXPR,LNOBIH 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      DATA HEADR1/'A1UTC   '/,ISECT/1/,NREP/0/,IDUM/0/ 
      DATA XMF/'MASTER',' FILE '/,UPDF/'UPDATE',' FILE '/ 
      DATA EPS/1.0D-5/,RDUM/0.0D0/ 
! CLEAR ARRAYS                                                          
!GEN DFLOAT                                                             
      CALL CLEAR(A1UTC,2*MAXDIM) 
      CALL CLEAR(A1UTCR,2*MAXDIM) 
      CALL CLEAR(A1UTCD,2*MAXDIM) 
! READ OLD MASTER FILE                                                  
      READ(OLDMST,10000,END=9000) XNMA6 
      IF(XNMA6.NE.HEADR1) GO TO 9100 
      NMASTS=NMCDS(ISECT) 
      DO 200 I=1,NMASTS 
      READ(OLDMST,10500,END=9000) IYMD,IHM,ISEC,A1UTC(I),A1UTCR(I),     &
     &   A1UTCD(I)                                                      
      IF(IYMD.LT.500101) IYMD=IYMD+1000000 
  200 END DO 
      IYMDP=IYMD 
! TEST FOR A1UTC UPDATE CARDS                                           
      IF(NUPCDS(ISECT).EQ.0) GO TO 2000 
! POSITION UPDATE FILE FOR READING A1-UTC UPDATES                       
      IF(NSKIP(ISECT).EQ.0) GO TO 300 
      NUPDS=NSKIP(ISECT) 
      DO 250 I=1,NUPDS 
      READ(UPDATE,10000,END=9200) XNMA6 
  250 END DO 
  300 CONTINUE 
      WRITE(OUTP,11200) 
      WRITE(OUTP,11300) 
! READ UPDATE HEADER RECORD                                             
      READ(UPDATE,10000,END=9200) XNMA6 
      IF(XNMA6.NE.HEADR1) GO TO 9300 
      NUPDS=NUPCDS(ISECT) 
      DO 1000 I=1,NUPDS 
      READ(UPDATE,10600,END=9200) IYMD,IHM,ISEC,DA1UTC,DA1UTR 
      IF(IYMD.LT.500101) IYMD=IYMD+1000000 
      IF(IYMD.GT.IYMDP) GO TO 600 
! NEW UPDATE PRECEDES PREVIOUS UPDATES.TEST FOR OVERRIDE OPTION         
      IF(.NOT.OVRIDE) GO TO 9400 
! DETERMINE WHICH A1-UTC VALUE TO REPLACE                               
      IHMS=IHM*100+ISEC 
      CALL DIFF(660101,0,IYMD,IHMS,IDAY,ISECS) 
      DA1UTD=DBLE(IDAY)+DBLE(ISECS)/8.64D4 
      DO 450 J=1,NMASTS 
      IF(ABS(A1UTCD(J)-DA1UTD).LE.EPS) GO TO 500 
  450 END DO 
      WRITE(OUTP,10800) DA1UTD 
      STOP 5 
! REPLACE OLD VALUE                                                     
  500 ICARD=J 
      NREP=NREP+1 
      IF(IYMD.GT.1000000)THEN 
      IYMDPR=IYMD-1000000 
      ELSE 
      IYMDPR=IYMD 
      ENDIF 
      WRITE(OUTP,11000) IYMDPR,IHM,ISEC,DA1UTC,DA1UTR,DA1UTD,           &
     & A1UTC(ICARD),A1UTCR(ICARD)                                       
      A1UTC(ICARD)=DA1UTC 
      A1UTCR(ICARD)=DA1UTR 
      GO TO 1000 
  600 IYMDP=IYMD 
      NMASTS=NMASTS+1 
      A1UTC(NMASTS)=DA1UTC 
      A1UTCR(NMASTS)=DA1UTR 
      IHMS=IHM*100+ISEC 
      CALL DIFF(660101,0,IYMD,IHMS,IDAY,ISECS) 
      A1UTCD(NMASTS)=DBLE(IDAY)+DBLE(ISECS)/8.64D4 
      IF(IYMD.GT.1000000)THEN 
      IYMDPR=IYMD-1000000 
      ELSE 
      IYMDPR=IYMD 
      ENDIF 
      WRITE(OUTP,11000) IYMDPR,IHM,ISEC,DA1UTC,DA1UTR,A1UTCD(NMASTS) 
 1000 END DO 

! WRITE NEW MASTER FILE AND PUNCH UPDATES TO OLD MASTER DECK            

 2000 CONTINUE 
      WRITE(NEWMST,10000) HEADR1,NMASTS 
      IYREF=50 
! SET IBASE=NO. OF DAYS FROM 1950 TO 660101                             
      IBASE=5845 
      NMCD=NMCDS(ISECT) 
!     write(6,*)'a1utcd(iseq)+ibase,iymd,ihm,sec'                       
      DO 2500 ISEQ=1,NMASTS 
      CALL DATE2(A1UTCD(ISEQ)+IBASE,IYMD,IHM,SEC) 
!     write(6,*)a1utcd(iseq)+ibase,iymd,ihm,sec                         
      ISEC=SEC+.1D0 
      IF(IYMD.GT.1000000)THEN 
      IYMDPR=IYMD-1000000 
      ELSE 
      IYMDPR=IYMD 
      ENDIF 
      WRITE(NEWMST,10500) IYMDPR,IHM,ISEC,A1UTC(ISEQ),A1UTCR(ISEQ),     &
     &   A1UTCD(ISEQ),ISEQ                                              
 2500 END DO 
 3000 CONTINUE 

! WRITE NEW GEODYN WORKING FILE                                         

      NVR3=NVRA1C/3 
      NREC=(NMASTS*3-1)/NVRA1C+1 
      NREC1=NREC-1 
      NREM=NMASTS*3-NREC1*NVRA1C 
      WRITE(NEWGDN) HEADR1,NREC,NREM,NMASTS,(IDUM,I=1,43) 
      IF(NREC1.LE.0) GO TO 3600 
! WRITE ALL FULL RECORDS                                                
      ISTART=1 
      ISTOP=NVR3 
      DO 3500 J=1,NREC1 
      WRITE(NEWGDN) (A1UTC(I),A1UTCR(I),A1UTCD(I),I=ISTART,ISTOP) 
      ISTART=ISTART+NVR3 
      ISTOP=ISTOP+NVR3 
 3500 END DO 
! WRITE PARTIAL RECORD.ZERO FILL REMAINING VALUES                       
 3600 IF(NREM.LE.0) GO TO 3700 
      NDUM=NVRA1C-NREM 
      ISTART=NREC1*NVR3+1 
      IF(NDUM.LE.0) WRITE(NEWGDN) (A1UTC(K),A1UTCR(K),A1UTCD(K),K=ISTART&
     &,NMASTS)                                                          
      IF(NDUM.GT.0) WRITE(NEWGDN) (A1UTC(K),A1UTCR(K),A1UTCD(K),K=ISTART&
     &,NMASTS),(RDUM,J=1,NDUM)                                          
 3700 CONTINUE 
      RETURN 
! ENDFILE ON MASTER FILE ENCOUNTERED                                    
 9000 WRITE(OUTP,10100) XMF 
      STOP 99 
! INCORRECT HEADER ON MASTER FILE                                       
 9100 WRITE(OUTP,10200) XMF,HEADR1,XNMA6 
      STOP 99 
! END FILE ON UPDATE FILE ENCOUNTERED                                   
 9200 WRITE(OUTP,10100) UPDF 
      STOP 99 
! INCORRECT HEADER ON UPDATE FILE                                       
 9300 WRITE(OUTP,10200) UPDF,HEADR1,XNMA6 
      STOP 99 
! OVERRIDE OPTION NOT INVOKED                                           
      IF(IYMD.GT.1000000)IYMD=IYMD-1000000 
      IF(IYMDP.GT.1000000)IYMDP=IYMDP-1000000 
 9400 WRITE(OUTP,10700) IYMD,IYMDP 
      STOP 99 
10000 FORMAT(A6,4X,I5) 
10100 FORMAT(5X,'UNEXPECTED END OF FILE ENCOUNTERED WHILE READING ',    &
     & 2A6,' IN SUBROUTINE RA1UTC. EXECUTION TERMINATING')              
10200 FORMAT( 5X,' INCORRECT HEADER RECORD ENCOUNTERED IN SUBROUTINE RA1&
     &UTC WHILE TRYING TO READ ',2A6,2(1X,A6))                          
10500 FORMAT(I6.6,I4.4,I2,2X,D16.9,D15.7,D25.16,2X,I8)   !RA1UTC!
10600 FORMAT(I6,I4,I2,F18.9,D20.12) 
10700 FORMAT( 5X,' DATE ON UPDATE CARD(',I6,') PRECEDES LAST DATE IN TAB&
     &LE(',I6,') AND OVERRIDE OPTION NOT INVOKED. EXECUTION TERMINATING'&
     &)                                                                 
10800 FORMAT(5X,' NO PREVIOUS A1-UTC TIME CORRESPONDING TO',D25.16,'(DAY&
     &S FROM 1950.0) FOUND IN TABLE')                                   
11000 FORMAT(1X,I6,1X,I4,I2,1X,D16.9,D15.7,D25.16,13X,D16.9,D15.7) 
11100 FORMAT(A6,' DUMMY HEADER USED TO INDICATE START OF REPLACED A1UTC &
     &CARDS')                                                           
11200 FORMAT(// 53(1H*),'UPDATES TO A1-UTC TABLES',53(1H*) /32X,        &
     & 'NEW VALUES',50X,'REPLACED VALUES')                              
11300 FORMAT(1X,'YYMMDD HHMMSS',5X,'A1-UTC',10X,'RATE',13X,'DAYS FROM 66&
     &0101',22X,'A1-UTC',10X,'RATE')                                    
      END                                           
!$RDKP3                                                                 
      SUBROUTINE RDKP3(LUOPT,NSKIP,NUPCDS) 
!********1*********2*********3*********4*********5*********6*********7**

! V8803.0           PGMR - BILL EDDY & WEI XIA                          
! V9201.0           PGMR - CRAIG JOHNSON                                

! PURPOSE:          TO UPDATE 3 HOURLY KP VALUES                        

! I/O  PARAMETERS:                                                      

! NAME              DESCRIPTION OF I/O PARAMETERS                       
!  LUOPT    I      FLAG INDICATING WHETHER OR NOT UPDATES ARE BEING DONE
!  NSKIP    I      NUMBER OF RECORDS TO SKIP IN UPDATE FILE             
!  NUPCDS   I      NUMBER OF RECORDS TO READ FROM UPDATE FILE           

! COMMENTS:                                                             

!********************************************************************** 

      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 

      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      CHARACTER CH*1, ACARD*80, FLXKP8*8, FLXKP*8, SECNAM*8, CH1*1 
      LOGICAL LUOPT 

      DIMENSION KPVAL(8),NDAYM(12),CH(8),IBUF2(96), IBUF(48) 
      DIMENSION IUYMD(900), KUPVAL(8,900), IKP3(288) 

      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      EQUIVALENCE (FLXKP,IBUF(1)) 

      DATA IUNT06/6/ 
      DATA ISUMHD/0/,ISUM/0/ 
      DATA NDAYM/31,28,31,30,31,30,31,31,30,31,30,31/ 
      DATA FLXKP8/'FLUXKP8 '/,ZERO/0.0D0/ 


!     SAVE NREC,IRECL                                                   

! READ UPDATES IF PRESENT                                               

      IF(LUOPT) THEN 
         REWIND UPDATE 

! SKIP UPDATES UNTIL 3 HOUR KP SECTION                                  

         NCDS = NSKIP 
         IF(NCDS.GT.0) THEN 
            DO 10 I = 1, NCDS 
            READ(UPDATE,'(A80)',END=9300) ACARD 
   10       CONTINUE 
         ENDIF 

! READ AND STORE UPDATES                                                

         READ(UPDATE,'(A8)',END=9200) SECNAM 
                                               !ERROR EXIT              
         IF(SECNAM.NE.FLXKP8) GO TO 9300 
         NUCDS = NUPCDS 

         DO 30 I = 1, NUCDS 
            READ(UPDATE,10000,END=9200) IUYMD(I),(KPVAL(J),CH(J),J=1,8),&
     &      KPSUM,CH1                                                   
            IF(IUYMD(I).LT.500101) IUYMD(I)=IUYMD(I)+1000000 
            IY=IUYMD(I)/10000 
            IYM=IUYMD(I)/100 
            IM=MOD(IYM,100) 
            CALL MODJUL(TIME,IUYMD(I),0,ZERO) 
            MJDNEW=TIME 
            IF(I.NE.1.AND.(MJDNEW-ITEMP).NE.1) GOTO 9900 
            ITEMP=MJDNEW 
            IUYMDP=IUYMD(I) 
            IF(CH1.EQ.'+') THEN 
              KPSUM = KPSUM*100 + 33 
            ELSE IF(CH1.EQ.'-') THEN 
              KPSUM = KPSUM*100 - 33 
            ELSE 
              KPSUM = KPSUM*100 
            ENDIF 
            ISUM=0 
            DO 20 J = 1, 8 
               IF(CH(J).EQ.'+') THEN 
                  KUPVAL(J,I) = KPVAL(J)*100 + 33 
               ELSE IF(CH(J).EQ.'-') THEN 
                  KUPVAL(J,I) = KPVAL(J)*100 - 33 
               ELSE 
                  KUPVAL(J,I) = KPVAL(J)*100 
               ENDIF 
            ISUM=ISUM+KUPVAL(J,I) 
   20       CONTINUE 
         IF(ABS(ISUM-KPSUM).GT.3)GOTO 9800 
   30    CONTINUE 
      ENDIF 

      IUCRD = 1 

! READ OLD MASTER FILE AND APPLY UPDATES                                

      READ(OLDMST,11000,END=9400) SECNAM, NMASTS 
      IF(SECNAM.NE.FLXKP8) GO TO 9500 

      CALL ZEROI(IBUF2,96) 

      INMASTS = 0 
      DO 1000 IREC = 1, NMASTS 
         READ(OLDMST,12000,END=9400) IYMD,(KPVAL(I),I=1,8),ISEQ 
         IF(IYMD.LT.500101) IYMD=IYMD+1000000 
         IF(ISEQ.NE.IREC) GO TO 9600 

! SEE IF WE HAVE REACHED TIME OF UPDATES; IF SO APPLY UPDATES           

         IF(LUOPT) THEN 
            IF(IYMD.EQ.IUYMD(IUCRD)) THEN 
            IF(IYMD.GT.1000000)THEN 
            IYMDPR=IYMD-1000000 
            ELSE 
            IYMDPR=IYMD 
            ENDIF 
            IF(IUYMD(IUCRD).GT.1000000)THEN 
            IPUYMD=IUYMD(IUCRD)-1000000 
            ELSE 
            IPUYMD=IUYMD(IUCRD) 
            ENDIF 
            WRITE(6,20000) IYMDPR,(KPVAL(I),I=1,8),IPUYMD,              &
     &                    (KUPVAL(I,IUCRD),I=1,8)                       
20000 FORMAT(' REPLACING VALUES AT '/1X,I6,8I4/'WITH'/                  &
     &       1X,I6,8I4)                                                 
               DO 40 I = 1, 8 
                  KPVAL(I) = KUPVAL(I,IUCRD) 
   40          CONTINUE 
               IUCRD = IUCRD + 1 
            ENDIF 
         ENDIF 

         IYM = IYMD/100 
         IY = IYM/100 
         IM = MOD(IYM,100) 
         ID = MOD(IYMD,100) 

         IF(IM.NE.ILMON) THEN 
            ISUMHD = 0 
            ILMON = IM 
         ENDIF 

         NDAYS = NDAYM(IM) 
         IF(IM.EQ.2 .AND. MOD(IY,4).EQ.0) NDAYS = 29 

! LOAD BUFFER                                                           

         INMAST = INMAST + 1 
         INDX = (ID - 1)/12 + 1 

         GO TO (100,200,300), INDX 

!* FOR DAYS 1-12                                                        
  100    CONTINUE 
            IND=(ID-1)*8 
            DO 150 I=1,8 
               ISUMHD=ISUMHD+KPVAL(I) 
               IBUF2(IND+I)=KPVAL(I) 
  150       CONTINUE 
            IF(ID.EQ.12) GOTO 450 
         GO TO 500 
!* FOR DAYS 13-24                                                       
  200    CONTINUE 
            IND=(ID-13)*8 
            DO 250 I=1,8 
               IBUF2(IND+I)=KPVAL(I) 
               ISUMHD=ISUMHD+KPVAL(I) 
  250       CONTINUE 
            IF(ID.EQ.24) GOTO 450 
         GO TO 500 
!* FOR DAYS 25-31                                                       
  300    CONTINUE 
            IF(ID.GT.NDAYS) GOTO 450 
            IND=(ID-25)*8 
            DO 350 I=1,8 
               IBUF2(IND+I)=KPVAL(I) 
               ISUMHD=ISUMHD+KPVAL(I) 
  350       CONTINUE 
            IF(ID.EQ.NDAYS) GOTO 400 
         GO TO 500 

  400    CONTINUE 
         IBUF2(95)=ISUMHD 
         IF(IYM.GT.10000)THEN 
         IBUF2(96)=IYM-10000 
         ELSE 
         IBUF2(96)=IYM 
         ENDIF 

  450    CONTINUE 
         WRITE(ISCRTCH) IBUF2 
         NSCRTCH = NSCRTCH + 1 
         CALL ZEROI(IBUF2,96) 

  500    CONTINUE 
 1000 END DO 

! SEE IF MORE UPDATES REMAIN (THESE ARE ADDING TO THE FILE)             

      IF(IUCRD.GE.NUCDS) GO TO 3000 
         WRITE(6,*) ' ADDING DATA TO TABLE (KP3) ' 

      DO 2000 IREC = IUCRD, NUCDS 

         IYMD = IUYMD(IREC) 
         IYM = IYMD/100 
         IY = IYM/100 
         IM = MOD(IYM,100) 
         ID = MOD(IYMD,100) 

         IF(IM.NE.ILMON) THEN 
            ISUMHD = 0 
            ILMON = IM 
         ENDIF 

         NDAYS = NDAYM(IM) 
         IF(IM.EQ.2 .AND. MOD(IY,4).EQ.0) NDAYS = 29 

         IF(IYMD.GT.1000000)THEN 
         IYMDPR=IYMD-1000000 
         ELSE 
         IYMDPR=IYMD 
         ENDIF 
         WRITE(6,'(1X,I6,8I4)') IYMDPR,(KUPVAL(I,IREC),I=1,8) 

! LOAD BUFFER                                                           

         INMAST = INMAST + 1 
         INDX = (ID - 1)/12 + 1 

         GO TO (1100,1200,1300), INDX 

!* FOR DAYS 1-12                                                        
 1100    CONTINUE 
            IND=(ID-1)*8 
            DO 1150 I=1,8 
               IBUF2(IND+I)=KUPVAL(I,IREC) 
               ISUMHD=ISUMHD+KUPVAL(I,IREC) 
 1150       CONTINUE 
            IF(ID.EQ.12) GO TO 1450 
         GO TO 1500 
!* FOR DAYS 13-24                                                       
 1200    CONTINUE 
            IND=(ID-13)*8 
            DO 1250 I=1,8 
               IBUF2(IND+I)=KUPVAL(I,IREC) 
               ISUMHD=ISUMHD+KUPVAL(I,IREC) 
 1250       CONTINUE 
            IF(ID.EQ.24) GO TO 1450 
         GO TO 1500 
!* FOR DAYS 25-31                                                       
 1300    CONTINUE 
            IF(ID.GT.NDAYS) GO TO 1450 
            IND=(ID-25)*8 
            DO 1350 I=1,8 
               IBUF2(IND+I)=KUPVAL(I,IREC) 
               ISUMHD=ISUMHD+KUPVAL(I,IREC) 
 1350       CONTINUE 
            IF(ID.EQ.NDAYS) GO TO 1400 
         GO TO 1500 

 1400    CONTINUE 
         IBUF2(95)=ISUMHD 
         IF(IYM.GT.10000)THEN 
         IBUF2(96)=IYM-10000 
         ELSE 
         IBUF2(96)=IYM 
         ENDIF 

 1450    CONTINUE 
         WRITE(ISCRTCH) IBUF2 
         NSCRTCH = NSCRTCH + 1 
         CALL ZEROI(IBUF2,96) 

 1500    CONTINUE 
 2000 END DO 

! END OF UPDATING SEE IF WE ENDED ON THE LAST DAY OF THE MONTH          
! IF NOT ZERO FILL TO THE END                                           

 3000 CONTINUE 

      IYMDLT=IYMD 
      IYMLT=IYMDLT/100 
      IF(ID.EQ.NDAYS) GOTO 4000 
      IDAYS = NDAYS - ID 
      INMAST = INMAST + IDAYS 

      IYMDLT=IYM*100+NDAYS 
      INDX2=(ID-1)/12+1 

      GO TO (3100,3200,3300),INDX2 
!* FOR DAYS 1-12                                                        
 3100 CONTINUE 
         IF(ID.EQ.12) GOTO 3120 
         IND1=ID*8 
 3110    CONTINUE 
            IND1=IND1+1 
            IBUF2(IND1)=0 
         IF(IND1.LT.96) GOTO 3110 

         WRITE(ISCRTCH) IBUF2 
         NSCRTCH = NSCRTCH + 1 
 3120    CONTINUE 
         CALL ZEROI(IBUF2,96) 
         WRITE(ISCRTCH) IBUF2 
         NSCRTCH = NSCRTCH + 1 
         IBUF2(95)=ISUMHD 
         IF(IYMLT.GT.10000)THEN 
         IBUF2(96)=IYMLT-10000 
         ELSE 
         IBUF2(96)=IYMLT 
         ENDIF 
         WRITE(ISCRTCH) IBUF2 
         NSCRTCH = NSCRTCH + 1 
         GOTO 4000 
!* FOR DAYS 13-24                                                       
 3200 CONTINUE 
         IF(ID.EQ.24) GOTO 3220 
         IND2=(ID-12)*8 
 3210    CONTINUE 
            IND2=IND2+1 
            IBUF2(IND2)=0 
         IF(IND2.LT.96) GOTO 3210 

         WRITE(ISCRTCH) IBUF2 
         NSCRTCH = NSCRTCH + 1 
 3220    CONTINUE 
         CALL ZEROI(IBUF2,96) 
         IBUF2(95)=ISUMHD 
         IF(IYMLT.GT.10000)THEN 
         IBUF2(96)=IYMLT-10000 
         ELSE 
         IBUF2(96)=IYMLT 
         ENDIF 
         WRITE(ISCRTCH) IBUF2 
         NSCRTCH = NSCRTCH + 1 
         GOTO 4000 
!* FOR DAYS 25-31                                                       
 3300 CONTINUE 
         IND3=(ID-24)*8 
 3310    CONTINUE 
            IND3=IND3+1 
            IBUF2(IND3)=0 
         IF(IND3.LT.93) GOTO 3310 
         IBUF2(95)=ISUMHD 
         IF(IYMLT.GT.10000)THEN 
         IBUF2(96)=IYMLT-10000 
         ELSE 
         IBUF2(96)=IYMLT 
         ENDIF 
         WRITE(ISCRTCH) IBUF2 
         NSCRTCH = NSCRTCH + 1 
         GOTO 4000 

 4000 CONTINUE 

! WRITE HEADER RECORDS FOR NEW BINARY AND MASTER FILES                  

      IDUM = 0 
      NKPVAL = INMAST*8 
      FLXKP = FLXKP8 
      IBUF(3) = NSCRTCH 
      IBUF(4) = 0 
      IBUF(5) = NKPVAL 
      WRITE(NEWGDN) IBUF 
      WRITE(NEWMST,11000) FLXKP8, INMAST, NKPVAL 

! READ SCRATCH FILE AND WRITE NEW BINARY AND MASTER FILES               

      REWIND ISCRTCH 
      NREC = NSCRTCH/3 
      ISEQ = 0 
      DO 5000 I=1,NREC 
         INDX = 1 
         KPSUM = 0 
         DO 4200 J = 1, 3 
            READ(ISCRTCH,END=9700) IBUF2 
            WRITE(NEWGDN) IBUF2 
            DO 4100 K = 1, 96 
               KPSUM = KPSUM + IBUF2(K) 
               IKP3(INDX) = IBUF2(K) 
               INDX = INDX + 1 
 4100       CONTINUE 
 4200    CONTINUE 
         KPSUM = KPSUM - (IBUF2(95) + IBUF2(96)) 

         IYMD = IBUF2(96)*100 + 1 
         IY = IBUF2(96)/100 
         IM = MOD(IBUF2(96),100) 
         NDAY = NDAYM(IM) 
         IF(IM.EQ.2 .AND. (MOD(IY,4).EQ.0)) NDAY = NDAY + 1 
         IF(IYMD.LT.500101)IYMD=IYMD+1000000 
         CALL MODJUL(DMJD,IYMD,0,0.0D0) 
         MJD = DMJD 

         DO 4300 J = 1, NDAY 
            ISTRT = (J - 1)*8 + 1 
            ISTP = ISTRT + 7 
            ISEQ = ISEQ + 1 
            DMJD = MJD 
            CALL GREGOR(DMJD,IYMD,IHM,SEC) 
            IF(IYMD.GT.1000000)THEN 
            IYMDPR=IYMD-1000000 
            ELSE 
            IYMDPR=IYMD 
            ENDIF 
            WRITE(NEWMST,12000) IYMDPR,(IKP3(K),K=ISTRT,ISTP),ISEQ 
            MJD = MJD + 1 
 4300    CONTINUE 
 5000 END DO 

      RETURN 

! ERROR EXITS                                                           

 9200 CONTINUE 
      WRITE(6,*) ' UNEXPECTED EOF ON UPDATE FILE SECTION(KP3) ' 
      STOP 16 

 9300 CONTINUE 
      WRITE(6,*) ' INVALID UPDATE HEADER FOR SECTION(KP3) ',SECNAM 
      STOP 16 

 9400 CONTINUE 
      WRITE(6,*) ' UNEXPECTED EOF ON MASTER FILE SECTION(KP3) ' 
      STOP 16 

 9500 CONTINUE 
      WRITE(6,*) ' INVALID MASTER FILE HEADER SECTION(KP3) ',SECNAM 
      STOP 16 

 9600 CONTINUE 
      WRITE(6,*) ' MASTER FILE OUT OF ORDER SECTION(KP3) ' 
      STOP 16 

 9700 CONTINUE 
      WRITE(6,*) ' UNEXPECTED EOF ON SCRATCH FILE SECTION(KP3) ' 
      STOP 16 

 9800 CONTINUE 
      IF(IUYMD(I).GT.1000000)IUYMD(I)=IUYMD(I)-1000000 
      WRITE(6,*) ' ERROR: SUM OF KP DATA IS NOT EQUAL TO THE REFERENCE  &
     & ',ISUM, KPSUM, ' AT ', IUYMD(I)                                  
      STOP 16 

 9900 CONTINUE 
      IF(IUYMD(I).GT.1000000)IUYMD(I)=IUYMD(I)-1000000 
!3/00 IF(IUYMD(I).GT.1000000)IUYMD=IUYMD(I)-1000000                     
      IF(IUYMDP.GT.1000000)IUYMDP=IUYMDP-1000000 
      WRITE(6,13000) IUYMDP, IUYMD, KPVAL 
      STOP 16 
10000 FORMAT(I6,2X,8(I1,A1,1X),I3,A1) 
11000 FORMAT(A8,1X,3I6) 
12000 FORMAT(I6.6,8I5,26X,I8)   !!KP!!
12001 FORMAT(1X,I6.6,8I5,26X,I8) 
13000 FORMAT(1X,' ERROR: INVALID TIME ORDER; YYMMDD & KPVAL= ',2I8,8I5) 

      END                                           
!$RFLUX                                                                 
      SUBROUTINE RFLUX(FLUX,MAXDIM,SSARR,MAXDM2,ITYPE) 
!***********************************************************************
! NAME              RFLUX                                               
! VERSION 7902.0    DATE 79/02/01       PGMR - BILL EDDY                

! FUNCTION          MERGES THE  FLUX  DATA FROM THE UPDATE FILE WITH THE
!                   FLUX   DATA FROM THE OLD MASTER FILE AND OUTPUTS THE
!                   FLUX   DATA ON THE NEW MASTER FILE AND THE GEODYN   
!                   WORKING FILE. ALSO OUTPUTS IN MASTER FILE FORMAT    
!                   THE  FLUX  CARDS TO BE ADDED TO THE OLD MASTER CARD 
!                   DECK AS A RESULT OF THE NEW UPDATES. USED FOR BOTH  
!                   SOLAR FLUX AND MAGNETIC FLUX DATA                   
! INPUT PARAMETERS                                                      
!                   FLUX      ARRAY USED TO STORE FLUX DATA             
!                   MAXDIM    MAXIMUM NUMBER OF I*4 WORDS AVAILABLE     
!                             FOR FLUX DATA                             
!                   SSARR     ARRAY USED TO STORE SUNSPOT DATA          
!                   MAXDIM2   MAXIMUM NUMBER OF I*4 WORDS AVAILABLE     
!                             FOR SUNSPOT DATA                          
!                   ITYPE     FLUX TYPE INDICATOR                       
!                              1 - SOLAR FLUX                           
!                              2 - MAGNETIC FLUX AP DATA                
!                              3 - MAGNETIC FLUX KP DATA                
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      LOGICAL PUNCHS,LNOBIH 
      LOGICAL BIHNEW,OVRIDE,PRNTMS,PRNTGD,SLFXPR 
      LOGICAL OVRFLW 
      INTEGER FLUX,IDUM2,SSARR 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      INTEGER FA1UTC,FPOLE,FFLUXS,FFLXAP,FFLXKP,FFLXKP8 
      INTEGER FFXAPP,FFXKP8P,FFXKPP 
      INTEGER FFLUX,FLXARR 
      INTEGER SFDATE,STARTD,STYM,SSDATE 
      CHARACTER*6 XNMA6,XMF,UPDF 
      CHARACTER*8 HEADR 

      DIMENSION FLUX(1),SSARR(1) 
      DIMENSION FLXARR(32) 
      DIMENSION MONTHS(12), HEADR(3), FFLUX(4), FLUXA(7), NVRFLX(3) 
      DIMENSION XMF(2),UPDF(2) 

      COMMON/BADINP/MNTHBD(12,2),NBFLUX(3) 
      COMMON/OVRFLW/OVRFLW(2) 
      COMMON/CARDS /NMCDS(6),NUPCDS(6),NSKIP(6) 
      COMMON/DAYMTH/NDAYS(12) 
      COMMON/HEADER/FA1UTC,LA1UTC,FPOLE,LPOLE,FFLUXS,LFLUXS,            &
     &              FFLXAP,LFLXAP,FFLXKP8,LFLXKP8,FFLXKP,LFLXKP,        &
     &              NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP             
      COMMON/OPTION/OVRIDE,PRNTMS,PRNTGD,BIHNEW,SLFXPR,LNOBIH 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      COMMON/SOLARD/SFDATE,SSDATE,NSSSTD,NSSNM,NSUNV 
      EQUIVALENCE(FFLUX(1),FFLUXS),(NVRFLX(1),NVRFS) 

      DATA XMF/'MASTER',' FILE '/,UPDF/'UPDATE',' FILE '/ 
      DATA IZERO,IDUM,IDUM2/3*0/ 
      DATA MONTHS/31,28,31,30,31,30,31,31,30,31,30,31/ 
      DATA HEADR/'FLUXS   ','FLUXAP  ','FLUXKP  '/ 


!GEN DFLOAT                                                             
      NREP=0 
      NBFLUX(ITYPE)=0 
      FLXSUM=0. 
      IHM=0 
      SEC=0.0D0 

! DETERMINE MONTH AND YEAR OF START AND END DATES OF TABLE              

      IYRSTR=FFLUX(ITYPE*2-1)/10000 
      IMNSTR=(FFLUX(ITYPE*2-1)-IYRSTR*10000)/100 
      IYREND=FFLUX(ITYPE*2)/10000 
      IMNEND=(FFLUX(ITYPE*2)-IYREND*10000)/100 

! SET SECTION NUMBER                                                    

      ISECT=2+ITYPE 

! CLEAR FLUX ARRAY                                                      

      NCLEAR=MAXDIM 
      CALL CLEAR(FLUX,NCLEAR) 

! READ OLD MASTER FILE                                                  

      READ(OLDMST,10000,END=9000) XNMA6 
      IF(XNMA6.NE.HEADR(ITYPE)) GO TO 9100 
      NCDS=NMCDS(ISECT) 
      NMASTS=(NCDS/3) 
      NMNTHS=NMASTS 

      I=0 
      DO 200 J=1,NCDS,3 
         I=I+1 
         NPREV=(I-1)*32 
         ISTRT1=NPREV+1 
         ISTOP1=ISTRT1+11 
         ISTRT2=ISTOP1+1 
         ISTOP2=ISTRT2+11 
         ISTRT3=ISTOP2+1 
         ISTOP3=ISTRT3+7 
! READ 32 OF THE 36 FLUX VALUES ON MASTER FILE PER MONTH                
         READ(OLDMST,10500,END=9000) IYMD,(FLUX(K1),K1=ISTRT1,ISTOP1),  &
     &        IYMD2,(FLUX(K2),K2=ISTRT2,ISTOP2),                        &
     &        IYMD3,(FLUX(K3),K3=ISTRT3,ISTOP3)                         
         IF(IYMD.LT.500101) IYMD=IYMD+1000000 
         IF(IYMD2.LT.500101) IYMD2=IYMD2+1000000 
         IF(IYMD3.LT.500101) IYMD3=IYMD3+1000000 
! SET 32ND VALUE=YYMM                                                   
              IF(IYMD.GT.1000000)THEN 
              IYMDFX=IYMD-1000000 
              ELSE 
              IYMDFX=IYMD 
              ENDIF 
              FLUX(I*32)=IYMDFX/100 
  200 END DO 

      IYM = IYMD3/100 
      IMNTHP = MOD(IYM,100) 
      IY = IYM/100 
      IDAY = MONTHS(IMNTHP) 
      IF(IMNTHP.EQ.2 .AND. MOD(IY,4).EQ.0) IDAY = 29 
      IYMDP = IYM*100 + IDAY 

! SET POINTER FOR START OF NEW UPDATES                                  
      ISTRT=NMASTS+1 
! TEST FOR FLUX UPDATE CARDS                                            
      IF(NUPCDS(ISECT).EQ.0) GO TO 2010 
! POSITION UPDATE FILE FOR READING FLUX UPDATES                         
      IF((NSKIP(ISECT-1)+NUPCDS(ISECT-1)+1).EQ.NSKIP(ISECT)) GO TO 300 
      REWIND UPDATE 
      IF(NSKIP(ISECT).EQ.0) GO TO 300 

      NUPDS=NSKIP(ISECT) 
      DO 250 I=1,NUPDS 
      READ (UPDATE,10000,END=9200) XNMA6 
  250 END DO 

! WRITE TITLE FOR UPDATE PRINTOUT                                       

  300 CONTINUE 
      READ(UPDATE,10000,END=9200) XNMA6,INORMAL 
      IF(XNMA6.NE.HEADR(ITYPE)) GO TO 9300 

      NUPDS=NUPCDS(ISECT) 
      FLXSUM = 0 
      READ(UPDATE,10650,END=9200) IYMD, FLUXA 
      IF(IYMD.LT.500101) IYMD=IYMD+1000000 
      IF(FLUXA(2).EQ.0.) THEN 
      FLUXT=FLUXA(1) 
      WRITE(OUTP,10400) HEADR(ITYPE) 
      DO 1000 LCNT=1,NUPDS 
         IF(LCNT.NE.1) READ(UPDATE,10600,END=9200) IYMD,FLUXT 
         IF(IYMD.LT.500101) IYMD=IYMD+1000000 
! IF UPDATES COMING FROM SGAS DAILY REPORTS NORMALIZE FLUXES TO 1 AU    

         IF(INORMAL.GT.0) THEN 
            IFLUXT = FLUXT 
            CALL FLUXNORM(IYMD,IFLUXT) 
            FLUXT = IFLUXT 
         ENDIF 

         IYR=IYMD/10000 
         IMNTH=(IYMD-IYR*10000)/100 
         IDAY=IYMD-IYR*10000-IMNTH*100 
         MAXDAY=MONTHS(IMNTH) 
         IF(IMNTH.EQ.2.AND.MOD(IYR,4).EQ.0) MAXDAY=29 
         NMNTHS=(IYR-IYRSTR)*12-IMNSTR+IMNTH 
         IPTR=NMNTHS*32 
         IPTRF = IPTR + IDAY 
! TEST IF NEW UPDATE PRECEDES PREVIOUS UPDATE                           
         IF(IYMD.LE.IYMDP .AND. (.NOT.OVRIDE)) GO TO 9400 
         IF(IYMD.LE.IYMDP) THEN 
            TFLUX = FLUX(IPTRF)/10.0 
            UFLUX = FLUXT/10.0 
            IF(IYMD.GT.1000000)THEN 
            IYMDPR=IYMD-1000000 
            ELSE 
            IYMDPR=IYMD 
            ENDIF 
            WRITE(OUTP,10450) IYMDPR,TFLUX,UFLUX 
         ELSE 
            UFLUX = FLUXT/10.0 
            IF(IYMD.GT.1000000)THEN 
            IYMDPR=IYMD-1000000 
            ELSE 
            IYMDPR=IYMD 
            ENDIF 
            WRITE(OUTP,10460) IYMDPR,UFLUX 
         ENDIF 
         IF(IYMD.GT.1000000)THEN 
           IYMDFX=IYMD-1000000 
         ELSE 
           IYMDFX=IYMD 
         ENDIF 
         FLUX(IPTR+32) = IYMDFX/100 
         FLUX(IPTRF) = FLUXT 
         IF(I.EQ.1) ISTART = IPTRF 
         FLXSUM = FLXSUM + FLUXT 
 1000 END DO 

! GET AVERAGE OF FLUXES                                                 

      FLXAVG = FLXSUM/NUPDS 

      IYREND = IYR 
      IMNEND = IMNTH 

! PREDICT FLUXES TO THE END OF THE MONTH IF REQUIRED                    

      IF(IDAY.LT.MAXDAY) THEN 

         ISTART = IYMD 

! SET POINTERS INTO ARRAYS                                              

         NPTR = IPTRF + 1 
                                     !LAST REAL FLUX VALUE IN TABLE     
         FLTFLX = FLUX(IPTRF) 
         TINTVL = 1.0D0 

         CALL MODJUL(TBLAST,ISTART,IHM,SEC) 

                                 !SET FIRST DATE OF PREDICTIONS         
         CALL ADDYMD(ISTART,1) 

! PREDICT                                                               

         IYR = ISTART/10000 
         IMO = MOD(ISTART,10000)/100 
         IDAY = MOD(ISTART,100) 
         NDAY = MONTHS(IMO) 
         IF(IMO.EQ.2 .AND. MOD(IYR,4).EQ.0) NDAY = 29 
         ISTOP = (IYR*100 + IMO)*100 + NDAY 
         NPTR = NPTR - IDAY + 1 

         NFLUXS = NFLUXS + (NDAY - IDAY) + 1 

         DO 1100 K = IDAY, NDAY 
            KM1 = K - 1 
            FLUX(NPTR+KM1) = 0 
 1100    CONTINUE 

!        ISTART = ISTART/100*100 + 1    !SET START OF PRED TO DAY1      
                                             !START TIME IN MJD         
         CALL MODJUL(TSTART,ISTART,IHM,SEC) 
                                             !STOP TIME IN MJD          
         CALL MODJUL(TSTOP,ISTOP,IHM,SEC) 

         IF(ITYPE.EQ.1) THEN 
            CALL FLXPRD(TBLAST,FLTFLX,TSTART,TSTOP,TINTVL,              &
     &                  FLXARR,SSARR(NPTR),NDAY)                        
            M=0 
            DO 1110 K = IDAY, NDAY 
               M=M+1 
               KM1 = K - 1 
               FLUX(NPTR+KM1) = FLXARR(M) 
 1110       CONTINUE 
         ELSE 
            DO 1150 K = IDAY, NDAY 
               KM1 = K - 1 
               FLUX(NPTR+KM1) = FLXAVG 
 1150       CONTINUE 
         ENDIF 

         IF(ISTART.GT.1000000)THEN 
         ISTRFX=ISTART-1000000 
         ELSE 
         ISTRFX=ISTART 
         ENDIF 
         FLUX(NPTR+31) = ISTRFX/100 

      ENDIF 

      ELSE 

      IYMDP=IYMD3/100*100+29 
      IMNTHP=IYMDP/100-IYMDP/10000*100 
      IDAYP=1 
      WRITE(OUTP,10410) HEADR(ITYPE) 
      DO 2000 LCNT=1,NUPDS 
      IF(LCNT.NE.1) READ(UPDATE,10650,END=9200) IYMD,FLUXA 
      IF(IYMD.LT.500101) IYMD=IYMD+1000000 
      IYR=IYMD/10000 
      IMNTH=(IYMD-IYR*10000)/100 
      IDAY=IYMD-IYR*10000-IMNTH*100 
      MAXDAY=MONTHS(IMNTH) 
      IF(IMNTH.EQ.2.AND.MOD(IYR,4).EQ.0) MAXDAY=29 
! CALCULATE NUMBER OF MONTHS FROM START OF TABLE-32 VALUES STORED PER MO
      NMNTHS=(IYR-IYRSTR)*12-IMNSTR+IMNTH 
      IPTR=NMNTHS*32 
! TEST IF NEW UPDATE PRECEDES PREVIOUS UPDATE                           
      IF(IYMD.GT.IYMDP) GO TO 600 
! TEST FOR OVERRIDE OPTION                                              
      IF(.NOT.OVRIDE) GO TO 9400 
! DETERMINE FLUX VALUES TO BE REPLACED                                  
      ISTART=IPTR+IDAY 
      ISTOP=MIN(IDAY+6,MAXDAY)+IPTR 
      IF(IYMD.GT.1000000)THEN 
      IYMDPR=IYMD-1000000 
      ELSE 
      IYMDPR=IYMD 
      ENDIF 
      WRITE(OUTP,10900) IYMDPR,FLUXA,IYMDPR,(FLUX(K),K=ISTART,ISTOP) 
! REPLACE OLD VALUES                                                    
      J=0 
      DO 500 I=ISTART,ISTOP 
      J=J+1 
      FLUX(I)=FLUXA(J)+.1D0 
  500 END DO 
      GO TO 2000 
! TEST IF UPDATE CARDS IN ORDER                                         
  600 IF(IDAY.EQ.IDAYP+7.AND.IMNTH.EQ.IMNTHP) GO TO 650 
      IF(IDAY.EQ.1.AND.IMNTH.EQ.IMNTHP+1) GO TO 650 
      IF(IYMD-IYMDP .EQ.8872) GO TO 650 
      IF(IYMD.GT.1000000)IYMD=IYMD-1000000 
      WRITE (OUTP,11300) HEADR(ITYPE),IYMD 
      STOP 
! ADD NEW FLUX VALUES                                                   
  650 IDAYP=IDAY 
      IMNTHP=IMNTH 
      IYMDP=IYMD 
      IPTRDY=IPTR+IDAY-1 
      IF(IDAY.NE.29) GO TO 680 
      IAVGFX=FLUXA(MAXDAY-IDAY+2)+.1D0 
      FLUXA(MAXDAY-IDAY+2)=0.0D0 
  680 CONTINUE 
      DO 700 I=1,7 
      FLXSUM=FLXSUM+FLUXA(I) 
      IF(FLUXA(I).GT.0.0D0) FLUX(IPTRDY+I)=FLUXA(I)+.1D0 
  700 END DO 
! LOAD YYMM INTO FLUX(32)                                               
      IYM=IYMD/100 
      IF(IYM.GT.10000)THEN 
      IYMFX=IYM-10000 
      ELSE 
      IYMFX=IYM 
      ENDIF 
      FLUX(IPTR+32)=IYMFX 
      IF(IDAY.NE.29) GO TO 750 
      IFLXSM=FLXSUM/DBLE(MAXDAY)+.5D0 
      FLXSUM=0.0D0 
      IF(IYMD.GT.1000000)THEN 
      IYMDPR=IYMD-1000000 
      ELSE 
      IYMDPR=IYMD 
      ENDIF 
      WRITE(OUTP,11900) IYMDPR,FLUXA,IAVGFX,IFLXSM 
! set average flux equal to calculated flux                             
      IAVGFX=IFLXSM 
      IF(IAVGFX.EQ.IFLXSM) GO TO 2000 
! BAD INPUT                                                             
      NBFLUX(ITYPE)=NBFLUX(ITYPE)+1 
      IF(NBFLUX(ITYPE).GT.12) THEN 
      OVRFLW(ITYPE)=.TRUE. 
      ELSE 
      OVRFLW(ITYPE)=.FALSE. 
      IF(IYMD.GT.1000000)THEN 
      IYMD=IYMDPR-1000000 
      ELSE 
      IYMD=IYMDPR 
      ENDIF 
      MNTHBD(NBFLUX(ITYPE),ITYPE)=IYMDPR-28 
      ENDIF 
      GO TO 2000 
  750 CONTINUE 
      IF(IYMD.GT.1000000)THEN 
      IYMDPR=IYMD-1000000 
      ELSE 
      IYMDPR=IYMD 
      ENDIF 
      WRITE(OUTP,10900) IYMDPR,FLUXA 
 2000 END DO 
 2010 CONTINUE 
      ENDIF 

! WRITE NEW MASTER FILE AND PUNCH UPDATES TO OLD MASTER DECK            

      LMNTH=(IYREND-IYRSTR)*12-IMNSTR+IMNEND+1 
      NCDS=LMNTH*3 
      NFLUXS=0 
      ISEQ=0 
      WRITE(NEWMST,10000) HEADR(ITYPE),NCDS 
      DO 1500 I=1,LMNTH 
         IBASE=(I-1)*32 
         IYM=FLUX(IBASE+32) 
         IF(IYM.LT.5001)IYM=IYM+10000 
         IY=IYM/100 
         IM=IYM-IY*100 
         IYMD=IYM*100+1 
         K1 = IBASE+1 
         K2 = IBASE+32 
         IF(IM.EQ.0) STOP 88 
         LDAY=MONTHS(IM) 
         IF(IM.EQ.2.AND.MOD(IY,4).EQ.0) LDAY=29 
         NFLUXS=NFLUXS+LDAY 
         DO 1450 J=1,2 
            ISTART=IBASE+1 
            ISTOP=IBASE+12 
            ISEQ=ISEQ+1 
            IF(IYMD.GT.1000000)THEN 
            IYMDPR=IYMD-1000000 
            ELSE 
            IYMDPR=IYMD 
            ENDIF 
            WRITE(NEWMST,10700) IYMDPR,(FLUX(L),L=ISTART,ISTOP),ISEQ 
            IBASE=IBASE+12 
            IYMD=IYMD+12 
 1450    CONTINUE 
         ISEQ=ISEQ+1 
         ISTART=ISTOP+1 
         ISTOP=ISTART+LDAY-25 
         NZERO=36-LDAY 
         IF(IYMD.GT.1000000)THEN 
         IYMDPR=IYMD-1000000 
         ELSE 
         IYMDPR=IYMD 
         ENDIF 
         WRITE(NEWMST,10700) IYMDPR,(FLUX(L),L=ISTART,ISTOP),           &
     &                       (IZERO,L1=1,NZERO),ISEQ                    
 1500 END DO 
! GENERATE MAGNETIC FLUX DATA FOR THE ONE MONTH LAG IN DATA BETWEEN     
! SOLAR FLUX AND MAGNETIC FLUX                                          
      IF(ITYPE.NE.1) THEN 
      IYRFAP=LFLXAP/10000 
      IMNFAP=(LFLXAP-IYRFAP*10000)/100 
      IYRFS=LFLUXS/10000 
      IMNFS=(LFLUXS-IYRFS*10000)/100 
      ILAG=(IYRFS-IYRFAP)*12 + (IMNFS-IMNFAP) 
      IF(ILAG.EQ.1) THEN 
! SET MAGNETIC FLUX END DATE = SOLAR FLUX END DATE                      
      LFLXAP=LFLUXS 
      MAXDAY=MONTHS(IMNEND) 
      IF(IMNEND.EQ.2.AND.MOD(IYREND,4).EQ.0) MAXDAY=29 
! CALCULATE AVERAGE FLUX VALUE OF LAST MONTH                            
      FLXAVG=0.0D0 
      IBASE=(LMNTH-1)*32 
      DO 3300 I=1,MAXDAY 
      FLXAVG=FLXAVG+FLUX(I+IBASE) 
 3300 END DO 
      FLXAVG=FLXAVG/MAXDAY 
      IBASE=IBASE+32 
      IMNEND=IMNEND+1 
      IF(IMNEND.GT.12) IMNEND=1 
      MAXDAY=MONTHS(IMNEND) 
      IF(IMNEND.EQ.2.AND.MOD(IYREND,4).EQ.0) MAXDAY =29 
      DO 3400 I=1,MAXDAY 
      FLUX(IBASE+I) = FLXAVG 
 3400 END DO 
      IF(LFLXAP.GT.1000000)THEN 
      LFXAPR=LFLXAP-1000000 
      ELSE 
      LFXAPR=LFLXAP 
      ENDIF 
      FLUX(IBASE+32)=LFXAPR/100 
      LMNTH=LMNTH+1 
      ENDIF 

      ENDIF 

! SOLAR FLUX PREDICTION CODE GOES HERE                                  

      IF(ITYPE.EQ.1 .AND. SLFXPR) THEN 

!     DO SUNSPOT PREDICTION FOR SUNSPOT FIRST DATE TO SOLAR FLUX        
!     LAST DATE                                                         

          CALL SUNSET(IYRSTR,IMNSTR,FLUX,SSARR,MAXDM2,LSSPTR) 

       LUPDAT=LFLUXS/100 
       IF(LUPDAT.LT.SFDATE)THEN 
            CALL MODJUL(TBLAST,LFLUXS,IHM,SEC) 
            NY=LUPDAT/100 
            NM=LUPDAT-NY*100 
            JDIM=MONTHS(NM) 
            IF(NM.EQ.2.AND.MOD(NY,4).EQ.0)JDIM=29 
            LENTRY=(LMNTH-1)*32 
!           OPTAINS LAST ENTRY                                          
            FLTFLX=FLUX(LENTRY+JDIM) 
!           RETURNS FIRST START DATE                                    
            CALL NEXTDT(STARTD,LUPDAT) 
            STYM=STARTD/100 
 3000       IF(STYM.GT.SFDATE)GO TO 3100 
                NPTR = LMNTH*32+1 
                LSSPTR = NSSNM*32+1 
                N1Y=STYM/100 
                N1M=STYM-N1Y*100 
!               NUMBER OF DAYS IN MONTH STORED INTO NDIM                
                NDIM=MONTHS(N1M) 
                IF(N1M.EQ.2.AND.MOD(N1Y,4).EQ.0)NDIM=29 
                LASTD=STYM*100+NDIM 
!    CALCULATE TOTAL NUMBER OF ACTUAL DATA                              
                NFLUXS = NFLUXS+NDIM 
                NSUNV = NSUNV+NDIM 
!               RETURNS TSTART IN MJD                                   
                CALL MODJUL(TSTART,STARTD,IHM,SEC) 
!               RETURNS TSTOP IN MJD                                    
                CALL MODJUL(TSTOP,LASTD,IHM,SEC) 
!               SET INTERVAL FOR PREDICTION                             
                TINTVL=1D0 
                DO 2550 K = 0,31 
                   FLUX(NPTR+K) = 0 
 2550           CONTINUE 
                CALL FLXPRD(TBLAST,FLTFLX,TSTART,TSTOP,TINTVL,          &
     &                      FLUX(NPTR),SSARR(LSSPTR),NDIM)              
                IF(STYM.GT.10000)THEN 
                STYMPR=STYM-10000 
                ELSE 
                STYMPR=STYM 
                ENDIF 
                FLUX(NPTR+31)=STYMPR 
                SSARR(LSSPTR+31)=STYM 
                LMNTH = LMNTH+1 
                NSSNM = NSSNM+1 
                CALL NEXTDT(STARTD,STYM) 
                STYM=STARTD/100 
                GO TO 3000 
 3100           CONTINUE 
            ENDIF 
         ENDIF 


 3500 CONTINUE 

! WRITE NEW GEODYN WORKING FILE -FLUX DATA                              

      NREC=(LMNTH*32-1)/NVRFLX(ITYPE)+1 
      NREC1=NREC-1 
      NREM=LMNTH*32-NREC1*NVRFLX(ITYPE) 
      WRITE(NEWGDN) HEADR(ITYPE),NREC,NREM,NFLUXS,(IDUM,I=1,43) 
! WRITE ALL FULL RECORDS                                                
      IF(NREC1.LE.0) GO TO 3900 
      DO 3800 J=1,NREC1 
      ISTART=(J-1)*NVRFLX(ITYPE)+1 
      ISTOP=ISTART+NVRFLX(ITYPE)-1 
      WRITE(NEWGDN) (FLUX(K),K=ISTART,ISTOP) 
 3800 END DO 
! WRITE LAST FLUX RECORD. ZERO FILL REMAINDER OF RECORD                 
 3900 IF(NREM.LE.0) GO TO 4000 
      NDUM=NVRFLX(ITYPE) -NREM 
      ISTART=NREC1*NVRFLX(ITYPE)+1 
      ISTOP=LMNTH*32 
      IF(NDUM.LE.0) WRITE(NEWGDN) (FLUX(K),K=ISTART,ISTOP) 
      IF(NDUM.GT.0) WRITE(NEWGDN) (FLUX(K),K=ISTART,ISTOP),             &
     &     (IDUM2,L=1,NDUM)                                             
 4000 CONTINUE 
      IF(ITYPE.EQ.1 .OR. ITYPE.EQ.3) GO TO 5000 
! CALL FLUXKP TO OBTAIN KP VALUES FROM AP VALUES                        
      NFLUX=LMNTH*32 
      DO 4200 IFLX=1,NFLUX,32 
      IFLX1=IFLX-1 
      DO 4200 J=1,31 
      CALL FLUXKP(FLUX(IFLX1+J)) 
 4200 CONTINUE 
      ITYPE=3 
      GO TO 3500 
 5000 RETURN 
! ENDFILE ON MASTER FILE ENCOUNTERED                                    
 9000 WRITE(OUTP,10100) XMF 
      STOP 16 
! INCORRECT HEADER ON MASTER FILE                                       
 9100 WRITE(OUTP,10200) XMF,HEADR(ITYPE),XNMA6 
      STOP 16 
! END FILE ON UPDATE FILE ENCOUNTERED                                   
 9200 WRITE(OUTP,10100) UPDF 
      STOP 16 
! INCORRECT HEADER ON UPDATE FILE                                       
 9300 WRITE(OUTP,10200) UPDF,HEADR(ITYPE),XNMA6 
      STOP 16 
! OVERRIDE OPTION NOT INVOKED                                           
      IF(IYMD.GT.1000000)IYMD=IYMD-1000000 
      IF(IYMDP.GT.1000000)IYMDP=IYMDP-1000000 
 9400 WRITE(OUTP,10300) IYMD,IYMDP 
      STOP 16 
10000 FORMAT(A6,4X,I5) 
10100 FORMAT(5X,'UNEXPECTED END OF FILE ENCOUNTERED WHILE READING ',    &
     & 2A6,' IN SUBROUTINE RFLUX. EXECUTION TERMINATING')               
10200 FORMAT( 5X,' INCORRECT HEADER RECORD ENCOUNTERED IN SUBROUTINE RFL&
     &UX  WHILE TRYING TO READ ',2A6,2(1X,A6))                          
10300 FORMAT( 5X,' DATE ON UPDATE CARD(',I6,') PRECEDES LAST DATE IN TAB&
     &LE(',I6,') AND OVERRIDE OPTION NOT INVOKED. EXECUTION TERMINATING'&
     &)                                                                 
10400 FORMAT(//50(1H*),'UPDATES TO ',A6,' TABLES',50(1H*)//) 
10410 FORMAT(//50(1H*),'UPDATES TO ',A6,' TABLES',50(1H*)/              &
     & 52X,'AVG MONTHLY FLUX VALUE'/54X,' INPUT',3X,'CALCULATED'//)     
10450 FORMAT(10X,'REPLACING OLD FLUX VALUE AT ',I6,' OLD VALUE = ',     &
     &       F8.1,' NEW VALUE = ',F8.1)                                 
10460 FORMAT(10X,'ADDING FLUX VALUES AT ',I6,' FLUX = ',F8.1) 
10500 FORMAT(2(I6,4X,12I5/),I6,4X,8I5) 
10600 FORMAT(I6,4X,F10.0) 
10650 FORMAT(I6,4X,7F10.0) 
10700 FORMAT (I6.6,4X,12I5,2X,I8)     !!RFLUX
10900 FORMAT(1X,I6,4X,7F6.0,22X,I6,4X,7I5) 
11100 FORMAT(A6,' DUMMY HEADER USED TO INDICATE START OF REPLACED ',    &
     &     A6,' CARDS')                                                 
11300 FORMAT(5X,A6,'UPDATE CARD NOT IN TIME ORDER FOR DATE ',I6,        &
     &     'EXECUTION TERMINATING')                                     
11500 FORMAT(5X,'END DATE OF SOLAR FLUX DATA (',I6,') IS NOT ONE',      &
     &    'MONTH LATER THAN END DATE OF MAGNETIC FLUX DATA (',I6,       &
     &').'/' END DATE OF MAGNETIC FLUX CHANGED TO END DATE OF SOLAR',   &
     &' FLUX.')                                                         
11900 FORMAT(1X,I6,4X,7F6.0,1X,I5,6X,I5,5X,I6,4X,7I5) 
      END                                           
!$RPOLE                                                                 
      SUBROUTINE RPOLE(A1UT1,XP,YP,EOPDEP,EOPDPS,INTVAL,NA1UT1,MAXDIM,  &
     & NUTC,DAYE,DAYB66,DUMMY)                                          
!***********************************************************************
! VERSION 7902.0    PGMR - BILL EDDY                                    
! VERSION 9104.0    PGMR - SCOTT B. LUTHCKE                             
! VERSION 0110.0    PGMR - TERRY WILLIAMS                               

! FUNCTION          MERGES THE POLE/A1UT1 DATA FROM THE UPDATE FILE WITH
!                   THE POLE/A1UT1 DATA FROM THE OLD MASTER FILE AND    
!                   OUTPUTS THE POLE/A1UT1 DATA ON THE NEW MASTER FILE  
!                   AND THE GEODYN WORKING FILE. ALSO OUTPUTS IN MASTER 
!                   FILE FORMAT THE POLE/A1UT1 CARDS TO BE ADDED TO THE 
!                   OLD MASTER DECK AS A RESULT OF THE NEW UPDATES      
! INPUT PARAMETERS                                                      
!                   A1UT1     ARRAY USED FOR A1-UT1 DATA                
!                   XP        ARRAY USED FOR X OF THE POLE DATA         
!                   YP        ARRAY USED FOR Y OF THE POLE DATA         
!                   EOPDEP    ARRAY USED FOR CELECTIAL POLE OFFSETS     
!                             IN LONGITUDE                              
!                   EOPDPS    ARRAY USED FOR CELECTIAL POLE OFFSETS     
!                             IN OBLIQUITY                              
!                   INTVAL    POLE, A1UT1, AND EOP'S INTERVAL IN HRS.   
!                   NA1UT1    NUMBER OF A1UT1, X-POLE, Y-POLE, EOPDEP,  
!                             AND EOPDPS, EACH                          

!  taw - added more precision to pole values (.001 milliarcseconds)     
!        works for units in .1 milliarcseconds and .001 milliarcseconds 
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      LOGICAL BIHNEW,OVRIDE,PRNTMS,PRNTGD,SLFXPR,LNOBIH 
      INTEGER XP,YP,X,Y,IDUM2,XP1,YP1,XP2,YP2 
      INTEGER EOPDEP,EOPDPS,DP,DS,INTVAL 
      INTEGER FA1UTC,FPOLE,FFLUXS,FFLXAP,FFLXKP8,FFLXKP 
      INTEGER FFXAPP,FFXKP8P,FFXKPP 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      CHARACTER*6 XNMA6,XMF,UPDF 
      CHARACTER*8 HEADR1,HEADR2,HEADR3 
      CHARACTER*60 FRMT 
      DIMENSION DUMMY(1) 
      DIMENSION XMF(2),UPDF(2) 
      DIMENSION XP(1),YP(1),A1UT1(1),EOPDEP(1),EOPDPS(1) 
      COMMON/CARDS /NMCDS(6),NUPCDS(6),NSKIP(6) 
      COMMON/HEADER/FA1UTC,LA1UTC,FPOLE,LPOLE,FFLUXS,LFLUXS,            &
     &              FFLXAP,LFLXAP,FFLXKP8,LFLXKP8,FFLXKP,LFLXKP,        &
     &              NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP             
      COMMON/KPOINT/INDD1,INDD2,INDD3 
      COMMON/OPTION/OVRIDE,PRNTMS,PRNTGD,BIHNEW,SLFXPR,LNOBIH 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      COMMON/EYMD/IPDUPD,IPDNMT 
      DATA XMF/'MASTER',' FILE '/,UPDF/'UPDATE',' FILE '/ 
      DATA HEADR1,HEADR2,HEADR3/'POLE    ','A1UT1   ','EOPS    '/ 
      DATA ISECT/2/,NREP/0/,IDUM2/0/,RDUM/0.0D0/,IDUM/0/ 
      DATA PI/3.1415926535897932D0/ 
      DATA DMJDBH/43994.0D0/ 
! IPOLU=0 MEANS POLE VALUES ARE IN UNITS OF MILLIARC SECONDS            
! IPOLU=1 MEANS POLE VALUES ARE IN UNITS OF 0.1 MILLIARC SECONDS        
! IPOLU=2 MEANS POLE X,Y VALUES ARE IN UNITS OF 0.001 MILLIARC SECONDS  
! IEOPU=2 MEANS POLE DEPS, DEPSILON VALUES ARE IN UNITS OF 0.001        
!    MILLIARC SECONDS                                                   
! CLEAR ARRAYS                                                          
      CALL CLEAR(XP,MAXDIM) 
      CALL CLEAR(YP,MAXDIM) 
      CALL CLEAR(A1UT1,MAXDIM*2) 
      CALL CLEAR(EOPDEP,MAXDIM) 
      CALL CLEAR(EOPDPS,MAXDIM) 
      DAYINT=INTVAL/24.0D0 
      IFPOLD=FPOLE/100 
      IFPOLH=(FPOLE - IFPOLD*100) * 10000 
      CALL DIFF(IFPOLD,IFPOLH,600101,0,IDAY,ISECS) 
      IHRPOL=ISECS/3600 
      DAYS=IDAY + IHRPOL/24.0D0 
! MJD FOR 600101 IS 36934                                               
      DMJDPL=(36934.0D0 - DAYS) 
      PI2=2.0D0*PI/365.24D0 
      PI4=2.0D0*PI2 
! READ OLD MASTER FILE                                                  
! ipolmu is flag for units of x,y of pole                               
! ieopmu is flag for units of deps, depsilon                            
! ipolmu = 0, input values in milliarcs                                 
! ipolmu = 1, input values in .1 milliarcs                              
! ipolmu = 2, input values in .001 milliarcs                            
! ieopmu values are already in .001 milliarcs, now a separate flag is us
      READ(OLDMST,10000,END=9000) XNMA6,IDUMMY,IPOLMU,IEOPMU 
      ISCALE=10 
      IF(IPOLMU.GT.0) ISCALE=1 
      IF(XNMA6.NE.HEADR1) GO TO 9100 
      IF(IPOLMU.LE.1) THEN 
         IPOLU = 1 
         WRITE(FRMT,('("(I8.8,2X,2I5,F15.10,4X,2I6,21X,I8)")'))
!sgp!    WRITE(FRMT,('("(I8,2X,2I5,D15.6,4X,2I6,21X,I8)")')) 
      ELSE 
         IPOLU = IPOLMU 
         WRITE(FRMT,('("(I8.8,2(1X,I7),F15.10,4X,2I7,15X,I8)")'))
!sgp!    WRITE(FRMT,('("(I8,2(1X,I7),D15.6,4X,2I7,15X,I8)")')) 
      ENDIF 
      NMASTS=NMCDS(ISECT) 
      DO 200 I=1,NMASTS 
      READ(OLDMST,FRMT,END=9000) IYMDH,XP1,YP1,A1UT1(I),EOPDEP(I),      &
     &                            EOPDPS(I)                             
      IF(IYMDH.LT.50010100) IYMDH=IYMDH+100000000 
! SCALE INPUT POLE VALUES BASED UPON FLAG                               
      XP(I)=XP1*ISCALE 
      YP(I)=YP1*ISCALE 
  200 END DO 
      IYMDHP=IYMDH 
! SET POINTER FOR START OF NEW UPDATES                                  
      ISTRT=NMASTS+1 
! TEST FOR POLE/A1UT1/EOPS UPDATE CARDS                                 
      IF(NUPCDS(ISECT).EQ.0) GO TO 2000 
! POSITION UPDATE FILE FOR READING POLE/A1UT1/EOPS UPDATES              
      IF((NSKIP(ISECT-1)+NUPCDS(ISECT-1)+1).EQ.NSKIP(ISECT)) GO TO 300 
      REWIND UPDATE 
      IF(NSKIP(ISECT).EQ.0) GO TO 300 
      NUPDS=NSKIP(ISECT) 
      DO 250 I=1,NUPDS 
      READ(UPDATE,10000,END=9200) XNMA6 
  250 END DO 
  300 CONTINUE 
! WRITE TITLE FOR UPDATE PRINTOUT                                       
      WRITE(OUTP,10400) 
      WRITE(OUTP,11200) 
! READ UPDATE HEADER RECORD                                             
      READ(UPDATE,10000,END=9200) XNMA6 
      IF(XNMA6.NE.HEADR1) GO TO 9300 
      NUPDS=NUPCDS(ISECT) 
      IHM=0 
      SEC=0.0D0 
      DO 1000 I=1,NUPDS 
      READ(UPDATE,10600,END=9200) IYMDH,XPOLE,YPOLE,UT1UTC,DEP,DPS 
      UT1UTC=UT1UTC*.0001D0 
      IF(IYMDH.LT.50010100) IYMDH=IYMDH+100000000 
! CONVERT FROM UT1-UTC TO A1-UT1                                        
      IYMD=IYMDH/100 
      IHOURS=IYMDH - IYMD*100 
      IHMS=IHOURS*10000 
      IHM=IHOURS*100 
      DAY=YMDAY(IYMD,IHM,SEC) 
! PASS A1-UTC ARRAYS TO TDIF                                            
      A1UTC=TDIF(4,3,DAY,DUMMY(INDD1),DUMMY(INDD2),DUMMY(INDD3),DUMMY,  &
     & DAYE,NUTC,NA1UT1,INTVAL,DAYB66)                                  
      TA1UT1=TDIF(4,1,DAY,DUMMY(INDD1),DUMMY(INDD2),DUMMY(INDD3),DUMMY, &
     & DAYE,NUTC,NA1UT1,INTVAL,DAYB66)                                  
      A1UT1X=A1UTC-UT1UTC 
! CHANGE INPUT POLE VALUES FROM UNITS OF MILLIARC SECONDS TO            
! .1 MILLIARC SECONDS FOR IPOLMU FLAG = 1 AND .001 MILLIARC             
! SECONDS FOR IPOLMU FLAG = 2                                           
      IF(IPOLMU.LE.1) THEN 
         X=(XPOLE+SIGN(0.05D0,XPOLE))*10.0D0 
         Y=(YPOLE+SIGN(0.05D0,YPOLE))*10.0D0 
      ELSE 
         X=(XPOLE+SIGN(0.05D0,XPOLE))*1000.0D0 
         Y=(YPOLE+SIGN(0.05D0,YPOLE))*1000.0D0 
      END IF 
! CHANGE INPUT DEP AND DPSI FROM UNITS OF MILLIARC SECONDS TO .001      
! MILLIARC SECONDS, FLAG NOT REALLY NEEDED HERE BUT MAY BE NEEDED       
! IN FUTURE                                                             
      IF(IEOPMU.LE.2) THEN 
         DP=(DEP+SIGN(0.0005D0,DEP))*1000.0D0 
         DS=(DPS+SIGN(0.0005D0,DPS))*1000.0D0 
      ENDIF 
      IF(IYMDH.GT.IYMDHP) GO TO 600 
! NEW UPDATE PRECEDES PREVIOUS UPDATES.TEST FOR OVERRIDE OPTION         
      IF(.NOT.OVRIDE) GO TO 9400 
! DETERMINE WHICH POLE/A1UT1 VALUE TO REPLACE                           
      CALL DIFF(IFPOLD,IFPOLH,IYMD,IHMS,IDAY,ISECS) 
      NHOURS=IDAY*24 + ISECS/3600 
      IND=NHOURS/INTVAL + 1 
      IF(IND.GE.ISTRT) GO TO 9500 
! REPLACE OLD VALUE                                                     
      IF(IYMDH.GT.100000000)THEN 
      IPYMDH=IYMDH-100000000 
      ELSE 
      IPYMDH=IYMDH 
      ENDIF 
      WRITE(OUTP,10900) IPYMDH,X,Y,UT1UTC,A1UT1X,TA1UT1,DP,DS 
      WRITE(OUTP,10901) IPYMDH,XP(IND),YP(IND),A1UT1(IND),EOPDEP(IND),  &
     & EOPDPS(IND)                                                      
      XP(IND)=X 
      YP(IND)=Y 
      A1UT1(IND)=A1UT1X 
      EOPDEP(IND)=DP 
      EOPDPS(IND)=DS 
      NREP=NREP+1 
! PUNCH MASTER FILE FORMAT NEW CARDS                                    
      ISEQ=IND 
      GO TO 1000 
! ADD NEW VALUES                                                        
  600 IYMDHP=IYMDH 
      NMASTS=NMASTS+1 
      XP(NMASTS)=X 
      YP(NMASTS)=Y 
      A1UT1(NMASTS)=A1UT1X 
      EOPDEP(NMASTS)=DP 
      EOPDPS(NMASTS)=DS 
      IF(IYMDH.GT.100000000)THEN 
      IPYMDH=IYMDH-100000000 
      ELSE 
      IPYMDH=IYMDH 
      ENDIF 
      WRITE(OUTP,10900) IPYMDH,X,Y,UT1UTC,A1UT1X,TA1UT1,DP,DS 
! SAVE DATE OF FINAL POLE VALUE OF UPDATE FILE                          
      IPDUPD=IYMDH 
 1000 END DO 

! WRITE NEW MASTER FILE AND PUNCH UPDATES TO OLD MASTER DECK            

 2000 CONTINUE 
      NCDS=NMASTS 
      WRITE(NEWMST,10000) HEADR1,NCDS,IPOLU,IEOPMU 
      NMAST=NMASTS 
! SET INDICATOR FOR START OF NEW UPDATES TO BE PUNCHED                  
      ISEQ=0 
      IYMDH=FPOLE 
      DO 2200 I=1,NMAST 
      ISEQ=ISEQ+1 
      IF(IYMDH.GT.100000000)THEN 
      IPYMDH=IYMDH-100000000 
      ELSE 
      IPYMDH=IYMDH 
      ENDIF 
      WRITE(NEWMST,FRMT) IPYMDH,XP(I),YP(I),A1UT1(I),EOPDEP(I),         &
     & EOPDPS(I),ISEQ                                                   
! SAVE DATE OF FINAL POLE VALUE OF NEW MASTER FILE AND NEW PUNCH FILE   
      IPDNMT=IYMDH 
      IYMD=IYMDH/100 
      IHOURS=IYMDH-IYMD*100 
      NHOURS=INTVAL+IHOURS 
      IDAYS=NHOURS/24 
      IHR=MOD(NHOURS,24) 
      CALL ADDYMD(IYMD,IDAYS) 
      IYMDH=IYMD*100 + IHR 
 2200 END DO 

! CONVERT POLE/A1UT1 TO REQUESTED BIH SYSTEM                            

      IF(LNOBIH) GOTO 3170 
      IF(.NOT.BIHNEW) GO TO 3120 
! DATA FROM START OF TABLE TO 790430 CHANGED TO NEW SYSTEM              
      IHEND=MIN(79050100,LPOLE) 
      IHENDD=IHEND/100 
      IHENDH=(IHEND - IHENDD*100) * 10000 
      CALL DIFF(IFPOLD,IFPOLH,IHENDD,IHENDH,IDAY,ISECS) 
      NHOURS=IDAY*24 + ISECS/3600 
      IREMAN=MOD(NHOURS,INTVAL) 

      IF(IREMAN.EQ.0) THEN 
! SINCE AN INTERVAL LANDS ON 790501 THEN TAKE ONE LESS                  
      ISTOP=NHOURS/INTVAL 
      ELSE 
      ISTOP=NHOURS/INTVAL + 1 
      ENDIF 

      ISTRT=1 
      SIGNN=1.0D0 
      DMJD=DMJDPL 
      GO TO 3140 
! DATA FROM MAY 1,1979 TO END OF TABLE CHANGED TO OLD BIH SYSTEM        
 3120 IF(LPOLE.LT.79050100) GO TO 3170 
      CALL DIFF(IFPOLD,IFPOLH,790501,0,IDAY,ISECS) 
      NHOURS=IDAY*24 + ISECS/3600 
      IREMAN=MOD(NHOURS,INTVAL) 

      IF(IREMAN.EQ.0) THEN 
      ISTRT=NHOURS/INTVAL + 1 
      ELSE 
      ISTRT=NHOURS/INTVAL + 2 
      ENDIF 

      ISTOP=NA1UT1 
      DMJD=DMJDBH 
      SIGNN=-1.0D0 
 3140 CONTINUE 
      DO 3150 I=ISTRT,ISTOP 
! CHANGE UNITS FOR CX FROM MILLIARC SECONDS TO .10 MILLIARC SECONDS     
! WFE 4/18/88                                                           
! CHECK FLAG TO CHANGE CX FROM MILLIARC SECONDS TO .001 MILLIARC SECONDS
! TAW 1/3/02                                                            
      IF(IPOLMU.LE.1) THEN 
         CX=(24.D0*SIN(PI2*(DMJD-43932.0D0))+                           &
     &     7.D0*SIN(PI4*(DMJD-43979.0D0)))*10.0D0                       
      ELSE 
         CX=(24.D0*SIN(PI2*(DMJD-43932.0D0))+                           &
     &     7.D0*SIN(PI4*(DMJD-43979.0D0)))*1000.0D0                     
      ENDIF 
      CX=CX+SIGN(.5D0,CX) 
      CT=7.0D-4*(SIN(PI2*(DMJD-44048.0D0))+SIN(PI4*(DMJD-44019.0D0))) 
! SIGN IS +1 FOR NEW BIH SYSTEM, -1 FOR OLD BIH SYSTEM                  
      ICX=SIGNN*CX 
      CT=SIGNN*CT 
      A1UT1(I)=A1UT1(I)-CT 
      XP(I)=XP(I)+ICX 
      DMJD=DMJD + DAYINT 
 3150 END DO 
 3170 CONTINUE 

! WRITE NEW GEODYN WORKING FILE  A1UT1 DATA                             

      NREC=(NMASTS-1)/NVRA11+1 
      NREC1=NREC -1 
      NREM=NMASTS-NREC1*NVRA11 
      WRITE(NEWGDN) HEADR2,NREC,NREM,NMASTS,(IDUM,I=1,43) 
! WRITE ALL FULL RECORDS FOR A1-UT1 DATA                                
      IF(NREC1.LE.0) GO TO 3250 
      DO 3200 J=1,NREC1 
      ISTART=(J-1)*NVRA11+1 
      ISTOP=ISTART+NVRA11-1 
      WRITE(NEWGDN) (A1UT1(K),K=ISTART,ISTOP) 
!     WRITE(6,11111) (A1UT1(K),K=ISTART,ISTOP)                          
!1111 FORMAT(1X,3D24.16)                                                
 3200 END DO 
!     STOP 56                                                           
! WRITE LAST A1-UT1 RECORD. ZERO FILL REMAINDER OF RECORD               
 3250 IF(NREM.LE.0) GO TO 3300 
      IF(NREM.LE.0) GO TO 3300 
      NDUM=NVRA11-NREM 
      ISTART=NREC1*NVRA11+1 
      IF(NDUM.LE.0) WRITE(NEWGDN) (A1UT1(K),K=ISTART,NMASTS) 
      IF(NDUM.GT.0) WRITE(NEWGDN) (A1UT1(K),K=ISTART,NMASTS),(RDUM,L=1, &
     & NDUM)                                                            
 3300 CONTINUE 

! WRITE NEW GEODYN WORKING FILE   POLE DATA                             

      NREC=(NMASTS*2-1)/NVRPOL+1 
      NREC1=NREC-1 
      NREM=NMASTS*2-NREC1*NVRPOL 
      WRITE(NEWGDN) HEADR1,NREC,NREM,NMASTS,IPOLU,(IDUM,I=1,42) 
      NVRPL2=NVRPOL/2 
      IF(NREC1.LE.0) GO TO 3500 
      DO 3400 J=1,NREC1 
      ISTART=(J-1)*NVRPL2+1 
      ISTOP=ISTART+NVRPL2-1 
      WRITE(NEWGDN) (XP(K),YP(K),K=ISTART,ISTOP) 
 3400 END DO 
! WRITE LAST POLE RECORD. ZERO FILL REMAINING VALUES IN RECORD          
 3500 IF(NREM.LE.0) GO TO 3600 
      NDUM=NVRPOL-NREM 
      ISTART=NREC1*NVRPL2+1 
      IF(NDUM.LE.0) WRITE(NEWGDN) (XP(K),YP(K),K=ISTART,NMASTS) 
      IF(NDUM.GT.0) WRITE(NEWGDN) (XP(K),YP(K),K=ISTART,NMASTS),(IDUM2, &
     & L=1,NDUM)                                                        
 3600 CONTINUE 

! WRITE NEW GEODYN WORKING FILE   EOP DATA                              

! NOTE: NVRPOL IS THE SAVE FOR THE EOP DATA                             
      NREC=(NMASTS*2-1)/NVRPOL+1 
      NREC1=NREC-1 
      NREM=NMASTS*2-NREC1*NVRPOL 
      WRITE(NEWGDN) HEADR3,NREC,NREM,NMASTS,IEOPMU,(IDUM,I=1,42) 
      NVRPL2=NVRPOL/2 
      IF(NREC1.LE.0) GO TO 3800 
      DO 3700 J=1,NREC1 
      ISTART=(J-1)*NVRPL2+1 
      ISTOP=ISTART+NVRPL2-1 
      WRITE(NEWGDN) (EOPDEP(K),EOPDPS(K),K=ISTART,ISTOP) 
 3700 END DO 
! WRITE LAST POLE RECORD. ZERO FILL REMAINING VALUES IN RECORD          
 3800 IF(NREM.LE.0) GO TO 3900 
      NDUM=NVRPOL-NREM 
      ISTART=NREC1*NVRPL2+1 
      IF(NDUM.LE.0) WRITE(NEWGDN) (EOPDEP(K),EOPDPS(K),K=ISTART,NMASTS) 
      IF(NDUM.GT.0) WRITE(NEWGDN) (EOPDEP(K),EOPDPS(K),K=ISTART,NMASTS) &
     &,(IDUM2,L=1,NDUM)                                                 
 3900 CONTINUE 
      RETURN 
! ENDFILE ON MASTER FILE ENCOUNTERED                                    
 9000 WRITE(OUTP,10100) XMF 
      STOP 16 
! INCORRECT HEADER ON MASTER FILE                                       
 9100 WRITE(OUTP,10200) XMF,HEADR1,XNMA6 
      STOP 16 
! END FILE ON UPDATE FILE ENCOUNTERED                                   
 9200 WRITE(OUTP,10100) UPDF 
      STOP 16 
! INCORRECT HEADER ON UPDATE FILE                                       
 9300 WRITE(OUTP,10200) UPDF,HEADR1,XNMA6 
      STOP 16 
! OVERRIDE OPTION NOT INVOKED                                           
      IF(IYMDH.GT.100000000)IYMDH=IYMDH-100000000 
      IF(IYMDHP.GT.100000000)IYMDHP=IYMDHP-100000000 
 9400 WRITE(OUTP,10700) IYMDH,IYMDHP 
      STOP 16 
! UPDATE CARD OUT OF ORDER                                              
      IF(IYMD.GT.1000000)IYMD=IYMD-1000000 
 9500 WRITE(OUTP,11300) IYMD 
      STOP 16 
10000 FORMAT(A6,4X,3I5) 
10100 FORMAT(5X,'UNEXPECTED END OF FILE ENCOUNTERED WHILE READING ',    &
     & 2A6,' IN SUBROUTINE RPOLE. EXECUTION TERMINATING')               
10200 FORMAT( 5X,' INCORRECT HEADER RECORD ENCOUNTERED IN SUBROUTINE RPO&
     &LE  WHILE TRYING TO READ ',2A6,2(1X,A6))                          
10300 FORMAT(15X,'UPDATES TO POLE AND A1UT1 TABLES'/14X,'POLE',10X,     &
     &'UT1UTC',8X,'A1UT1',10X,'A1UT1'/2X,'IYMD',7X,'X',5X,'Y',33X,      &
     &'PREDICTED')                                                      
10400 FORMAT(// 50(1H*),'UPDATES TO POLE/A1-UT1 TABLES',51(1H*) /32X) 
10500 FORMAT(I8,2X,2I5,D15.6,4X,2I6,21X,I8) 
10600 FORMAT(I8,2X,2F10.0,F10.6,2F10.0) 
10700 FORMAT( 5X,' DATE ON UPDATE CARD(',I8,') PRECEDES LAST DATE IN TAB&
     &LE(',I8,') AND OVERRIDE OPTION NOT INVOKED. EXECUTION TERMINATING'&
     &)                                                                 
10800 FORMAT(I6,4X,2I5,D15.6,37X,I8) 
10900 FORMAT(1X,'NEW:',2X,I8,2X,I7,2X,I5,2X,D15.6,2X,D15.6,2X,D15.6,4X, &
     & I7,2X,I7)                                                        
10901 FORMAT(1X,'OLD:',2X,I8,2X,I7,2X,I7,19X,D15.6,21X,I7,2X,I7) 
11100 FORMAT(A6,' DUMMY HEADER USED TO INDICATE START OF REPLACED POLE/A&
     &1UT1/EOP CARDS')                                                  
11200 FORMAT(7X,'YYMMDDHH',5X,'X', 6X,'Y', 8X,'UT1-UTC',11X,'A1-UT1',   &
     &  6X,'A1-UT1 PREDICTED',3X,'DEPSI',3X,'DPSI')                     
11300 FORMAT(1X,'POLE UPDATE CARD OUT OF TIME ORDER FOR ',I8,' EXECUTION&
     & TERMINATING IN SUBROUTINE RPOLE')                                
      END                                           
!$SCALFX                                                                
      SUBROUTINE SCALFX(LCALC,TIN,FLXLST,FLXINT,SSNINT,SCALE,TBLAST) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      LOGICAL LCALC 

!---------------------------------------------------------------------- 
!     ....SCALE ADJUSTS THE Y-VALUES TO MATCH THE TABLE FLUX            
!     ....TO THE FLUX FOR PREDICTION                                    

!     ....TIN    = MJD OF THE REQUESTED TIME                            
!     ....LCALC  = LOGICAL SWITCH --                                    
!     ....         IF TRUE, CALCULATE SCALE AND APPLY                   
!     ....         IF FALSE, ONLY APPLY SCALE                           
!     ....FLXLST = LAST REAL FLUX VALUE IN SOLAR FLUX TABLES            
!     ....FLXINT = OUTPUT INTERPOLATED FLUX VALUE                       
!     ....SCALE  = SCALE FACTOR FOR OUTPUT FLUX VALUES                  

!---------------------------------------------------------------------- 

!     ....DAYCYC IS THE LENGTH OF THE SOLAR CYCLE IN DAYS (11 YR.)      
!     ....DAYDIV IS THE TIME INTERVAL IN DAYS BETWEEN VALUES            
      DATA DAYCYC/4017.D0/, DAYDIV/30.4375D0/ 

      DAMAX = 24.D0 * DAYDIV 

                                                                        
      IF( .NOT. LCALC )  GO TO 100 

      SCALE = FLXLST / FLXINT 
  100 CONTINUE 

!---------------------------------------------------------------------- 

!     THE FOLLOWING SCALING IS SMOOTHED OVER 2 YEARS                    

       scalsc = (tin-tblast)/damax 
      if(scalsc.ge.1.0D0) then 
       scale2=1.0D0 
       goto 150 
      endif 
      scale2 = scale - ((scale-1.0D0)*scalsc) 

!---------------------------------------------------------------------- 

  150 CONTINUE 

      FLXINT = FLXINT * SCALE2 
      SSNINT = SSNINT * SCALE2 
      RETURN 
      END                                           
!$SUNSET                                                                
      SUBROUTINE SUNSET (IYRSTR,IMNSTR,FLUX,SSARR,MAXDM2,LSSPTR) 
!*****************************************************************      
!     THIS SUBROUTINE WILL CALCULATE THE SUN SPOT DATA FROM A SPECIFIED 
!     START DATE TO THE END DATE OF THE FLUX DATA.                      

!     IYRSTR   STARTING YEAR OF THE FLUX TABLE                          
!     IMNSTR   STARTING MONTH OF THE FLUX TABLE                         
!     FLUX     ARRAY USED TO STORE FLUX DATA                            
!     SSARR    ARRAY USED TO STORE SUN SPOT DATA                        
!     MAXDM2   MAXIMUM DIMENSION OF SSARR                               
!     LSSPTR   POINTER USED FOR SSARR                                   

      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      INTEGER FLUX,IDUM2,SSARR 
      INTEGER FA1UTC,FPOLE,FFLUXS,FFLXAP,FFLXKP8,FFLXKP 
      INTEGER FFXAPP,FFXKP8P,FFXKPP 
      INTEGER SFDATE,STARTD,STYM,SSDATE 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      DIMENSION FLUX(1),SSARR(1) 
      DIMENSION MONTHS(12),HEADR(3) ,FFLUX(4),FLUXT(7),NVRFLX(3) 
      COMMON/HEADER/FA1UTC,LA1UTC,FPOLE,LPOLE,FFLUXS,LFLUXS,            &
     &              FFLXAP,LFLXAP,FFLXKP8,LFLXKP8,FFLXKP,LFLXKP,        &
     &              NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP             
      COMMON/SOLARD/SFDATE,SSDATE,NSSSTD,NSSNM,NSUNV 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      DATA MONTHS/31,28,31,30,31,30,31,31,30,31,30,31/ 

!     CLEAR SUN SPOT ARRAY                                              

      NCLEAR = (MAXDM2+1)/2 
      CALL CLEAR (SSARR,NCLEAR) 

!     GET YEAR AND MONTH FOR START OF SUNSPOTS, NSSTD                   

      KYR = NSSSTD/100 
      KMN = NSSSTD-KYR*100 
!     WRITE(OUTP,3500) IYRSTR,IMNSTR,KYR,KMN                            
!3500 FORMAT(2X,'IYRSTR,IMNSTR,KYR,KMN ',4(2X,I6))                      

!     CALCULATE NUMBER OF MONTHS FROM BEGINNING OF FLUX TABLE TO        
!     SUN SPOT START DATE = NOMNTH AND NUMBER OF MONTHS FOR SUN         
!     SPOT ARRAY = NSSSNM                                               

      NOMNTH = (KYR-IYRSTR)*12-IMNSTR+KMN 
      NSSNM = 0 
      NSUNV = 0 

!     STORE NSSSTD INTO CHANGING VARIABLE FOR START DATE TO CHECK       
!     LOOP FOR EACH MONTH, DO UNTIL START DATE > LFLUXS                 

       NSTART = NSSSTD 
       NFLUXS = LFLUXS/100 
!      WRITE(OUTP,50000) NSTART,NFLUXS                                  
!50000 FORMAT(1H0,2X,'NSTART, NFLUXS ',2(I6,2X))                        
   50 IF(NSTART.GT.NFLUXS) GO TO 200 
           KPTR = NOMNTH*32 
           LSSPTR = NSSNM*32 
!      WRITE(OUTP,51000) KPTR,LSSPTR                                    
!51000 FORMAT(1H0,2X,'KPTR, LSSPTR ',I8,2X,I8)                          
           NSY = NSTART/100 
           NSM = NSTART-NSY*100 
           NDIM = MONTHS(NSM) 
           IF(NSM.EQ.2.AND.MOD(NSY,4).EQ.0) NDIM = 29 
           NSUNV = NSUNV+NDIM 

!     CALCULATE SUN SPOT NUMBER FOR EACH DAY                            

           DO 100 I = 1,NDIM 
                FLXVAL = FLUX(KPTR+I) 
                FLXVAL = FLXVAL/10.D0 
                SS1 = SQRT(0.728015D0**2-(4.*0.000890443D0*             &
     &              (63.7451D0-FLXVAL)))                                
                SS2 = (-0.728015D0+SS1)/(2.*0.000890443D0) 
                SS3 = (-0.728015D0-SS1)/(2.*0.000890443D0) 
                SSVAL = MAX(SS2,SS3) 
                NSSVAL = SSVAL+0.5D0 
                SSARR(LSSPTR+I) = NSSVAL 
!      WRITE(OUTP,52000) LSSPTR+I,SSARR(LSSPTR+I)                       
!52000 FORMAT(5X,'POINTER, SSARR(I) ',I8,2X,I8)                         
  100      CONTINUE 
           SSARR(LSSPTR+32) = FLUX(KPTR+32) 

!     CHECK TO SEE IF ARRAY HAS BEEN OVERFLOWED                         

           IF((LSSPTR+32).GT.MAXDM2) GO TO 2500 

!     CONTINUE TO NEXT MONTH                                            
           NOMNTH = NOMNTH+1 
           NSSNM = NSSNM+1 
           CALL NEXTDT(NDATE,NSTART) 
           NSTART = NDATE/100 
           GO TO 50 
  200 CONTINUE 
      RETURN 

!     WRITE OUT MESSAGE FOR ARRAY OVERFLOW                              

 2500 WRITE(OUTP,10000) LSSPTR,MAXDM2 
      STOP 6 
10000 FORMAT(1H0,5X,'SUN SPOT ARRAY OVERFLOW, POINTER IS POINTING AT',  &
     &       2X,I8,2X,'AND MAXIMUM DIMENSION IS',2X,I8)                 
      END                                           
!$SUNSPT                                                                
      SUBROUTINE SUNSPT(SSARR) 
!******************************************************                 
!     SUBROUTINE CALLED TO WRITE OUT THE SUN SPOT TABLE                 
!******************************************************                 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      INTEGER FLUX,IDUM2,SSARR 
      INTEGER FA1UTC,FPOLE,FFLUXS,FFLXAP,FFLXKP8,FFLXKP 
      INTEGER FFXAPP,FFXKP8P,FFXKPP 
      INTEGER SFDATE,STARTD,STYM,SSDATE 
      INTEGER OLDMST,UPDATE,OUTP,PNCH 
      CHARACTER*8 HDR 
      DIMENSION HDR(1) 
      DIMENSION FLUX(1),SSARR(1) 
      DIMENSION MONTHS(12),HEADR(3) ,FFLUX(4),FLUXT(7),NVRFLX(3) 
      COMMON/HEADER/FA1UTC,LA1UTC,FPOLE,LPOLE,FFLUXS,LFLUXS,            &
     &              FFLXAP,LFLXAP,FFLXKP8,LFLXKP8,FFLXKP,LFLXKP,        &
     &              NVRA1C,NVRA11,NVRPOL,NVRFS,NVRFAP,NVRKP             
      COMMON/SOLARD/SFDATE,SSDATE,NSSSTD,NSSNM,NSUNV 
      COMMON/UNITS /OLDMST,UPDATE,NEWMST,INTP,OUTP,NEWGDN,PNCH,ISCRTCH 
      DATA IZERO,IDUM,IDUM2/3*0/ 
      DATA HDR/'SUNSPT  '/ 

!     WRITE NEW GEODYN WORKING FILE - SUN SPOT DATA                     
!       WRITE HEADER RECORD                                             

      NREC = (NSSNM*32-1)/NVRFS+1 
      NREC1 = NREC-1 
      NREM = NSSNM*32-NREC1*NVRFS 
      WRITE(NEWGDN) HDR(1),NREC,NREM,NSUNV,(IDUM,I=1,43) 
!     WRITE(OUTP,5000) HDR(1),NREC,NREM,NSUNV                           
!5000 FORMAT(//1X,A8,2I4,I6)                                            

!     WRITE OUT DATA RECORDS                                            

      IF(NREC1.LE.0) GO TO 3900 
      DO 3800 J = 1,NREC1 
           ISTART = (J-1)*NVRFS+1 
           ISTOP = ISTART+NVRFS-1 
           WRITE(NEWGDN)(SSARR(K),K=ISTART,ISTOP) 
 3800 END DO 

!     WRITE LAST SUN SPOT RECORD. ZERO FILL REMAINDER OF RECORD         

 3900 IF(NREM.LE.0) GO TO 4000 
      NDUM = NVRFS-NREM 
      ISTART = NREC1*NVRFS+1 
      ISTOP = NSSNM*32 
      IF(NDUM.LE.0)WRITE(NEWGDN)(SSARR(K),K=ISTART,ISTOP) 
      IF(NDUM.GT.0)WRITE(NEWGDN)(SSARR(K),K=ISTART,ISTOP),              &
     &                                        (IDUM2,L=1,NDUM)          
 4000 CONTINUE 
      RETURN 
      END                                           
!$SYSTIM                                                                
      SUBROUTINE SYSTIM 
!********1*********2*********3*********4*********5*********6*********7**
! SYSTIM           MM/DD/YY            0000.0    PGMR - EDDY            


! FUNCTION                    CALLS SYSTEM ROUTINE FOR CURRENT DATE AND 
!                             TIME INFORMATION AND LOADS DATE AND TIME  
!                             INTO A SINGLE FLOATING POINT WORD         

! COMMENTS                                                              


!            OUTPUT IS YYMMDDHHMMSS. IN COMMON EXDATE                   

!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      CHARACTER*8 C_CYMD 
      CHARACTER*5 CZONE 
      CHARACTER*10 CTIME 
      CHARACTER*6 C_YMD 

      COMMON/EXDATE/IEXYMD,IEXHMS 


      DATA NUMOFF/48/ 
      DATA C1D6/1.0D6/ 

!********************************************************************** 
! START OF EXECUTABLE CODE ******************************************** 
!********************************************************************** 

      IEXYMD=0 
      IEXHMS=0 

! GET DATE AND TIME                                                     

       CALL DATE_AND_TIME(C_CYMD, CTIME, CZONE) 

       C_YMD = C_CYMD(3:8) 

       READ(C_YMD,"(I6)") IEXYMD 
       READ(CTIME,"(I6)") IEXHMS 

      XDATE =DBLE(IEXYMD) 
      XTIME =DBLE(IEXHMS) 
      G2SRTM=XDATE*C1D6+XTIME 
      RETURN 
      END                                           
!$TABINT                                                                
      SUBROUTINE TABINT( TIN, FLXINT, SSNINT ) 
!*************************************************************          

!     ....TIN = MJD OF THE REQUESTED TIME                               
!     ....FLXINT = OUTPUT INTERPOLATED FLUX VALUE                       
!     ....SSNINT = OUTPUT INTERPOLATED SUN SPOT VALUE                   

      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      REAL SUNNBR 
      DIMENSION SUNNBR(133) 

!---------------------------------------------------------------------- 

!     SUNNBR IS THE SMOOTHED OBSERVED AND PREDICTED SUNSPOT             
!     NUMBERS FOR CYCLE 21                                              
!     ....TAKEN FROM SOLAR-GEOPHYSICAL REPORT MARCH 1982 #451 PT.1      
!     ....PAGE 13                                                       

         DATA SUNNBR/                                                   &
     &  15., 13., 12., 13., 13., 12., 13., 14., 14., 13., 14., 15.,     &
     &  17., 18., 20., 22., 24., 26., 19., 33., 39., 46., 52., 57.,     &
     &  61., 65., 77., 70., 83., 89., 97.,104.,108.,111.,113.,118.,     &
     & 124.,131.,137.,141.,147.,153.,155.,155.,156.,158.,162.,165.,     &
     & 164.,163.,161.,159.,156.,155.,153.,150.,150.,150.,148.,142.,     &
     & 140.,141.,143.,143.,143.,141.,140.,140.,139.,138.,134.,130.,     &
     & 126.,122.,119.,116.,113.,110.,105.,100., 96., 90., 86., 82.,     &
     &  79., 76., 74., 72., 69., 67., 65., 64., 62., 62., 62., 61.,     &
     &  59., 57., 53., 50., 48., 47., 46., 44., 43., 41., 39., 37.,     &
     &  36., 36., 35., 35., 34., 32., 31., 30., 29., 28., 27., 27.,     &
     &  26., 25., 24., 23., 21., 19., 17., 16., 16., 15., 15., 14.,     &
     &  14./                                                            
! 1976                                                                  
! 1977                                                                  
! 1978                                                                  
! 1979                                                                  
! 1980                                                                  
! 1981                                         |>> PREDICTED            
! 1982                                                                  
! 1983                                                                  
! 1984                                                                  
! 1985                                                                  
! 1986                                                                  
! 1987                                                                  
!CC        , 14., 14., 15., 15., 16., 18./                              

!     ....TABBEG IS THE FIRST MJD IN THE TABLE (JAN 0, 1976)            

      DATA TABBEG/42778.D0/ 

!     ....DAYCYC IS THE LENGTH OF THE SOLAR CYCLE IN DAYS (11 YR.)      
!     ....DAYDIV IS THE TIME INTERVAL IN DAYS BETWEEN VALUES            
      DATA DAYCYC/4017.D0/, DAYDIV/30.4375D0/ 

!---------------------------------------------------------------------- 

!     ....GET TIME RELATIVE TO START OF TABLE                           

      TTAB = TIN - TABBEG + 1.D0 
      TTAB = MOD( TTAB , DAYCYC ) 
      IF( TTAB .LT. 0.0D0 ) TTAB = DAYCYC + TTAB 
!      WRITE(6,5000) TIN,TTAB                                           
!5000  FORMAT(' TIN,TTAB ',2D20.10)                                     

!---------------------------------------------------------------------- 

!     ....GET POINTER IN TABLE                                          

      IPNT = TTAB / DAYDIV + 1 
      DAINDV = MOD( TTAB , DAYDIV ) 

!---------------------------------------------------------------------- 

!     ....SUN IN THE TIME DERIVATIVE OF THE SUNSPOT NUMBER              

!     IF( IPNT .EQ. IPNTO ) GO TO 50                                    
!     IPNTO = IPNT                                                      

      SUN = ( SUNNBR(IPNT+1) - SUNNBR(IPNT) ) / DAYDIV 
!50   CONTINUE                                                          

!     ....SSNINT IS THE INTERPOLATED SUNSPOT NUMBER                     

      SSNINT = DAINDV * SUN + SUNNBR(IPNT) 

!---------------------------------------------------------------------- 

!     ....CONVERT FROM SUNSPOT NUMBER (SUNNBR)                          
!     ....TO SOLAR FLUX (PRFLX)                                         

      FLXINT = 63.7451D0 + 0.728015D0    * SSNINT                       &
     &                  + 0.000890443D0 * SSNINT**2                     
      FLXINT = FLXINT * 10.D0 
!      WRITE(6,5001) IPNT,DAINDV,SUN,SSNINT,FLXINT                      
!5001  FORMAT(' IPNT,DAINDV,SUN,SSNINT,FLXINT ',I4,4D20.10)             

      RETURN 
      END                                           
!$TABLRD                                                                
      SUBROUTINE TABLRD (FLXARR,SSNARR,NSTDT,NEDDT,NMAXD) 
!***************************************************************        

!     TABLRD SUBROUTINE READS IN THE VALUES FOR BOTH THE FLUX ARRAY     
!     AND THE SUN SPOT ARRAY FOR USE IN CALCULATION IN ORAN             

!     FLXARR - ARRAY USED TO STORE FLUX VALUES                          
!     SSNARR - ARRAY USED TO STORE SUN SPOT VALUES                      
!     NSTDT  - START DATE OF VALUES TO BE OBTAINED                      
!     NEDDT  - END DATE OF VALUES TO BE OBTAINED                        
!     NMAXD  - MAXIMUM DIMENSION OF FLUX AND SUN SPOT ARRAY             

      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      INTEGER IBUF2,FLXARR,SSNARR 
      CHARACTER*6 HEADR,HEADER 
      DIMENSION FLXARR(1),SSNARR(1),HEADR(4) 
      DIMENSION IBUF2(96),MONTHS(12),NVAR(1) 
      DATA HEADR /'FLUXS ','FLUXAP','FLUXKP','SUNSPT'/ 
      DATA MONTHS /31,28,31,30,31,30,31,31,30,31,30,31/ 
      DATA NVAR /96/ 

!     READ FLUX HEADER AND THEN SUN SPOT HEADER                         

      IUNT32 = 32 
      IUNT06 = 6 
      ITYPE = 1 
      READ(IUNT32,END=60100) HEADER 
  100 READ(IUNT32,END=60100) HEADER,NREC,NREM,NVAL 
      IF (HEADER.NE.HEADR(ITYPE)) THEN 
          DO 200 I = 1,NREC 
              READ(IUNT32,END=60100) IBUF2 
  200     CONTINUE 
      GO TO 100 
      ENDIF 
      NVREC=NVAR(1)/32 
      IND = 0 
      DO 2000 I = 1,NREC 
           READ(IUNT32,END=60100) IBUF2 
           IF(I.EQ.NREC)NVREC=NREM/32 
           K1=-31 
           K2=0 
           DO 1000 J = 1,NVREC 
                K1 = K1+32 
                IYYMM=IBUF2(J*32) 
                IF(IYYMM.LT.NSTDT) GO TO 1000 
                IF(IYYMM.GT.NEDDT) GO TO 1000 
                IYY = IYYMM/100 
                IMM = IYYMM-IYY*100 
                IDD = MONTHS(IMM) 
                IF(IMM.EQ.2.AND.MOD(IYY,4).EQ.0) IDD = 29 
                K2 = K1+IDD-1 
                KB = K1 
                KE = K2 
                IF (ITYPE.EQ.1) THEN 
                     DO 250 K = KB,KE 
                           IND = IND+1 
                           FLXARR(IND) = IBUF2(K) 
  250                 CONTINUE 
                ELSE 
                      DO 275 K = KB,KE 
                           IND = IND+1 
                           SSNARR(IND) = IBUF2(K) 
  275                 CONTINUE 
                ENDIF 
 1000      CONTINUE 
 2000 END DO 
!     TEST IF NUMBER OF FLUX VALUES EXCEEDS ALLOCATED SPACE             
      IF(IND.GT.NMAXD) GO TO 60200 
      IF(IND.EQ.NMAXD) GO TO 3000 
!     IF REQUESTED DATE EXCEEDS TABLE END DATE ZERO OUT REST OF ARRAY   
      IF (ITYPE.EQ.1.AND.NEDDT.GT.IYYMM) WRITE(IUNT06,80000)NEDDT,IYYMM 
      IND1 = IND+1 
      DO 2500 J = IND1,NMAXD 
           FLXARR(J) = 0 
 2500 END DO 
 3000 CONTINUE 
      IF(ITYPE.NE.1) GO TO 4000 
      ITYPE = 4 
      GO TO 100 
 4000 RETURN 
!     END OF FILE ON UNIT 32                                            
60100 WRITE(IUNT06,80100) IUNT32 
      STOP 16 
!     FLUX VALUES EXCEEDS MAXIMUM DIMENSION                             
60200 WRITE(IUNT06,80200) IND,NMAXD 
      STOP 16 
80000 FORMAT(5X,'REQUESTED DATE OF ',I4,' EXCEEDS TABLE END',           &
     &' DATE OF ',I4)                                                   
80100 FORMAT(5X,'UNEXPECTED END OF FILE ON UNIT',I3,                    &
     &      'EXECUTION TERMINATING')                                    
80200 FORMAT(5X,'NUMBER OF VALUES(',I5,') EXCEEDS ALLOCATED SPACE (',I5,&
     & ') IN TABLRD')                                                   
      END                                           
!$TDIF                                                                  
      FUNCTION TDIF(BASE,IN,DAY0,UTCDIF,UTCRT,UTCT,A1T1,                &
     & DAYE,NUTC,NA1UT1,INTVAL,DAYB66)                                  
!********************************************************************   
! NAME              TDIF                                                

! PURPOSE           TO COMPUTE THE DIFFERENCE IN SECONDS BETWEEN ANY    
!                   TWO OF THE FOLLOWING TIME SYSTEMS                   
!                    A.1, UTC, UT1, UT2                                 

! CALLING SEQUENCE  X=TDIF(BASE,IN,DAY0)                                

!    SYMBOL  TYPE   DESCRIPTION                                         

!    BASE    I      INPUT - DESIRED TIME SYSTEM :                       
!                            (1=UT1, 2=UT2, 3=UTC, 4=A.1)               

!    IN      I      INPUT - CURRENT TIME SYSTEM :                       
!                            (1=UT1, 2=UT2, 3=UTC, 4=A.1)               

!    DAY0    DP     INPUT - CURRENT TIME IN DAYS FROM JAN 0.0 OF THE    
!                           REFERENCE YEAR                              

!    TDIF    R      OUTPUT - COMPUTED TIME DIFFERENCE IN SECONDS        

! COMMON BLOCKS     INITBK                                              

! REFERENCES        'GEODYN SYSTEMS DESCRIPTION'                        
!                   VOLUME 1 - GEODYN DOCUMENTATION                     




      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      INTEGER BASE,ID(4) 
      DOUBLE PRECISION MOON,K2 
      DIMENSION UTCT(1),UTCDIF(1),UTCRT(1),A1T1(1) 
      LOGICAL NOT1ST 
      DATA NOT1ST/.FALSE./ 
      DATA ID/2,1,4,3/ 
      DATA TWOPI/6.2831853072D0/ 
! DAYE = NUMBER OF DAYS FROM 1 JAN 1966 TO LAST DATA POINT IN A1T1 TABLE
!     DATA K2/0.29D0/                                                   
      UT2UT1(TWOPID,FORPID)=.022D0*SIN(TWOPID)-.012D0*COS(TWOPID)       &
     &                     -.006D0*SIN(FORPID)+.007D0*COS(FORPID)       
      IF(NOT1ST) GO TO 5 
      NOT1ST=.TRUE. 
      IDAYE=DAYE+0.5D0 
      IY1=660101 
      IYMD=IY1 
      CALL ADDYMD(IYMD,IDAYE) 
      IDAYE=UTCT(NUTC)+0.5D0 
      CALL ADDYMD(IY1,IDAYE) 
      LIMIT=NA1UT1-1 
      REFTIM=YMDAY(660101,0,0.D0) 
      V2=A1T1(NA1UT1) 
! CALCULATE POINTER TO A1UT1 VALUE 365 DAYS BACK FROM THE END OF THE    
! NA1UT1-(8760/INTVAL)                                                  
      ISUB=8760/INTVAL 
      V1=A1T1(NA1UT1-ISUB) 
      TWPIOY=TWOPI/365.2422D0 
! NOTE: .201 IS THE DIFFERENCE BETWEEN THE MJD OF THE YEAR 1966         
!       AND THE MJD OF THE BESSELIAN YEAR 1966                          
      TWOPID=TWPIOY*(DAYE-.201D0) 
      FORPID=2.0D0*TWOPID 
      U2=UT2UT1(TWOPID,FORPID) 
      TWOPID=TWPIOY*(DAYE-365.D0-.201D0) 
      FORPID=2.0D0*TWOPID 
      U1=UT2UT1(TWOPID,FORPID) 
      W2=U2-V2 
      W1=U1-V1 
      SLOPE=(W2-W1)/365.D0 
      UT2A1=W2-SLOPE*DAYE 
    5 TDIF=0.0D0 
      IF(BASE.EQ.IN) RETURN 
      IF(BASE.GT.4.OR.IN.GT.4) RETURN 
      IF(BASE.LE.0.OR.IN.LE.0) RETURN 
      DAYS=DAY0-REFTIM 
! TIME CORRECTIONS IN ORDER                                             
!  UT2-UT1 TO UT1-A.1 TO A.1-UTC                                        
      I1=ID(IN) 
      I2=ID(BASE) 
      J1=MIN(I1,I2) 
      J2=MAX(I1,I2) 
      IF(IN.NE.4) GO TO 200 
! IF ARGUMENT IN A.1 CONVERT TO UTC                                     
      DO 100 I=1,NUTC 
      IF(DAYS.LT.UTCT(I+1)) GO TO 150 
  100 END DO 
      I=NUTC 
  150 A1UT=UTCDIF(I)+UTCRT(I)*(DAYS-UTCT(I)) 
      DAYS=DAYS-A1UT/8.64D4 
! START CORRECTION CALCULATION WITH SMALLER TIME BASE                   
  200 GO TO (10,60,30,50),J1 
   60 IF(DAYS.LE.DAYE) GO TO 20 
      GO TO 15 
   10 IF(DAYS.GT.DAYE.AND.J2.GT.2) GO TO 18 
! COMPUTE UT2-UT1                                                       
   15 TWOPID=TWPIOY*(DAYS-.201D0) 
      FORPID=2.0D0*TWOPID 
      TDIF=TDIF+UT2UT1(TWOPID,FORPID) 
! TEST FOR OUTPUT TIME SYSTEM                                           
   18 IF(J2.EQ.2) GO TO 50 

! COMPUTE UT1-A1 USING A1-UT1 TABLES                                    

! JULIAN DATE JAN 1,1966 = 2439126.5D0                                  
! 20  DJUL=DAYS+2439126.5D0                                             
! CALCULATE CORRECTION TO UT1 TIME DUE TO SOLID EARTH TIDE              
!     CALL LIBARG(DJUL)                                                 
!     T2MOON=MOON+MOON                                                  
!     UT1TID=(-K2*(2.47D0*SIN(T2MOON)+1.02D0*SIN(T2MOON-COMG)+          
!    .   2.63D0*SIN(GP)+0.58D0*SIN(T2MOON-2.0D0*CL-GP)))/1.0D3          
!     TDIF=TDIF+UT1TID                                                  
   20 CONTINUE 
      IF(DAYS.GT.DAYE) GO TO 22 
! NUMBER OF A1UT1 DATA POINTS FROM BEG. OF TABLES TO DATA POINT         
! INCLUDE FRACTION                                                      
      DAYINT=INTVAL/24.0D0 
      DT=(DAYS+DAYB66+DAYINT)/DAYINT 
      DT1=DT 
      I=MIN(LIMIT,INT(MAX(1.,REAL(DT1)))) 
      DT=DT-DBLE(I) 
      TDIF=TDIF-A1T1(I)-DT*(A1T1(I+1)-A1T1(I)) 
      GO TO 28 
   22 TDIF=-TDIF+UT2A1+SLOPE*DAYS 
   28 IF(J2.EQ.3) GO TO 50 
! COMPUTE A.1-UTC                                                       
   30 DO 40 I=1,NUTC 
      IF(DAYS.LT.UTCT(I+1)) GO TO 45 
   40 END DO 
      I=NUTC 
   45 TDIF=TDIF+UTCDIF(I)+UTCRT(I)*(DAYS-UTCT(I)) 
! SET DIRECTION OF CORRECTION                                           
   50 IF(I1.EQ.J1)TDIF=-TDIF 
      RETURN 
      END                                           
!$YMDAY                                                                 
      DOUBLE PRECISION FUNCTION YMDAY(IYMD,IHM,SEC) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      COMMON/CTIME/DAYREF(11),IYREF 
      IY=IYREF*10000+101 
      IHMS=IHM*100 
      CALL DIFF(IY,0,IYMD,IHMS,ID,IS) 
      YMDAY=86400*(ID+1)+IS 
      YMDAY=(YMDAY+SEC)/8.64D4 
      RETURN 
      END                                           
!$ZEROI                                                                 
      SUBROUTINE ZEROI(IA,N) 
!********1*********2*********3*********4*********5*********6*********7**

! NAME ZEROI                                     PGMR BILL EDDY         

! FUNCTION                    USED TO ZERO OUT INTEGER ARRAYS           

! INPUT/OUTPUT PARAMETERS:                                              
!                   IA     - INTEGER ARRAY TO BE SET TO ZERO            
!                   N      - NUMBER OF ELEMENTS IN IA TO ZERO OUT       

!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      SAVE 
      DIMENSION IA(1) 
      DATA IUNT06/6/ 

!********************************************************************** 
! START OF EXECUTABLE CODE ******************************************** 
!********************************************************************** 

      IENTRY=1 
      IF(N.LE.0) GO TO 100 

! USE INTEGER 0 TO CLEAR ARRAY                                          

      DO 10 I=1,N 
   10 IA(I)=0 
      RETURN 
  100 WRITE(IUNT06,80000) IENTRY,N 
      RETURN 
80000 FORMAT(10X,'WARNING - NEGATIVE OR ZERO ARGUMENT PASSED TO ENTRY', &
     &'NO.',I2,'IN ROUTINE ZEROI.',I10)                                 
      END                                           
!$ZTIME                                                                 
      SUBROUTINE ZTIME(NOW,J) 
!*******************************************************                
      CHARACTER*1 NOW 
      DIMENSION NOW(24) 
      DO 1000 I=1,24 
      NOW(I)=' ' 
 1000 END DO 
      RETURN 
      END                                           
