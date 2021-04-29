!ANTHERDEF
      SUBROUTINE ANTHERDEF(AZ,ELEV,DECL,ATM_TEMP,MJD,TAI,STANAM,DELAY)
!      PROGRAM ANTHERDEF
!********1*********2*********3*********4*********5*********6*********7**
! ANTHERMDEF-2E         03/??/12            1201.01   PGMR - SMCS
!
! FUNCTION:  COMPUTES THE PORTION OF TROP_DEL THAT IS DUE TO
!            THERMAL DEFORMATION OF THE ANTENNA
!
!
! CALLING SEQUENCE
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    AZ      I    S    RADIO SOURCE AZIMUTH
!   ELEV     I    S    RADIO SOURCE ELEVATION
!   DECL     I    S    RADIO SOURCE DECLINATION
! ATM_TEMP   I    S    ATMOSPHERIC TEMPERATURE
!   MJD      I    S    MODIFIED JULIAN DATE
!   TAI      I    S    TAI
!  STANAM    I    S    STATION NAME
!  DELAY     O    S    DELAY DUE TO THERMAL EXPANSION
!
!
! COMMENTS:
!********1*********2*********3*********4*********5*********6*********7**
! NOTE: FROM DAN MacMillan on March 9 2012 via email.
!   "[VLBI] is not currently using the time lag since [they] don't have
!      continuous measurements of temperatuer at the sites. Temperature
!      is recorded only during the 24 hour sessions... [They] are
!      looking in to using ECMWF data to fill in gaps between 24 hour
!      sessions..."
!
!********1*********2*********3*********4*********5*********6*********7**
!
! SUBROUTINES USED  FNDNUM
!
! INPUT FILES       NONE
!
! OUTPUT FILES      NONE
!
! REFERENCES: Conventions on Thermal Expansion Modelling of Radio
!             Telescopes for Geodetic and Astrometric VLBI
!                     By Axel Nothnagel (15 November 2008)
!
!
!             Incorporated into this subroutine is the
!             "Antenna Information File Format Version 2008.04.22".
!             This information is coded as DATA statements. Below
!             is the description of each:
!
! STA_NAM:  IVS Station Name
! FTYPE : Focus Type: FO_PRIM = Primary, FO_SECN = Secondary
! MOTYP : Mounting Type : MO_AZEL (azimuthal), MO_EQUA (equatorial),
!                         MO_XYNO (XY North), MO_XYEA (XY east),
!                         MO_RICH (RICHMOND, displaced equatorial)
! RADOME: FLAG : RA_NO , RA_YES
! METYPE: Measurement type: ME_COMP (complete), ME_INCM (incomplete)
!                           ME_ROUGH (rough)
! REF_TMP: Reference Temperature (degrees Celcius)
! SIN_AMP: Sin amplitude if annual temperature variations wrt J2000
!          epoch (degrees Celcius)
! COS_AMP: Cos amplitude if annual temperature variations wrt J2000
!          epoch (degrees Celcius)
! REF_PRE: Reference Pressure (hPa)
! ANT_DIA: Antenna diameter (m)
! HEI_FOU: Height of Foundation (m)
! DEP_FOU: Depth of Foundation (m)
! XPANCOEFF: Foundation Thermal Expansion Coeff (1/K)
! XPANCOEFS: Antenna Elements Thermal Expansion Coeff (1/K)
!    NOTE: THE EXPANCOEF values are assumed from the paper.
!
! HEI_PIL: Length of Fixed axis (height of pillar) (m)
! AXI_OFF: Length of Axis Offset (m)
! HEI_VER: Distance from the movable axis to the antenna vertex (m)
! HEI_SUB: Height of the Subreflector above the vertex (m)
! ANTLAG : Antenna elements lag (2 hours) (From paper)
! FOUNDLAG: Foundation lag (6 hours) (from paper)
!
!********1*********2*********3*********4*********5*********6*********7**
! OTHER VARIABLES USED IN SUBROUTINE:
!
! HF = height of concrete foundation
! HP = height of the antenna pillar
! HV = height of the vertex
! HS = height of the subreflector
! AO = Axis Offset
! EXCOEFF = Expansion coefficients for foundation
! EXCOEFA = Expansion coefficients for antenna elements
! DECL = Radio Source Declination
! AZ = Radio source azimuth
! ELEV = Radio source elevation
! FA = antenna focus factor
! ATM_TEMP = Surrounding air temperature
! REF_TEMP = Reference air temperature
! OBS_EPOCH = Observation epoch
!      NOTE: The observation epoch is defined by the Modified Julian Date
!            (MJD) times the number of seconds in the day plus the
!            fraction of seconds in the day (TAI) + 32.184.
!
! ANTLAG = Time lag of antenna elements
! FOUNDLAG = Time lag of foundation
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT NONE

      INCLUDE 'COMMON_DECL.inc'
      COMMON/CLIGHT/VLIGHT,ERRLIM,XCLITE
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY


      DOUBLE PRECISION INV_LIGHT,HFSINELEV,HPSINELEV
      DOUBLE PRECISION FOUND_XPAN,ANT_XPAN,FAHS
      DOUBLE PRECISION  DEL_TAU2,DEL_TAUT,TRIG1,TRIG2
      DOUBLE PRECISION COSSIN,SQRTTRIG,DEL_TAU1
      DOUBLE PRECISION TRIG3
      DOUBLE PRECISION  EXCOEFF,EXCOEFA,DECL,AZ
      DOUBLE PRECISION  ELEV,ATM_TEMP
      DOUBLE PRECISION OBS_EPOCH,ANTLAG,FOUNDLAG
      DOUBLE PRECISION TAI,To
      DOUBLE PRECISION HF,HP,HV,HS,AO,FA,REF_TEMP
      DOUBLE PRECISION REF_TMP(157),REF_PRE(157)
      DOUBLE PRECISION ANT_DIA(157),HEI_FOU(157)
      DOUBLE PRECISION COS_AMP(157),SIN_AMP(157)
      DOUBLE PRECISION  DEP_FOU(157),XPANCOEFF(157)
      DOUBLE PRECISION XPANCOEFS(157)
      DOUBLE PRECISION HEI_PIL(157),AXI_OFF(157)
      DOUBLE PRECISION HEI_VER(157),HEI_SUB(157)
      DOUBLE PRECISION  DLAMDA, PHIo,DELAY

      CHARACTER*8 STA_NAM(157),STANAM
      CHARACTER*7 FTYPE(157),MOTYP(157),RADOME(157),METYPE(157)

      INTEGER MJD,IRET,ARRAY_INDEX

!      DATA <NAME>/value,value/

      DATA STA_NAM/"AIRA    ","ALGOPARK","AUSTINTX","AZORES  ",         &
     &             "BADARY  ","BERMUDA ","BLKBUTTE","BLOOMIND",         &
     &             "BR-VLBA ","BREST   ","CARNUSTY","CARROLGA",         &
     &             "CHICHI10","CHLBOLTN","CRIMEA  ","CTVASBAY",         &
     &             "CTVASTJ ","DEADMANL","DSS15   ","DSS45   ",         &
     &             "DSS63   ","DSS65   ","DSS65A  ","EFLSBERG",         &
     &             "ELY     ","FD-VLBA ","FLAGSTAF","FORTLEZA",         &
     &             "FORT_ORD","FORTORDS","FTD_7900","GBT-VLBA",         &
     &             "GGAO7108","GIFU11  ","GIFU3   ","GILCREEK",         &
     &             "GOLDVENU","GORF7102","GRASSE  ","HALEAKAL",         &
     &             "HARTRAO ","HATCREEK","HAYSTACK","HN-VLBA ",         &
     &             "HOBART26","HOFN    ","HOHENFRG","HOHNBERG",         &
     &             "HRAS_085","JPL_MV1 ","KAINAN  ","KANOZAN ",         &
     &             "KARLBURG","KASHIM11","KASHIM34","KASHIMA ",         &
     &             "KAUAI   ","KIRSBERG","KODIAK  ","KOGANEI ",         &
     &             "KOGANEI3","KOKEE   ","KP-VLBA ","KWAJAL26",         &
     &             "LA_VLBA ","LEONRDOK","MAMMOTHL","MARCUS  ",         &
     &             "MARPOINT","MATERA  ","MCD_7850","MEDICINA",         &
     &             "METSAHOV","METSHOVI","MIAMI20 ","MILESMON",         &
     &             "MIURA   ","MIYAZAKI","MIZNAO10","MIZUSGSI",         &
     &             "MK-VLBA ","MOJ_7288","MOJAVE12","MON_PEAK",         &
     &             "MV2ONSLA","NL_VLBA ","NOBEY_6M","NOME    ",         &
     &             "NOTO    ","NRAO_140","NRAO20  ","NRAO85_1",         &
     &             "NRAO85_3","NYALES20","OCOTILLO","OHIGGINS",         &
     &             "ONSALA60","ONSALA85","OV-VLBA ","OVR_7853",         &
     &             "OVRO_130","PARKES  ","PBLOSSOM","PENTICTN",         &
     &             "PIETOWN ","PINFLATS","PLATTVIL","PRESIDIO",         &
     &             "PT_REYES","PVERDES ","QUINCY  ","RICHMOND",         &
     &             "ROBLED32","SAGARA  ","SANPAULA","SANTIA12",         &
     &             "SC-VLBA ","SEATTLE1","SESHAN25","SEST    ",         &
     &             "SHANGHAI","SINTOTU ","SINTOTU3","SNDPOINT",         &
     &             "SOURDOGH","SUWON   ","SVETLOE ","SYOWA   ",         &
     &             "TATEYAMA","TIDBIN64","TIGOCONC","TIGOWTZL",         &
     &             "TITIJIMA","TOMAKO11","TOULOUSE","TROMSONO",         &
     &             "TRYSILNO","TSUKU3  ","TSUKUBA ","TSUKUB32",         &
     &             "URUMQI  ","USUDA64 ","VERAMZSW","VERNAL  ",         &
     &             "VICTORIA","VLA-N8  ","VNDNBERG","WESTFORD",         &
     &             "WETTZELL","WHTHORSE","YAKATAGA","YEBES   ",         &
     &             "YEBES40M","YELLOWKN","YLOW7296","YUMA    ",         &
     &             "ZELENCHK"/


      DATA FTYPE/"FO_SECN","FO_PRIM","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_PRIM","FO_PRIM",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_PRIM","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_PRIM","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_PRIM","FO_SECN","FO_SECN","FO_PRIM",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_PRIM","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_PRIM","FO_SECN",     &
     &           "FO_SECN","FO_PRIM","FO_SECN","FO_SECN","FO_PRIM",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_PRIM","FO_PRIM",     &
     &           "FO_PRIM","FO_PRIM","FO_PRIM","FO_PRIM","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_PRIM","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_PRIM","FO_PRIM","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN","FO_SECN","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_PRIM","FO_PRIM","FO_SECN","FO_SECN",     &
     &           "FO_SECN","FO_SECN"/

      DATA MOTYP/"MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_XYNO","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_EQUA","MO_EQUA","MO_AZEL","MO_AZEL","MO_XYEA",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_XYNO","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_EQUA","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_XYNO","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_EQUA",     &
     &           "MO_AZEL","MO_EQUA","MO_EQUA","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_RICH","MO_EQUA","MO_AZEL","MO_AZEL",     &
     &           "MO_XYNO","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL","MO_AZEL",     &
     &           "MO_AZEL","MO_AZEL"/

      DATA RADOME/42*"RA_NO  ","RA_YES ",29*"RA_NO  ","RA_YES ",        &
     &            23*"RA_NO  ","RA_YES ",50*"RA_NO  ","RA_YES ",        &
     &             9*"RA_NO  "/


      DATA METYPE/"ME_INCM","ME_COMP","ME_INCM","ME_COMP","ME_COMP",    &
     &            "ME_INCM","ME_INCM","ME_INCM","ME_INCM","ME_COMP",    &
     &            "ME_COMP","ME_INCM","ME_INCM","ME_ROUG","ME_INCM",    &
     &            "ME_INCM","ME_INCM","ME_INCM","ME_COMP","ME_COMP",    &
     &            "ME_INCM","ME_COMP","ME_COMP","ME_COMP","ME_INCM",    &
     &            "ME_COMP","ME_INCM","ME_COMP","ME_INCM","ME_INCM",    &
     &            "ME_INCM","ME_INCM","ME_COMP","ME_COMP","ME_COMP",    &
     &            "ME_COMP","ME_ROUG","ME_INCM","ME_COMP","ME_INCM",    &
     &            "ME_COMP","ME_COMP","ME_COMP","ME_COMP","ME_COMP",    &
     &            "ME_COMP","ME_COMP","ME_COMP","ME_COMP","ME_INCM",    &
     &            "ME_INCM","ME_INCM","ME_COMP","ME_COMP","ME_COMP",    &
     &            "ME_COMP","ME_COMP","ME_COMP","ME_INCM","ME_COMP",    &
     &            "ME_COMP","ME_COMP","ME_COMP","ME_ROUG","ME_COMP",    &
     &            "ME_INCM","ME_INCM","ME_COMP","ME_ROUG","ME_COMP",    &
     &            "ME_INCM","ME_COMP","ME_ROUG","ME_COMP","ME_ROUG",    &
     &            "ME_INCM","ME_COMP","ME_INCM","ME_ROUG","ME_INCM",    &
     &            "ME_COMP","ME_INCM","ME_ROUG","ME_INCM","ME_INCM",    &
     &            "ME_COMP","ME_INCM","ME_INCM","ME_COMP","ME_COMP",    &
     &            "ME_COMP","ME_COMP","ME_COMP","ME_COMP","ME_INCM",    &
     &            "ME_COMP","ME_COMP","ME_ROUG","ME_COMP","ME_ROUG",    &
     &            "ME_ROUG","ME_ROUG","ME_INCM","ME_INCM","ME_COMP",    &
     &            "ME_INCM","ME_INCM","ME_INCM","ME_INCM","ME_INCM",    &
     &            "ME_INCM","ME_COMP","ME_ROUG","ME_INCM","ME_INCM",    &
     &            "ME_COMP","ME_COMP","ME_INCM","ME_COMP","ME_ROUG",    &
     &            "ME_ROUG","ME_INCM","ME_INCM","ME_INCM","ME_INCM",    &
     &            "ME_INCM","ME_COMP","ME_INCM","ME_COMP","ME_ROUG",    &
     &            "ME_COMP","ME_COMP","ME_INCM","ME_COMP","ME_COMP",    &
     &            "ME_COMP","ME_COMP","ME_INCM","ME_INCM","ME_COMP",    &
     &            "ME_COMP","ME_ROUG","ME_INCM","ME_INCM","ME_INCM",    &
     &            "ME_INCM","ME_INCM","ME_COMP","ME_COMP","ME_INCM",    &
     &            "ME_INCM","ME_INCM","ME_INCM","ME_INCM","ME_INCM",    &
     &            "ME_INCM","ME_COMP"/

      DATA REF_TMP/16.4D0, 4.7D0,25.3D0,16.1D0, 2.4D0,20.1D0,16.8D0,    &
     &             14.6D0,12.4D0,11.8D0, 9.6D0,19.0D0,22.9D0,10.5D0,    &
     &             13.8D0, 6.2D0, 8.0D0,14.1D0,12.6D0,14.1D0,12.5D0,    &
     &             12.5D0,12.5D0, 9.0D0, 6.4D0,15.5D0, 7.5D0,26.7D0,    &
     &             16.2D0,14.8D0,15.6D0, 9.3D0,13.4D0,15.0D0,15.0D0,    &
     &             -3.3D0,12.1D0,13.3D0, 7.2D0, 4.9D0,16.1D0, 9.3D0,    &
     &              8.6D0, 7.1D0,13.2D0, 6.9D0, 9.7D0, 6.4D0,15.5D0,    &
     &             15.9D0,16.2D0,13.3D0, 9.7D0,14.6D0,14.5D0,14.5D0,    &
     &             17.0D0, 9.4D0, 4.8D0,14.4D0,14.5D0,16.9D0,10.2D0,    &
     &             27.6D0,10.1D0,21.0D0, 2.4D0,25.3D0,14.3D0,14.0D0,    &
     &             12.8D0,14.6D0, 6.8D0, 6.9D0,24.6D0,10.5D0,15.1D0,    &
     &             17.6D0,11.2D0,10.8D0, 0.6D0,13.2D0,13.1D0, 7.9D0,    &
     &              9.1D0,13.9D0, 5.9D0,-4.8D0,19.9D0, 9.4D0, 9.4D0,    &
     &              9.4D0, 9.6D0,-4.1D0,20.4D0,-2.6D0, 9.0D0, 9.0D0,    &
     &             10.1D0,10.2D0,10.0D0,17.3D0,12.9D0,10.0D0, 7.7D0,    &
     &             11.6D0,10.3D0,16.1D0,15.7D0,18.1D0, 9.0D0,24.6D0,    &
     &             12.4D0,15.2D0,16.9D0,11.8D0,25.5D0,14.0D0,18.9D0,    &
     &              3.5D0,18.9D0, 7.2D0, 7.2D0, 4.7D0,-3.3D0,14.2D0,    &
     &              5.8D0,-8.6D0,15.2D0,14.0D0,12.6D0, 7.9D0,22.9D0,    &
     &              8.1D0,14.7D0, 3.2D0, 3.6D0,14.4D0,14.4D0,14.3D0,    &
     &              4.5D0, 4.8D0,11.1D0, 9.2D0,13.2D0, 9.6D0,17.4D0,    &
     &              8.9D0, 7.8D0,-0.3D0, 4.3D0,11.6D0,11.6D0,-0.5D0,    &
     &             -0.5D0,19.4D0, 7.3D0/


      DATA SIN_AMP/157*0.0D0/
      DATA COS_AMP/157*0.0D0/

      DATA REF_PRE/ 978.2D0, 984.2D0, 993.3D0,1019.6D0, 917.2D0,        &
     &             1016.7D0, 957.5D0, 987.9D0, 983.2D0,1010.8D0,        &
     &             1010.8D0, 979.0D0,1005.4D0,1004.2D0,1012.5D0,        &
     &             1005.0D0, 997.3D0, 918.9D0, 903.8D0, 941.4D0,        &
     &              928.1D0, 928.1D0, 928.1D0, 972.8D0, 808.7D0,        &
     &              836.6D0, 783.1D0,1007.3D0,1012.9D0, 986.1D0,        &
     &              839.0D0, 917.8D0,1010.5D0,1010.2D0,1011.0D0,        &
     &              970.3D0, 894.0D0,1010.0D0, 874.0D0, 698.8D0,        &
     &              863.7D0, 900.1D0, 997.7D0, 976.6D0,1005.7D0,        &
     &             1000.0D0,1002.7D0, 906.9D0, 837.8D0, 965.2D0,        &
     &             1010.1D0, 973.7D0,1010.7D0,1010.2D0,1008.3D0,        &
     &             1008.1D0, 885.3D0, 990.1D0,1003.2D0,1002.7D0,        &
     &             1004.0D0, 884.4D0, 806.2D0,1006.3D0, 801.3D0,        &
     &              988.9D0, 767.3D0,1011.7D0,1013.9D0, 957.7D0,        &
     &              796.6D0,1014.8D0,1006.7D0,1008.6D0,1015.1D0,        &
     &              932.2D0,1005.2D0,1003.7D0,1003.8D0, 996.4D0,        &
     &              639.0D0, 912.2D0, 910.7D0, 812.6D0,1012.6D0,        &
     &              987.6D0, 861.4D0, 970.6D0,1003.6D0, 919.0D0,        &
     &              919.7D0, 919.6D0, 922.0D0, 998.7D0,1018.9D0,        &
     &              990.8D0,1010.7D0,1010.8D0, 880.0D0, 881.9D0,        &
     &              879.5D0, 971.8D0, 912.7D0, 950.4D0, 762.0D0,        &
     &              875.1D0, 847.9D0,1019.4D0,1016.2D0,1006.6D0,        &
     &              889.8D0,1015.3D0, 927.4D0, 999.6D0, 993.1D0,        &
     &              936.2D0,1011.6D0,1014.9D0,1010.4D0, 763.0D0,        &
     &             1011.8D0,1001.6D0,1001.5D0, 995.7D0, 922.2D0,        &
     &             1010.0D0,1005.0D0, 963.1D0,1002.7D0, 939.7D0,        &
     &              999.0D0, 945.1D0,1006.5D0,1005.3D0,1001.3D0,        &
     &              994.2D0, 931.0D0,1009.3D0,1009.3D0,1007.5D0,        &
     &              794.8D0, 846.5D0,1003.1D0, 838.7D0,1009.6D0,        &
     &              786.1D0,1016.7D0,1001.3D0, 943.9D0, 926.2D0,        &
     &             1005.0D0, 911.8D0, 911.8D0, 987.2D0, 987.0D0,        &
     &              986.3D0, 883.2D0/

      DATA ANT_DIA/ 10.0D0, 46.0D0,  5.0D0,  3.9D0, 32.0D0,  5.0D0,     &
     &               3.9D0,  5.0D0, 25.0D0,  5.0D0,  5.0D0,  5.0D0,     &
     &              10.0D0, 26.0D0, 22.0D0,  3.6D0,  3.6D0,  5.0D0,     &
     &              34.0D0, 34.0D0, 70.0D0, 34.0D0, 34.0D0,100.0D0,     &
     &               3.9D0, 25.0D0,  5.0D0, 14.2D0,  5.0D0,  3.9D0,     &
     &               5.0D0,104.0D0,  5.0D0, 11.0D0,  3.0D0, 25.9D0,     &
     &              26.0D0,  5.0D0,  5.0D0,  5.0D0, 26.0D0, 26.0D0,     &
     &              37.0D0, 25.0D0, 26.0D0,  3.9D0,  5.0D0,  3.9D0,     &
     &              25.9D0,  9.0D0,  5.0D0,  2.4D0,  3.9D0, 11.0D0,     &
     &              34.0D0, 26.0D0,  9.0D0,  3.9D0,  3.9D0, 11.0D0,     &
     &               3.0D0, 20.0D0, 25.0D0, 25.6D0, 25.0D0,  5.0D0,     &
     &               5.0D0, 10.0D0, 25.9D0, 20.0D0,  5.0D0, 32.0D0,     &
     &              14.0D0,  5.0D0, 20.0D0,  3.9D0, 11.0D0,  5.0D0,     &
     &              10.0D0,  5.0D0, 25.0D0,  3.9D0, 12.0D0,  5.0D0,     &
     &               3.9D0, 25.0D0,  0.0D0,  3.9D0, 32.0D0, 43.0D0,     &
     &              20.0D0, 25.9D0, 25.9D0, 20.0D0,  3.9D0,  9.0D0,     &
     &              20.0D0, 25.0D0, 25.0D0,  5.0D0, 43.0D0, 64.0D0,     &
     &               5.0D0,  3.9D0, 25.0D0,  5.0D0,  5.0D0,  3.9D0,     &
     &               5.0D0,  3.9D0,  5.0D0, 18.0D0, 32.0D0,  5.0D0,     &
     &               3.9D0, 12.0D0, 25.0D0,  3.9D0, 25.0D0, 15.0D0,     &
     &               6.0D0,  5.0D0,  3.8D0,  3.9D0,  5.0D0,  3.8D0,     &
     &              32.0D0, 11.0D0, 11.0D0, 64.0D0,  6.0D0,  6.0D0,     &
     &               5.0D0, 11.0D0,  3.9D0,  5.0D0,  3.9D0,  3.8D0,     &
     &               5.0D0, 32.0D0, 25.0D0, 64.0D0,  0.0D0,  3.9D0,     &
     &               3.9D0,  0.0D0,  9.0D0, 18.0D0, 20.0D0,  3.9D0,     &
     &               3.9D0, 13.7D0, 13.7D0,  5.0D0,  9.0D0,  3.9D0,     &
     &              32.0D0/

      DATA HEI_FOU/ 3.00D0,13.00D0, 0.00D0, 0.00D0, 1.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 0.50D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              3.00D0, 0.00D0, 0.80D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              3.00D0, 3.00D0, 0.00D0, 3.00D0, 3.00D0, 0.00D0,     &
     &              0.00D0, 0.50D0, 0.00D0, 4.44D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 0.00D0, 0.20D0, 0.20D0, 0.60D0,     &
     &              0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,     &
     &             10.45D0, 0.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 1.00D0, 0.10D0, 0.00D0, 0.20D0,     &
     &              0.30D0, 5.20D0, 0.00D0, 0.00D0, 0.00D0, 4.50D0,     &
     &              0.20D0, 5.49D0, 0.50D0, 0.00D0, 0.50D0, 0.00D0,     &
     &              0.00D0, 0.20D0, 0.00D0, 3.00D0, 0.00D0, 2.01D0,     &
     &              0.00D0, 0.00D0, 0.05D0, 0.00D0, 0.20D0, 0.10D0,     &
     &              0.00D0, 0.10D0, 0.50D0, 0.00D0, 0.50D0, 0.00D0,     &
     &              0.00D0, 0.50D0, 0.00D0, 0.00D0, 1.95D0,12.96D0,     &
     &              5.49D0, 0.56D0, 0.56D0, 0.05D0, 0.00D0, 1.00D0,     &
     &             11.30D0, 0.00D0, 0.50D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 0.50D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.10D0,     &
     &              0.00D0, 0.00D0, 0.50D0, 0.00D0, 4.20D0, 0.00D0,     &
     &              0.00D0, 0.10D0, 0.10D0, 0.00D0, 0.00D0, 0.10D0,     &
     &              1.00D0, 0.30D0, 0.00D0, 0.00D0, 0.20D0, 0.20D0,     &
     &              0.00D0, 0.20D0, 0.00D0, 0.00D0, 0.00D0, 0.10D0,     &
     &              0.10D0, 1.80D0, 1.00D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 0.00D0,10.60D0, 8.00D0, 0.00D0,     &
     &              0.00D0, 0.50D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              1.00D0/

      DATA DEP_FOU/50*0.0D0,2*3.00D0,25*0.0D0,3.00D0,0.0D0,3.00D0,      &
     &             33*0.0D0,3.0D0,7*0.0D0,3.0D0,3.9D0,0.0D0,0.0D0,      &
     &             3.0D0,0.0D0,0.46D0,0.0D0,0.0D0,1.50D0,6*0D0,2*3.0D0, &
     &             18*0.0D0/

      DATA XPANCOEFF/14*1.D-5,1.2D-5,40*1.D-5,1.2D-5,3*1.D-5,1.2D-5,    &
     &               87*1.D-5,0.6D-5,9*1.D-5/

      DATA XPANCOEFS/147*1.2D-5,7.2D-6,9*1.2D-5/

      DATA HEI_PIL/ 6.70D0, 9.60D0, 4.35D0, 2.80D0,21.90D0, 4.20D0,     &
     &              2.90D0, 4.30D0,13.70D0, 4.40D0, 4.40D0, 4.30D0,     &
     &              6.70D0,13.00D0,14.60D0, 3.90D0, 3.90D0, 4.30D0,     &
     &             16.80D0,16.80D0,35.00D0,16.80D0,16.80D0,50.00D0,     &
     &              2.70D0,13.70D0, 4.40D0, 3.71D0, 4.40D0, 2.90D0,     &
     &              4.40D0,52.00D0, 4.40D0, 7.50D0, 2.00D0,13.10D0,     &
     &             13.00D0, 4.30D0, 4.30D0, 4.10D0,12.70D0,16.30D0,     &
     &             13.60D0,13.70D0,13.15D0, 2.80D0, 4.30D0, 2.90D0,     &
     &              9.30D0, 6.50D0, 4.00D0, 4.00D0, 2.90D0, 7.50D0,     &
     &             16.40D0,13.20D0, 7.70D0, 2.80D0, 3.00D0, 7.50D0,     &
     &              2.00D0, 9.19D0,13.70D0,13.00D0,13.70D0, 4.40D0,     &
     &              4.20D0, 6.80D0,13.00D0,10.50D0, 4.40D0,15.60D0,     &
     &              7.00D0, 4.20D0,10.87D0, 2.80D0, 7.50D0, 4.00D0,     &
     &              5.00D0, 4.00D0,13.70D0, 2.80D0, 6.00D0, 4.40D0,     &
     &              2.80D0,13.70D0, 0.00D0, 3.00D0,15.60D0,23.46D0,     &
     &              9.19D0,13.08D0,13.08D0,10.87D0, 2.80D0, 6.20D0,     &
     &              2.90D0,12.50D0,13.70D0, 4.40D0,21.50D0,32.00D0,     &
     &              4.30D0, 2.80D0,13.70D0, 4.30D0, 4.30D0, 2.90D0,     &
     &              4.30D0, 2.80D0, 4.40D0, 0.00D0,16.00D0, 4.00D0,     &
     &              2.80D0, 6.00D0,13.70D0, 2.80D0,12.50D0, 0.00D0,     &
     &              3.00D0, 4.00D0, 4.25D0, 2.90D0, 4.60D0, 4.25D0,     &
     &             21.90D0, 6.20D0, 7.50D0,32.00D0, 1.46D0, 1.46D0,     &
     &              4.00D0, 7.50D0, 2.80D0, 4.40D0, 2.80D0, 4.25D0,     &
     &              4.00D0,17.30D0,12.50D0,32.00D0, 0.00D0, 2.80D0,     &
     &              2.90D0, 0.00D0, 6.50D0, 8.30D0, 4.00D0, 2.80D0,     &
     &              2.88D0, 9.00D0, 0.00D0, 4.20D0, 7.10D0, 2.80D0,     &
     &              21.90D0/

      DATA AXI_OFF/-0.0126D0, 0.0014D0, 0.0000D0, 0.0000D0, 0.0025D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 2.1306D0,-0.0021D0,   &
     &             -0.0021D0, 0.0000D0,-0.0025D0, 0.3060D0,-0.0018D0,   &
     &              0.0011D0, 0.0011D0, 0.0000D0,-0.0039D0,-0.0013D0,   &
     &             -0.0000D0,-0.0026D0,-0.0026D0, 0.0103D0, 0.0000D0,   &
     &              2.1324D0, 0.0000D0, 0.0060D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0,-0.0883D0,-0.0021D0,-0.0072D0, 0.0000D0,   &
     &              7.2850D0, 0.9295D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              6.6953D0,-0.0097D0, 0.0000D0, 2.1295D0, 8.1913D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 6.7007D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 0.0013D0, 0.0090D0,   &
     &             -0.0023D0, 2.4350D0, 0.0000D0, 0.0000D0, 0.0077D0,   &
     &              0.0000D0, 0.5182D0, 2.1327D0, 0.0000D0, 2.1329D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 6.6887D0,-0.0038D0,   &
     &              0.0000D0, 1.8301D0, 0.0051D0, 0.0000D0, 0.0068D0,   &
     &              0.0000D0, 0.0065D0, 0.0000D0,-0.0013D0, 0.0000D0,   &
     &              2.1343D0, 0.0000D0,-0.0031D0, 0.0000D0, 0.0000D0,   &
     &              2.1296D0, 0.0000D0, 0.0000D0, 1.8310D0,14.9398D0,   &
     &              0.5091D0, 6.7074D0, 6.7076D0, 0.5242D0, 0.0000D0,   &
     &              0.0000D0,-0.0060D0, 2.1500D0, 2.1323D0, 0.0000D0,   &
     &              0.0106D0,-0.0310D0, 0.0000D0, 0.0000D0, 2.1377D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0, 5.1822D0, 6.7060D0, 0.0000D0, 0.0000D0,   &
     &              0.0023D0, 2.1337D0, 0.0000D0, 0.0001D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0,-0.0030D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0, 0.0125D0,-0.0056D0, 0.0069D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0,-0.0004D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0006D0,   &
     &             -0.0057D0, 0.0000D0,-0.0108D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 0.3163D0,-0.0001D0, 0.0000D0,   &
     &              0.0000D0,-0.0005D0, 2.0000D0, 0.0000D0, 0.0070D0,   &
     &              0.0000D0,-0.0115D0/

      DATA HEI_VER/ 0.0000D0, 6.3000D0, 0.0000D0, 0.0000D0, 1.9000D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 4.6000D0, 1.3000D0,   &
     &              1.3000D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              0.6500D0, 0.6500D0, 0.0000D0, 2.7000D0, 2.7000D0,   &
     &              0.0000D0, 2.7000D0, 2.7000D0, 8.5000D0, 0.0000D0,   &
     &              4.6000D0, 0.0000D0, 2.8400D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 1.3000D0, 2.1500D0, 1.1000D0,   &
     &              3.4000D0, 0.0000D0, 0.0000D0, 1.3000D0, 0.0000D0,   &
     &              2.3000D0, 4.4000D0, 4.3000D0, 4.6000D0, 3.6500D0,   &
     &              1.3000D0, 1.3000D0, 0.0000D0, 2.3000D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 2.1500D0, 2.7300D0,   &
     &              0.0000D0, 1.7000D0, 0.0000D0, 0.0000D0, 2.1500D0,   &
     &              1.1000D0, 2.4400D0, 4.6000D0, 0.0000D0, 4.6000D0,   &
     &              0.0000D0, 0.0000D0, 3.8000D0, 0.0000D0, 3.8000D0,   &
     &              0.0000D0, 4.3000D0, 0.0000D0, 1.3000D0, 2.4400D0,   &
     &              0.0000D0, 2.1500D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              4.6000D0, 0.0000D0, 5.0000D0, 0.0000D0, 0.0000D0,   &
     &              4.6000D0, 0.0000D0, 0.0000D0, 4.3000D0, 2.7500D0,   &
     &              2.4400D0, 2.2200D0, 2.2200D0, 2.4400D0, 0.0000D0,   &
     &              0.0000D0, 3.4000D0, 0.0000D0, 4.6000D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 4.6000D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0, 2.3000D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              5.0000D0, 4.6000D0, 0.0000D0, 3.3400D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0, 1.9000D0, 1.2500D0, 2.1500D0, 0.0000D0,   &
     &              0.7500D0, 0.7500D0, 0.0000D0, 2.1500D0, 1.3000D0,   &
     &              1.3000D0, 0.0000D0, 0.0000D0, 0.0000D0, 4.3000D0,   &
     &              3.3000D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0, 0.0000D0, 3.0000D0, 3.7000D0, 0.0000D0,   &
     &              0.0000D0, 2.4000D0, 0.0000D0, 0.0000D0, 0.0000D0,   &
     &              0.0000D0, 1.9000D0/

      DATA HEI_SUB/ 0.00D0,18.30D0, 0.00D0, 0.00D0,10.60D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 8.80D0, 1.60D0, 1.60D0, 0.00D0,     &
     &              0.00D0, 0.00D0,10.10D0, 1.08D0, 1.08D0, 0.00D0,     &
     &             10.80D0,10.80D0, 0.00D0,10.80D0,10.80D0,28.00D0,     &
     &              0.00D0, 8.80D0, 0.00D0, 5.68D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 1.60D0, 3.81D0, 0.50D0,13.70D0,     &
     &              0.00D0, 0.00D0, 1.60D0, 0.00D0, 9.40D0,13.00D0,     &
     &             13.95D0, 8.80D0,11.03D0, 1.60D0, 1.60D0, 0.00D0,     &
     &              9.40D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 3.81D0,     &
     &             10.31D0,12.30D0, 2.90D0, 0.00D0, 0.00D0, 3.81D0,     &
     &              0.50D0, 8.60D0, 8.80D0, 0.00D0, 8.80D0, 0.00D0,     &
     &              0.00D0, 1.40D0, 0.00D0, 5.70D0, 0.00D0, 9.30D0,     &
     &              0.00D0, 1.60D0, 8.60D0, 0.00D0, 3.81D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 8.80D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 8.80D0, 0.00D0, 0.00D0, 9.30D0,18.29D0,     &
     &              8.60D0,10.97D0,10.97D0, 8.60D0, 0.00D0, 0.00D0,     &
     &              5.50D0, 0.00D0, 8.80D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 8.80D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 0.00D0, 9.40D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 8.80D0, 0.00D0, 6.70D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,     &
     &             10.60D0, 4.61D0, 3.81D0, 0.00D0, 4.50D0, 4.50D0,     &
     &              0.00D0, 3.81D0, 1.60D0, 1.60D0, 0.00D0, 0.00D0,     &
     &              0.00D0,10.80D0, 7.00D0, 0.00D0, 0.00D0, 0.00D0,     &
     &              0.00D0, 0.00D0, 0.00D0, 3.60D0, 7.90D0, 0.00D0,     &
     &              0.00D0, 3.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,     &
     &             10.60D0/

      DATA ANTLAG/7200.D0/,FOUNDLAG/21600.D0/

!********1*********2*********3*********4*********5*********6*********7**
! TEST CASE
!
! STATION TSUKUB32 (alt_azimuth- mo_azel)
!      HF = 1.8
!      HP = 17.3
!      HV = 4.3
!      HS = 10.8
!      AO = 0.0006
!      EXCOEFF = 0.0000100
!      EXCOEFA = 0.0000120
!      DECL = 0.290012027298244
!      AZ = 4.66390668470710
!      ELEV = 0.573124107666037
!      FA = 1.8
!      ATM_TEMP = 27.9
!      REF_TEMP = 14.3
!      ANTLAG = 7200
!      FOUNDLAG = 21600
!      MJD = 54690
!      TAI = 43.00
!
!STATION HARTTRAO (POLAR - MO_EQUA)
!
!      HF = 0.000
!      HP = 12.7
!      HV = 2.3
!      HS = 9.4
!      AO = 6.6953
!      EXCOEFF = 0.0000100
!      EXCOEFA = 0.0000120
!      DECL = 0.290012027298244
!      AZ = 0.962974115328189
!      ELEV = 0.427298237358420
!      FA = 1.8
!      ATM_TEMP = 3.99
!      REF_TEMP = 16.1
!      ANTLAG = 7200
!      FOUNDLAG = 21600
!      MJD = 54690
!      TAI = 43.00
!********1*********2*********3*********4*********5*********6*********7**
! START EXECUTABLE CODE
!********1*********2*********3*********4*********5*********6*********7**
       HF = 0.0D0
       HP = 0.0D0
       HV = 0.0D0
       HS = 0.0D0
       AO = 0.0D0

       ATM_TEMP = ATM_TEMP - 273.15D0

!********1*********2*********3*********4*********5*********6*********7**
! DEFINE THE BASIC ELEMENTS oF THE EQUATIONS
!********1*********2*********3*********4*********5*********6*********7**

       OBS_EPOCH = DBLE(MJD)*86400.0D0 + TAI + 32.184

       INV_LIGHT = 1/VLIGHT

       CALL FNDCHR(STANAM,STA_NAM,157,IRET)
       ARRAY_INDEX = IRET

       HF = HEI_FOU(ARRAY_INDEX)

       HP = HEI_PIL(ARRAY_INDEX)

       HV = HEI_VER(ARRAY_INDEX)

       HS = HEI_SUB(ARRAY_INDEX)

       AO = AXI_OFF(ARRAY_INDEX)

       EXCOEFF = XPANCOEFF(ARRAY_INDEX)

       EXCOEFA = XPANCOEFS(ARRAY_INDEX)

       REF_TEMP = REF_TMP(ARRAY_INDEX)

       IF (FTYPE(ARRAY_INDEX) == "FO_PRIM") THEN

          FA = 0.9D0

       ELSEIF (FTYPE(ARRAY_INDEX) == "FO_SECN") THEN

          FA = 1.8D0

       ENDIF

!********1*********2*********3*********4*********5*********6*********7**
! DEBUG
!
!       write(6,*) "OBS_EPOCH is:", OBS_EPOCH
!       write(6,*) "ANTHERDEF: STANAM is :", STANAM
!       write(6,*) "ANTHERDEF: IRET is :", IRET
!       write(6,*) "ANTHERDEF: HF is :", HF
!       write(6,*) "ANTHERDEF: HP is :", HP
!       write(6,*) "ANTHERDEF: HV is :", HV
!       write(6,*) "ANTHERDEF: HS is :", HS
!       write(6,*) "ANTHERDEF: AO is :", AO
!       write(6,*) "ANTHERDEF: EXCOEFF is :", EXCOEFF
!       write(6,*) "ANTHERDEF: EXCOEFA is :", EXCOEFA
!       write(6,*) "ANTHERDEF: REF_TEMP is :", REF_TEMP
!       write(6,*) "ANTHERDEF: FA is :", FA
!       write(6,*) "ANTHERDEF: AZ is: ",AZ
!       write(6,*) "ANTHERDEF: ELEV is :",ELEV
!       write(6,*) "ANTHERDEF: DECL is :", DECL
!       write(6,*) "ANTHERDEF: ATM_TEMP is :", ATM_TEMP
!DEBUG
!********1*********2*********3*********4*********5*********6*********7**

!********1*********2*********3*********4*********5*********6*********7**
! DEFINE THE COMMON VALUES TO ALL EQUATIONS
!********1*********2*********3*********4*********5*********6*********7**

       HFSINELEV = (HF * SIN(ELEV))

       HPSINELEV = (HP * SIN(ELEV))

       FOUND_XPAN = EXCOEFF*(ATM_TEMP*(OBS_EPOCH-FOUNDLAG) - To)

       ANT_XPAN = EXCOEFA*(ATM_TEMP*(OBS_EPOCH-ANTLAG) - To)

       FAHS = FA * HS

!********1*********2*********3*********4*********5*********6*********7**
!DEBUG
!        write(6,*) "ANTHERDEF: ANTLAG is :", ANTLAG
!        write(6,*) "ANTHERDEF: FOUNDLAG is :", FOUNDLAG
!        write(6,*) "ANTHERDEF: HFSINELEV is:", HFSINELEV
!        write(6,*) "ANTHERDEF: HPSINELEV is:", HPSINELEV
!        write(6,*) "ANTHERDEF: FOUND_XPAN is:", FOUND_XPAN
!        write(6,*) "ANTHERDEF: ANT_XPAN is:", ANT_XPAN
!        write(6,*) "ANTHERDEF: FAHS is:", FAHS
!DEBUG
!********1*********2*********3*********4*********5*********6*********7**


!********1*********2*********3*********4*********5*********6*********7**
! DETERMINE THE TOTAL DELAY BASED ON MOUNT TYPE
!********1*********2*********3*********4*********5*********6*********7**


       IF (MOTYP(ARRAY_INDEX) == "MO_XYEA") THEN

!Antenna type 1 and 2 have the same equations.
!**********************************************
! Antenna Type 1: XY EAST-WEST
!**********************************************
         write(6,*) "ANTHERDEF: IN XYEA"

        COSSIN = COS(ELEV) * SIN(AZ)

        SQRTTRIG = SQRT(1-(COSSIN * COSSIN))

        DEL_TAU1 =  FOUND_XPAN * HFSINELEV

        DEL_TAU2 = ANT_XPAN * (HPSINELEV + AO * SQRTTRIG + HV - FAHS)

        DEL_TAUT = INV_LIGHT * (DEL_TAU1 + DEL_TAU2)

      ELSEIF (MOTYP(ARRAY_INDEX) == "MO_XYNO") THEN
!**********************************************
! Antenna Type 2: XY NORTH-SOUTH
!**********************************************
         write(6,*) "ANTHERDEF: IN XYNO"

        COSSIN = COS(ELEV) * SIN(AZ)

        SQRTTRIG = SQRT(1-(COSSIN * COSSIN))

        DEL_TAU1 =  FOUND_XPAN * HFSINELEV

        DEL_TAU2 = ANT_XPAN * (HPSINELEV + AO * SQRTTRIG + HV - FAHS)

        DEL_TAUT = INV_LIGHT * (DEL_TAU1 + DEL_TAU2)

      ELSEIF (MOTYP(ARRAY_INDEX) == "MO_AZEL") THEN
!*************************************************
! Antenna Type 3: AZIMUTH-ELEVATION (alt-azimuth)
!*************************************************
        write(6,*) "ANTHERDEF: IN AZEL"

       DEL_TAU1 = FOUND_XPAN * HFSINELEV

       DEL_TAU2 = ANT_XPAN * (HPSINELEV + AO * COS(ELEV) + HV - FAHS)

       DEL_TAUT = INV_LIGHT * (DEL_TAU1 + DEL_TAU2)

!********1*********2*********3*********4*********5*********6*********7**
!DEBUG
!        write(6,*) "ANTHERDEF: DEL_TAU1 is:", DEL_TAU1
!        write(6,*) "ANTHERDEF: DEL_TAU2 is:", DEL_TAU2
!        write(6,*) "ANTHERDEF: DEL_TAUT is:", DEL_TAUT
!DEBUG
!********1*********2*********3*********4*********5*********6*********7**

      ELSEIF (MOTYP(ARRAY_INDEX) == "MO_EQUA") THEN
!*****************************************************
! Antenna Type 4: HOUR ANGLE-DECLINATION (POLAR MOUNT)
!*****************************************************
         write(6,*) "ANTHERDEF: IN EQUA"

        DEL_TAU1 = FOUND_XPAN * HFSINELEV

        DEL_TAU2 = ANT_XPAN * (HPSINELEV + AO *COS(DECL) + HV - FAHS)

        DEL_TAUT = INV_LIGHT * (DEL_TAU1 + DEL_TAU2)

!********1*********2*********3*********4*********5*********6*********7**
!DEBUG
!         write(6,*) "ANTHERDEF: DEL_TAU1 is:", DEL_TAU1
!         write(6,*) "ANTHERDEF: DEL_TAU2 is:", DEL_TAU2
!         write(6,*) "ANTHERDEF: DEL_TAUT is:", DEL_TAUT
!DEBUG
!********1*********2*********3*********4*********5*********6*********7**

       ELSEIF (MOTYP(ARRAY_INDEX) == "MO_RICH") THEN
!*********************************************************************
! Antenna Type 5: RICHMOND VLBI ANT. (POLAR MOUNT WITH AXIS DISPLACED)
!*********************************************************************
         write(6,*) "ANTHERDEF: IN RICHMOND"

        PHIo = 39.06D0*DEGRAD
        DLAMDA = -0.12D0*DEGRAD

        DEL_TAU1 = FOUND_XPAN * HFSINELEV

        TRIG1 = SIN(ELEV) * SIN(PHIo)

        TRIG2 = COS(ELEV) * COS(PHIo)

        TRIG3 = COS(AZ) * COS(DLAMDA) + SIN(AZ) * SIN(DLAMDA)

        SQRTTRIG = SQRT(1 - (TRIG1 + TRIG2 * TRIG3)*                   &
     &                      (TRIG1 + TRIG2 * TRIG3))

        DEL_TAU2 = ANT_XPAN + HPSINELEV + AO * SQRTTRIG + HV - FAHS

        DEL_TAUT = INV_LIGHT * (DEL_TAU1 + DEL_TAU2)

      ENDIF

      DELAY = DEL_TAUT

      RETURN

!       END PROGRAM ANTHERDEF
        END SUBROUTINE ANTHERDEF
