# printenv > output_text.txt
#set > output_text.txt  
set -x
SATELLITE=st
MATPRE=40

### CHOOSE DENSITY MODEL:

DEN_DIR=msis00
SETUP_DEN_DIR=msis
GDYN_version=_msis00_f90
ACCELS=acceloff
ARC_name=st031221_2wk
ARC=${1:-${ARC_name}}


FTN05DIR=/data/geodyn_proj/runs_geodyn/st/setups/${SETUP_DEN_DIR}_${ACCELS}
INPUTDIR=$FTN05DIR

SERIES=${DEN_DIR}_${ACCELS}
G2SDIR=/data/geodyn_proj/geodyn_code/IIS/ORIG
G2EDIR=/data/geodyn_proj/geodyn_code/IIE/MODS${GDYN_version}
EMATUDIR=/data/geodyn_proj/runs_geodyn/extra_dirs           
COMMONDIR=/data/geodyn_proj/runs_geodyn/extra_dirs                  
COMMONHD4DIR=/data/geodyn_proj/runs_geodyn/extra_dirs               
#STATIONDIR=/Volumes/PODA/system/stations                ### should I make this?
#SYSDIR=/home/m_geodyn
#ORISMBIASDIR=/Volumes/HD4/jwbeall/dor/j2/g2b/mbias 
DIRGRAV=/data/geodyn_proj/runs_geodyn/st/gravity                       
#FTN05DIR=/data/runs_geodyn/st/setups            
DELDIR=/data/geodyn_proj/runs_geodyn/extra_dirs/deletes/$SATELLITE                 
G2BDIR=/data/geodyn_proj/runs_geodyn/$SATELLITE/g2b                   
ATGRAVDIR=/data/geodyn_proj/runs_geodyn/$SATELLITE/atgrav             
OUTPUTDIR=/data/data_geodyn/results/$SATELLITE/${DEN_DIR}/${SERIES}                   
mkdir /data/data_geodyn/results/$SATELLITE/${DEN_DIR}
/data/data_geodyn/results//data/data_geodyn/results/$SATELLITE/${DEN_DIR}/${SERIES}
mkdir $OUTPUTDIR
BINDIR=/data/geodyn_proj/runs_geodyn/extra_dirs/bin
mkdir /data/data_geodyn/results
mkdir /data/data_geodyn/results/$SATELLITE
mkdir $OUTPUTDIR

### INPUT FILES
### UNIT5 for GEODYN
SATID=${2-7501001}
ARCFIL=$ARC
SOLRADFIL=${ARCFIL}.goco05s

### GEODYN DATA FILE (GEODYN2 BINARY FILE FOR OBSERVATIONS)
G2BFIL=${3:-starlette_03_slrg2b.rm0} 

### GRAVITY FIELD
GRAVITY=goco05s 

### ATMOSPHERIC GRAVITY
ATGRAVFIL=${4:-ATGRAV.glo-3HR_20030101-20080204_1860_AOD1B_0006.0090}


echo "    Density Model:     " $DEN_DIR
echo "    GEODYN Version:    " $GDYN_version
echo "    Estimate GenAccel: " $ACCELS
echo "    ARC run:           " $ARC
echo "    Output directory:  " $OUTPUTDIR



mkdir /data/geodyn_proj/runs_geodyn/tmp/
mkdir /data/geodyn_proj/runs_geodyn/tmp/$SATELLITE
TMPDIR=/data/geodyn_proj/runs_geodyn/tmp/$SATELLITE/$SERIES 
mkdir $TMPDIR
#echo $TMPDIR
#echo $PWD




###Create temporary directory to do GEODYN runs
mkdir $TMPDIR
chmod 777 $TMPDIR
#echo "fichier grav " $ATGRAVDIR/$ATGRAVFIL

if  test -r $INPUTDIR/${ARCFIL}.bz2; then
  echo "    INPUT file:        " ${INPUTDIR}/${ARCFIL}.bz2
 else
  echo " INPUT FILE ${INPUTDIR}/${ARCFIL} not found."
  exit
fi

#if  test -r $INPUTDIR/${ARCFIL}; then
#  echo " INPUT file is " ${INPUTDIR}/${ARCFIL}
# else
#  echo " INPUT FILE ${INPUTDIR}/${ARCFIL} not found."
#  exit
#fi

if test -r $G2BDIR/${G2BFIL}; then
#  echo " FORT.40 file is "$G2BDIR/${G2BFIL}
  echo " " 
 else
 echo " FORT.40 file" $G2BFIL "not found."
 exit
fi
date
##
rm -rf $TMPDIR/${ARC}.$GRAVITY
mkdir $TMPDIR/${ARC}.$GRAVITY
chmod 777 $TMPDIR/${ARC}.$GRAVITY
cd $TMPDIR/${ARC}.$GRAVITY
echo $PWD
echo $ARC | cut -c 3-8 > YMD
YMD=`cat YMD`
echo $ARC |cut -c 3-4 > yr
YR=`cat yr`


rm -f EXAT01
rm -f iieout iisout ftn* phobos.ephem deimos.ephem
rm -f giie.* iieerr iiserr ftn* template1 template2
rm -f pin* res*  ht* *.ps *.pxy doppler.* del*  RESIDS.${1}
rm -f SAVE.*
mkdir $OUTPUTDIR/
#mkdir $OUTPUTDIR/XYZOUT/
#mkdir $OUTPUTDIR/PREPRO/
mkdir $OUTPUTDIR/sumry/
mkdir $OUTPUTDIR/orbits/
mkdir $OUTPUTDIR/RESIDS/
mkdir $OUTPUTDIR/PUNCH/
mkdir $OUTPUTDIR/IIEOUT/
mkdir $OUTPUTDIR/TELEM
mkdir $OUTPUTDIR/EMAT
mkdir $OUTPUTDIR/EMAT/scans
mkdir $OUTPUTDIR/IISSET
mkdir $OUTPUTDIR/all_outputs/
mkdir $OUTPUTDIR/DENSITY/
mkdir $OUTPUTDIR/XYZ_TRAJ/
mkdir $OUTPUTDIR/KEP_TRAJ/



rm -f $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}.gz
rm -f $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}.bz2

rm -f $OUTPUTDIR/XYZ_TRAJ/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/XYZ_TRAJ/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/XYZ_TRAJ/${ARC}.${GRAVITY}.gz
rm -f $OUTPUTDIR/XYZ_TRAJ/${ARC}.${GRAVITY}.bz2

rm -f $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}.gz
rm -f $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}.bz2

rm -f $OUTPUTDIR/KEP_TRAJ/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/KEP_TRAJ/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/KEP_TRAJ/${ARC}.${GRAVITY}.gz
rm -f $OUTPUTDIR/KEP_TRAJ/${ARC}.${GRAVITY}.bz2


#rm -f $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/orbits/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/orbits/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/orbits/${ARC}.${GRAVITY}.gz
rm -f $OUTPUTDIR/IIEOUT/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/IIEOUT/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/IIEOUT/${ARC}.${GRAVITY}.bz2
rm -f $OUTPUTDIR/sumry/${ARC}.${GRAVITY} 
#rm -f $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}
#rm -f $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}.Z
#rm -f $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}.bz2 
rm -f $OUTPUTDIR/RESIDS/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/RESIDS/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/RESIDS/${ARC}.${GRAVITY}.gz

rm -f $OUTPUTDIR/IISSET/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/IISSET/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/IISSET/${ARC}.${GRAVITY}.gz
rm -f $OUTPUTDIR/IISSET/${ARC}.${GRAVITY}.bz2

rm -f $OUTPUTDIR/PUNCH/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/TELEM/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/EMAT/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}.Z
#
#
rm -f dum2 dum1 dum3
cp $INPUTDIR/${ARCFIL}.bz2 inputfile.bz2 
bunzip2 inputfile.bz2
### Construct Common Setup
######################
ln -s  ${G2BDIR}/${G2BFIL} ftn40

# get the gravity field				
cp ${DIRGRAV}/grvfld.${GRAVITY} ftn12
!head ftn12
if [ $YR -ge 0  -a $YR -lt 18 ]; then 
ln -s /data/geodyn_proj/runs_geodyn/st/ephem/ephem1421.data ftn01
fi
#if [ $YR -ge 18 -a $YR -le 25 ]; then 
#ln -s  /Users/geodyn/SUPPORT/ephem1430.data_2025 ftn01
#fi
#if [ $YR -ge 51  ]; then 
#ln -s /Users/geodyn/SUPPORT/ephem1421.data ftn01
#fi

##
cp /data/geodyn_proj/geodyn_code/RUNS/INPUTS/gdntable.data ftn02      #/Users/geodyn/SUPPORT/DAILY_TABLES/gdntable.data ftn02   # do i need to grab the new ones of these?
##
mv inputfile giis.input
cp giis.input ftn05
#
##ATGRAV information goes on unit18
#echo "voila le fichier" $ATGRAVDIR/$ATGRAVFIL

ln -s $ATGRAVDIR/$ATGRAVFIL fort.18

#echo "this is the file loc"
#pwd
#date
#
$G2SDIR/giis2002_gfortran > iisout 2>iiserr

date
##Save the Interface files from 2s. Then cleanup temporary files.
mv ftn41 giie.ft12
mv ftn11 giie.ft11
## exit 0
rm -f ftn* fort.*
echo " #"
echo " #		End of IIS"
echo " #"
echo " #		Run IIE"
echo " #"
#
##Interface files in 2e must be ftn11, ftn12
cp giie.ft11 ftn11
cp giie.ft12 ftn12
# ramp load module (2e)
date
$G2EDIR/giie2002_gfortran >iieout 2>iieerr
echo " #                End of IIE"
date

### PERL utility to get updated punch from geodyn output
#####Does not update EPOCH cards. Use GEODYN punch file (ftn07)
#####To get updated EPOCH cards and Elements.
##### This turn_arc_params.pl utility works on the entire setup;
##### -It will not work on the arc setup only.
##
/users/flemoine/bin/turn_arc_params.pl giis.input iieout 0 -1
cat iieerr iieout > blob
date
rm -f fort.11 fort.12 fort.13 fort.14
cat iisout iiserr iieout iieerr > IIEOUT.${ARC}.${GRAVITY}
mv fort.9 sumry
mv fort.19 Resid
cat fort.7 ftn07 > punch.gdn
fgrep EPOCH punch.gdn > sumry1
cat sumry1 sumry > blob
mv blob sumry
mv fort.71 emat
mv ftn97 telem
mv ftn08 xyzout
mv ftn10 aeiout
mv fort.8 ascii_xyz
mv fort.10 ascii_kep
mv fort.30 orbfil
mv fort.31 orbfil2
mv fort.99 densityfil
mv fort.98 msis_in_file
mv fort.101 msis_out_file
mv fort.103 msis_SWI_file
rm -f slvtmp* ftn* fort.*
$EMATUDIR/ematu <<EOF 2>err >output
emat
2
EOF
cat err output > blob
mv blob output.scan

echo " #               Finished renaming files"

rm -r WORK
rm -f $OUTPUTDIR/orbits/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/orbits/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/orbits/${ARC}.${GRAVITY}.gz

rm -f  $OUTPUTDIR/sumry/${ARC}.${GRAVITY} 
rm -f  $OUTPUTDIR/RESIDS/${ARC}.${GRAVITY}
rm -f  $OUTPUTDIR/RESIDS/${ARC}.${GRAVITY}.Z
rm -f  $OUTPUTDIR/RESIDS/${ARC}.${GRAVITY}.gz


rm -f  $OUTPUTDIR/IIEOUT/${ARC}.${GRAVITY}
rm -f  $OUTPUTDIR/IIEOUT/${ARC}.${GRAVITY}.Z
rm -f  $OUTPUTDIR/IIEOUT/${ARC}.${GRAVITY}.bz2
rm -f  $OUTPUTDIR/PUNCH/${ARC}.${GRAVITY}
rm -f  $OUTPUTDIR/EMAT/${ARC}.${GRAVITY}
rm -f  $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}
rm -f  $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}.Z
rm -f  $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}.bz2

rm -f $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}.gz

rm -f $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}.gz

rm -f $OUTPUTDIR/XYZ_TRAJ/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/XYZ_TRAJ/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/XYZ_TRAJ/${ARC}.${GRAVITY}.gz
rm -f $OUTPUTDIR/KEP_TRAJ/${ARC}.${GRAVITY}
rm -f $OUTPUTDIR/KEP_TRAJ/${ARC}.${GRAVITY}.Z
rm -f $OUTPUTDIR/KEP_TRAJ/${ARC}.${GRAVITY}.gz

echo " #               Finished removing files"

##
cp giis.input  $OUTPUTDIR/IISSET/${ARC}.${GRAVITY}
#bzip2          $OUTPUTDIR/IISSET/${ARC}.${GRAVITY}

cp output.scan $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}
#bzip2          $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}
cp emat  $OUTPUTDIR/EMAT/${ARC}.${GRAVITY}
cp sumry $OUTPUTDIR/sumry/${ARC}.${GRAVITY} 
cp Resid $OUTPUTDIR/RESIDS/${ARC}.${GRAVITY}
cp punch $OUTPUTDIR/PUNCH/${ARC}.${GRAVITY}
cp punch.gdn   $OUTPUTDIR/PUNCH/${ARC}.${GRAVITY}.gdn
mv IIEOUT.${ARC}.${GRAVITY} $OUTPUTDIR/IIEOUT/${ARC}.${GRAVITY}

cp orbfil $OUTPUTDIR/orbits/${ARC}.${GRAVITY}_orb1
cp orbfil2 $OUTPUTDIR/orbits/${ARC}.${GRAVITY}_orb2

mv telem   $OUTPUTDIR/TELEM/${ARC}.${GRAVITY}

cp densityfil $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}
cp msis_in_file $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}_msisin
cp msis_out_file $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}_msisout
cp msis_SWI_file $OUTPUTDIR/DENSITY/${ARC}.${GRAVITY}_msisSWI
#cp xyzout $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}
#cp aeiout $OUTPUTDIR/all_outputs/${ARC}.${GRAVITY}
cp ascii_xyz $OUTPUTDIR/XYZ_TRAJ/${ARC}.${GRAVITY}
cp ascii_xyz $OUTPUTDIR/KEP_TRAJ/${ARC}.${GRAVITY}

echo " #               Finished copying files to outputdir"
#rm -f $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}
#rm -f $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}.Z
#rm -f $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}.bz2
#mv xyzout  $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}
#mv aeiout  $OUTPUTDIR/AEIOUT/${ARC}.${GRAVITY}
#compress $OUTPUTDIR/XYZOUT/${ARC}.${GRAVITY}
##
cd ..
rm -rf $TMPDIR/${ARC}.${GRAVITY}

echo " #***************End of Script"
