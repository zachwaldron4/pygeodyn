#     def setup_directory_structure_and_rungeodyn(self):
#         ####-------------------------------------------------------------
#         ####       Setup Directory Structure 
#         ####-------------------------------------------------------------
#         if self.verbose:
#             def verboseprint(*args, **kwargs):
#                     print(*args, **kwargs)
#         else:
#             verboseprint = lambda *a, **k: None # do-nothing function

        
#         path_run_sat     = '/data/geodyn_proj/runs_geodyn/'+self.SATELLITE
#         path_run_inputs  = '/data/data_geodyn/inputs/'+self.SATELLITE
#         path_run_outputs  = '/data/data_geodyn/results/'+self.SATELLITE
#         path_geodyn_code = '/data/geodyn_proj/geodyn_code'
#         path_extra_dirs  = '/data/data_geodyn/extra_dirs'

#         #### Run Series (This will be a output directory identifier)
#         SERIES = self.DEN_DIR + '_' + self.ACCELS + self.SpecialRun_name

#         #### Point to the GEODYN executables
#         G2SDIR      = path_geodyn_code + '/IIS/ORIG'
#         G2EDIR      = path_geodyn_code + '/IIE/' + self.GDYN_version

#         #### Extra and Temporary directories
#         EMATUDIR     = path_extra_dirs          
#         COMMONDIR    = path_extra_dirs                 
#         COMMONHD4DIR = path_extra_dirs             
#         DELDIR       = path_extra_dirs + '/deletes/'+self.SATELLITE                 
#         BINDIR       = path_extra_dirs + '/bin'
#         TMPDIR       = path_extra_dirs +'/tmp/'+self.SATELLITE+'/'+SERIES 

#         #### Control Inputs for the GEODYN run
#             ## Input Setup File directory
#         FTN05DIR  = path_run_sat + '/setups/'+self.SETUP_DEN_DIR+'_'+self.ACCELS
#         INPUTDIR  = FTN05DIR

#         ## Input other necessary directorys
#         DIRGRAV   = path_run_inputs + '/gravity'
#         G2BDIR    = path_run_inputs + '/g2b'
#         ATGRAVDIR = path_run_inputs + '/atgrav'
#         EPHEMDIR  = path_run_inputs + '/ephem'
#         OUTPUTDIR = path_run_outputs + '/'+self.DEN_DIR+'/'+SERIES

        
#         #-------------------------------------------------------------
#         #  Make Directories if they do not exists
#         #-------------------------------------------------------------

#         #### If the below directories do not exists, build them:    
#         ## make output directory 
#         self.make_directory_check_exist(path_run_outputs)
#         self.make_directory_check_exist(path_run_outputs + '/'+self.DEN_DIR)
#         self.make_directory_check_exist(OUTPUTDIR) 


#         ## Make temporary directory path
#         self.make_directory_check_exist(path_extra_dirs) 
#         self.make_directory_check_exist(path_extra_dirs+'/tmp') 
#         self.make_directory_check_exist(path_extra_dirs+'/tmp/'+self.SATELLITE)
#         self.make_directory_check_exist(path_extra_dirs+'/tmp/'+self.SATELLITE+'/'+SERIES)

# #         sys.exit("Want to stop code")
#         ####-------------------------------------------------------------
#         ####      Control Input Files:
#         ####-------------------------------------------------------------

#         ### Arc File
#         ARC     = self.ARC_name
#         ARCFIL  = ARC

#         ### Gravity Field
#         GRAVITY = 'goco05s'

#         ### Solar Radiation file
#         SOLRAD_filename = ARCFIL+'.'+GRAVITY

#         ### GEODYN DATA FILE (GEODYN2 BINARY FILE FOR OBSERVATIONS)
#         G2B_filename    ='starlette_03_slrg2b.rm0'
#             # G2BFIL=${3:-starlette_03_slrg2b.rm0} 

#         ### ATMOSPHERIC GRAVITY
#         ATGRAV_filename ='ATGRAV.glo-3HR_20030101-20080204_1860_AOD1B_0006.0090'
#             # ATGRAVFIL=${4:-ATGRAV.glo-3HR_20030101-20080204_1860_AOD1B_0006.0090}
        
#         now = datetime.now()
#         current_time = now.strftime("%H:%M:%S")
#         print(self.run_ID,"    Current Time =     ", current_time)
#         print()
#         print(self.run_ID,"    Density Model:     " ,self.DEN_DIR)
#         print(self.run_ID,"    GEODYN Version:    " ,self.GDYN_version)
#         print(self.run_ID,"    Estimate GenAccel: " ,self.ACCELS)
#         print(self.run_ID,"    ARC run:           " ,ARC)
#         print(self.run_ID,"    Output directory:  " ,OUTPUTDIR)
#         print(self.run_ID,"    Call Options:     ",self.options_in)

        
        
#         ####-------------------------------------------------------------
#         ####      Double check that the input files exist:
#         ####-------------------------------------------------------------

#         _INPUT_filename = INPUTDIR +'/'+ ARCFIL+'.bz2'
#         _G2B_filename   = G2BDIR   +'/'+ G2B_filename

#         if os.path.exists(_INPUT_filename):
#             verboseprint("    FORT.5  (input) file:  ", _INPUT_filename)
#         else:
#             print(self.run_ID,"    FORT.5  (input) file:  ", _INPUT_filename," not found.")    

#         if os.path.exists(_G2B_filename):
#             verboseprint("    FORT.40 (g2b)   file:  ", _G2B_filename)
#         else:
#             print(self.run_ID,"    FORT.40 (g2b)   file:  ", _G2B_filename," not found.")    

#         ####-------------------------------------- 
#         ####      Start Running Commands:
#         ####-------------------------------------- 

#         #### Remove old TMPRDIR version and remake it 
#         os.system('rm -rf '+TMPDIR+'/'+ARC)
# #         time.sleep(0.5)

#         self.make_directory_check_exist(TMPDIR+'/'+ARC) 
#         os.system('chmod 777 '+TMPDIR+'/'+ARC)
# #         time.sleep(0.5)


#         #### Save the YMD and YR as files
#         YMD = ARC[2:8]
#         yr  = ARC[2:4] 

#         f = open(TMPDIR+'/'+ARC+'/'+"YMD", "w")
#         f.write(YMD)
#         f.close()

#         f = open(TMPDIR+'/'+ARC+'/'+"YR", "w")
#         f.write(yr)
#         f.close()

#         TMPDIR_arc = TMPDIR+'/'+ARC
        
#         #### Clean the directory of necessary GEODYN inputs
        
#         #### Navigate TO the TMPDIR
#         os.chdir(TMPDIR_arc)
# #         time.sleep(0.5)

#         os.system('rm -f EXAT01')
# #         time.sleep(0.5)
#         os.system('rm -f iieout iisout ftn* phobos.ephem deimos.ephem')
# #         time.sleep(0.5)
#         os.system('rm -f giie.* iieerr iiserr ftn* template1 template2')
# #         time.sleep(0.5)
#         os.system('rm -f pin* res*  ht* *.ps *.pxy doppler.* del*  RESIDS.${1}')
# #         time.sleep(0.5)
#         os.system('rm -f SAVE.*')

#         # #### Make the GEODYN output directories to be saved later

#         self.make_directory_check_exist(OUTPUTDIR+'/orbits/')
#         self.make_directory_check_exist(OUTPUTDIR+'/RESIDS/')
#         self.make_directory_check_exist(OUTPUTDIR+'/PUNCH/')
#         self.make_directory_check_exist(OUTPUTDIR+'/IIEOUT/')
#         self.make_directory_check_exist(OUTPUTDIR+'/TELEM/')
#         self.make_directory_check_exist(OUTPUTDIR+'/EMAT/')
#         self.make_directory_check_exist(OUTPUTDIR+'/EMAT/scans/')
#         self.make_directory_check_exist(OUTPUTDIR+'/IISSET/')
#         self.make_directory_check_exist(OUTPUTDIR+'/DENSITY/')
#         self.make_directory_check_exist(OUTPUTDIR+'/XYZ_TRAJ/')
#         self.make_directory_check_exist(OUTPUTDIR+'/KEP_TRAJ/')

        
        
#         ####-------------------------------------------------------------
#         ####     Construct Common Setup of a GEODYN RUN
#         ####-------------------------------------------------------------

#         ##### TODO Add an if statement to choose correct ephemeris file.
#         _grav_field_filename = DIRGRAV +'/grvfld.'+GRAVITY
#         _ephem_filename      = EPHEMDIR+'/ephem1421.data'
#         _gdntable_filename   = '/data/geodyn_proj/geodyn_code/RUNS/INPUTS/gdntable.data'
#         _ATGRAV_filename     = ATGRAVDIR +'/'+ ATGRAV_filename

#         #### make symlink to the G2B file and save as ftn40
#         if not os.path.exists(TMPDIR_arc +'/ftn40'):
#             os.symlink(_G2B_filename, TMPDIR_arc +'/ftn40')
# #             time.sleep(0.5)
#         else:
#             verboseprint('symlink is set up: g2b file')

#         #### make symlink to the gravity field and save as ftn12
#         if not os.path.exists(TMPDIR_arc +'/ftn12'):
#             os.symlink(_grav_field_filename, TMPDIR_arc +'/ftn12')
# #             time.sleep(0.5)
#         else:
#             verboseprint('symlink is set up: grav_field file')

#         #### make symlink to the ephemerides and save as ftn01
#         if not os.path.exists(TMPDIR_arc +'/ftn01'):
#             os.symlink(_ephem_filename, TMPDIR_arc +'/ftn01')
# #             time.sleep(0.5)
#             verboseprint('linked:   ephem file > ftn01')
#         else:
#             verboseprint('symlink is set up: ephem file')


#         #### make symlink to the gdntable and save as ftn02
#         if not os.path.exists(TMPDIR_arc +'/ftn02'):
#             os.symlink(_gdntable_filename, TMPDIR_arc +'/ftn02')
# #             time.sleep(0.5)
#             verboseprint('linked:   gdntable  > ftn02')
#         else:
#             verboseprint('symlink is set up: gdntable file')


#         #### make symlink to the ATGRAVFIL and save as fort.18
#         if not os.path.exists(TMPDIR_arc +'/fort.18'):
#             os.symlink(_ATGRAV_filename, TMPDIR_arc +'/fort.18')
# #             time.sleep(0.5)

#             verboseprint('linked:   atgrav  > fort.18')
#         else:
#             verboseprint('symlink is set up: atgrav file')


#         if not os.path.exists(TMPDIR_arc+'/ftn05.bz2'):
#             os.system('cp '+_INPUT_filename+' '+TMPDIR_arc+'/ftn05.bz2')
# #             time.sleep(0.5)
#             verboseprint('copying          : iieout file')
#         else:
#             verboseprint('copied           : iieout file')
        
#         if not os.path.exists(TMPDIR_arc+'/ftn05'):
#             os.system('bunzip2 '+TMPDIR_arc+'/ftn05.bz2')
# #             time.sleep(0.5)
#             verboseprint('unzip            : iieout file')
#         else:
#             verboseprint('unzipped         : iieout file')

#         if not os.path.exists(TMPDIR_arc+'/giis.input'):
# #             time.sleep(0.5)
#             os.system('cp  '+TMPDIR_arc+'/ftn05 '+TMPDIR_arc+'/giis.input')
#             verboseprint('copy             : giis.input file')
#         else:
#             verboseprint('copied           : giis.input file')   


            
#         ####-------------------------------------
#         ####     RUN GEODYN IIS
#         ####-------------------------------------

#         ### Must change directory to run the IIS executable
#         os.chdir(TMPDIR_arc)
#         time.sleep(1)

#         #### Before running GEODYN, populate the geodyn_options.txt file with the run options:
#         self.geodyn_modify_inputs(self.options_in, self.DEN_DIR)
        
        
#         #### RUN THE EXECUTABLE
#         command_IIS = G2SDIR+'/giis2002_gfortran > '+'iisout 2> '+'iiserr'
#         subprocess.run(command_IIS, shell = True)
#         time.sleep(0.5)

#         #### Save the Interface files from 2s. 
#         if not os.path.exists('giie.ft12'):
#             os.system('cp ftn41 giie.ft12')
# #             time.sleep(0.5)
#             os.system('cp giie.ft12 ftn12')
# #             time.sleep(0.5)
#         else:
#             verboseprint('ft12 is ready')

#         if not os.path.exists('giie.ft11'):
#             os.system('cp ftn11 giie.ft11')
# #             time.sleep(0.5)
#             os.system('cp giie.ft11 ftn11')
# #             time.sleep(0.5)
#         else:
#             verboseprint('ft11 is ready')


#         ###  Then cleanup temporary files.
#         os.system('rm -f ftn* fort.*')
# #         time.sleep(0.5)
#         print(self.run_ID,"         End of  IIS" )
#         print(self.run_ID,"         Running IIE" )

#         now = datetime.now()
#         current_time = now.strftime("%H:%M:%S")
#         print(self.run_ID,"         Current Time =", current_time)

#         ####-------------------------------------
#         ####     RUN GEODYN IIE
#         ####-------------------------------------


#         ##Interface files in 2e must be ftn11, ftn12
#         if not os.path.exists('ftn12'):
#             os.system('cp giie.ft12 ftn12')
# #             time.sleep(0.5)
#         else:
#             verboseprint('ftn12 is ready')

#         if not os.path.exists('ftn11'):
#             os.system('cp giie.ft11 ftn11')
# #             time.sleep(0.5)
#         else:
#             verboseprint('ftn11 is ready')

#         start = time.time()
#         #### RUN THE EXECUTABLE
       
#         command_IIE = G2EDIR+'/giie2002_gfortran > '+'iieout 2> '+'iieerr'
#         time.sleep(0.5)
#         subprocess.run(command_IIE, shell = True)
#         time.sleep(0.5)
#         print(self.run_ID,"         End of IIE" )
#         end = time.time()
#         elapsed = end - start
#         print(self.run_ID,'         Time of IIE: ',elapsed,'secs')

#         now = datetime.now()
#         current_time = now.strftime("%H:%M:%S")
#         print(self.run_ID,"         Current Time =", current_time)

            
#         ####-------------------------------------
#         ####     After the IIS and IIE runs:
#         ####-------------------------------------

#         #### ZACH-- the below bit is from frank and I'm not sure what it is meant to do...
#         ## PERL utility to get updated punch from geodyn output
#         ## Does not update EPOCH cards. Use GEODYN punch file (ftn07)
#         ## To get updated EPOCH cards and Elements.
#         ## This turn_arc_params.pl utility works on the entire setup;
#         ## -It will not work on the arc setup only.
#         ##
#         # /users/flemoine/bin/turn_arc_params.pl giis.input iieout 0 -1
#         #### End Frank bit.
#         #--------------------------


#         #### Remove files that won't be needed...
#         os.system('rm -f fort.11 fort.12 fort.13 fort.14')
# #         time.sleep(0.5)

#         #### Combine the iisout, iiserr and the new iieout files...
#         os.system('cat iisout iiserr iieout iieerr > '+'IIEOUT.'+ARC+'.'+GRAVITY)
# #         time.sleep(0.5)

#         #### Rename the summary file to save
#         os.system('mv fort.9 sumry')
# #         time.sleep(0.5)

#         #### Rename the Resisual file to save
#         os.system('mv fort.19 Resid')
# #         time.sleep(0.5)

#         #### make the "punch file". 
#         # Punch file just contains special geodyn stuff, it is not important for us.
#         os.system('cat fort.7 ftn07 > punch.gdn')
# #         time.sleep(0.5)

#         #### Continue adding to the blob file 
#         os.system('fgrep EPOCH punch.gdn > sumry1')
# #         time.sleep(0.5)
#         os.system('cat sumry1 sumry > blob')
# #         time.sleep(0.5)



#         ####-----------------------------------------
#         ####     Rename the files that will be saved:
#         ####-----------------------------------------

#         ## Check which of the files exist, and report to the user that some don't exist:

#         output_files = {
#                         'fort.71': 'emat',
#                         'ftn97'  : 'telem',
#                         'ftn08'  : 'xyzout',
#                         'ftn10': 'aeiout' ,
#                         'fort.8': 'ascii_xyz',
#                         'fort.10': 'ascii_kep',
#                         'fort.30': 'orbfil',
#                         'fort.31': 'orbfil2',
#                         'fort.99': 'densityfil',
#                         'fort.98': 'msis_in_file',
#                         'fort.101':'msis_out_file' ,
#                         'fort.103':'msis_SWI_file' ,
#                         }

#         for i,val in enumerate(output_files):
#             if not os.path.exists(val):
#                 print('File is not in',self.run_ID,':',val,'--', output_files[val] )
#             else:
#                 pass
# #         time.sleep(0.5)
#         os.system('mv blob sumry')
#         os.system('mv fort.71 emat')
#         os.system('mv ftn97 telem')
#         os.system('mv ftn08 xyzout')
#         os.system('mv ftn10 aeiout')
#         os.system('mv fort.8 ascii_xyz')
#         os.system('mv fort.10 ascii_kep')
#         os.system('mv fort.30 orbfil')
#         os.system('mv fort.31 orbfil2')
#         os.system('mv fort.99 densityfil')
#         os.system('mv fort.98 msis_in_file')
#         os.system('mv fort.101 msis_out_file')
#         os.system('mv fort.103 msis_SWI_file')
#         os.system('rm -f slvtmp* ftn* fort.*')
# #         time.sleep(0.5)
#         # $EMATUDIR/ematu <<EOF 2>err >output
#         # emat
#         # 2
#         # EOF
#         # cat err output > blob
#         # mv blob output.scan

#         print(self.run_ID,'               Finished renaming files')      
        
        
        
        
        


#         ####-----------------------------------------
#         ####     Save files to their directories:
#         ####-----------------------------------------

#         #### First remove all files currently stored in the output directory
#         os.system('rm -f  '+OUTPUTDIR+'/sumry/${ARC}.'+ GRAVITY+'')
#         os.system('rm -f  '+OUTPUTDIR+'/PUNCH/${ARC}.'+ GRAVITY+'')
#         # os.system('rm -f  '+OUTPUTDIR+'/EMAT/${ARC}.'+ GRAVITY+'')

#         os.system('rm -f '+OUTPUTDIR+'/orbits/${ARC}.'+ GRAVITY+'')
#         os.system('rm -f '+OUTPUTDIR+'/orbits/${ARC}.'+ GRAVITY+'.Z')
#         os.system('rm -f '+OUTPUTDIR+'/orbits/${ARC}.'+ GRAVITY+'.gz')

#         os.system('rm -f  '+OUTPUTDIR+'/RESIDS/${ARC}.'+ GRAVITY+'')
#         os.system('rm -f  '+OUTPUTDIR+'/RESIDS/${ARC}.'+ GRAVITY+'.Z')
#         os.system('rm -f  '+OUTPUTDIR+'/RESIDS/${ARC}.'+ GRAVITY+'.gz')

#         os.system('rm -f  '+OUTPUTDIR+'/IIEOUT/${ARC}.'+ GRAVITY+'')
#         os.system('rm -f  '+OUTPUTDIR+'/IIEOUT/${ARC}.'+ GRAVITY+'.Z')
#         os.system('rm -f  '+OUTPUTDIR+'/IIEOUT/${ARC}.'+ GRAVITY+'.bz2')

#         # os.system('rm -f  '+OUTPUTDIR+'/EMAT/scans/scan.'+ ARC+'.'+GRAVITY+'')
#         # os.system('rm -f  '+OUTPUTDIR+'/EMAT/scans/scan.'+ ARC+'.'+GRAVITY+'.Z')
#         # os.system('rm -f  '+OUTPUTDIR+'/EMAT/scans/scan.'+ ARC+'.'+GRAVITY+'.bz2')

#         os.system('rm -f '+OUTPUTDIR+'/DENSITY/'+ ARC+'.'+GRAVITY+'')
#         os.system('rm -f '+OUTPUTDIR+'/DENSITY/'+ ARC+'.'+GRAVITY+'.Z')
#         os.system('rm -f '+OUTPUTDIR+'/DENSITY/'+ ARC+'.'+GRAVITY+'.gz')

#         os.system('rm -f '+OUTPUTDIR+'/XYZ_TRAJ/'+ ARC+'.'+GRAVITY+'')
#         os.system('rm -f '+OUTPUTDIR+'/XYZ_TRAJ/'+ ARC+'.'+GRAVITY+'.Z')
#         os.system('rm -f '+OUTPUTDIR+'/XYZ_TRAJ/'+ ARC+'.'+GRAVITY+'.gz')

#         os.system('rm -f '+OUTPUTDIR+'/KEP_TRAJ/'+ ARC+'.'+GRAVITY+'')
#         os.system('rm -f '+OUTPUTDIR+'/KEP_TRAJ/'+ ARC+'.'+GRAVITY+'.Z')
#         os.system('rm -f '+OUTPUTDIR+'/KEP_TRAJ/'+ ARC+'.'+GRAVITY+'.gz')



#         os.system('cp giis.input  '+OUTPUTDIR+'/IISSET/'+ ARC+'.'+GRAVITY+'')
#         # cp output.scan $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}
#         # cp output.scan $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}
#         # cp emat  $OUTPUTDIR/EMAT/${ARC}.${GRAVITY}
#         os.system('cp sumry '+OUTPUTDIR+'/sumry/'+ ARC+'.'+GRAVITY +'')
#         os.system('cp Resid '+OUTPUTDIR+'/RESIDS/'+ ARC+'.'+GRAVITY+'')
#         os.system('cp punch '+OUTPUTDIR+'/PUNCH/'+ ARC+'.'+GRAVITY+'')
#         os.system('cp punch.gdn   '+OUTPUTDIR+'/PUNCH/'+ ARC+'.'+GRAVITY+'.gdn')
#         os.system('mv IIEOUT.'+ ARC+'.'+GRAVITY+' '+OUTPUTDIR+'/IIEOUT/'+ ARC+'.'+GRAVITY+'')

#         os.system('cp orbfil '+OUTPUTDIR+'/orbits/'+ARC+'.'+GRAVITY+'_orb1')
#         os.system('cp orbfil2 '+OUTPUTDIR+'/orbits/'+ARC+'.'+GRAVITY+'_orb2')

#         os.system('mv telem    '+OUTPUTDIR+'/TELEM/'+ARC+'.'+GRAVITY+'')
#         os.system('cp densityfil  '+OUTPUTDIR+'/DENSITY/'+ARC+'.'+GRAVITY+'')
#         os.system('cp msis_in_file '+OUTPUTDIR+'/DENSITY/'+ARC+'.'+GRAVITY+'_msisin')
#         os.system('cp msis_out_file '+OUTPUTDIR+'/DENSITY/'+ARC+'.'+GRAVITY+'_msisout')
#         os.system('cp msis_SWI_file '+OUTPUTDIR+'/DENSITY/'+ARC+'.'+GRAVITY+'_msisSWI')
#         os.system('cp ascii_xyz '+OUTPUTDIR+'/XYZ_TRAJ/'+ARC+'.'+GRAVITY+'')
#         os.system('cp ascii_xyz '+OUTPUTDIR+'/KEP_TRAJ/'+ARC+'.'+GRAVITY+'')

#         print(self.run_ID,"               Finished copying files to outputdir")

#         #### Go up a level and delete the temporary directory:
#         os.chdir('../')
#         os.system('rm -rf '+TMPDIR_arc)
        
        