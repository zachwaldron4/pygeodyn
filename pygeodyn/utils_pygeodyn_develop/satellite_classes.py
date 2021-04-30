
#### ===============
#### Import modules:
#### ===============

#### Computer/CL functions
import os
import os.path
import sys
import subprocess
import shutil
# import time

#### ----------------------------------------
#### ============================================


# Import class to be inherited
import sys  
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/utils_pygeodyn_develop/')
from pygeodyn_Control import pygeodyn_CONTROL



#=======================================
#-------------ICESAT2 CLASS-------------
#=======================================

class Satellite_ICESat2(pygeodyn_CONTROL):
    '''
    The ICESat2 class inherets the pygeodyn_CONTROL class and its methods.
    This class is used to make modifications to pygeodyn_CONTROL to run the ICESAT2 satellite.
 
    '''
    def __init__(self,params):
        super().__init__(params)

        #### HARD CODE the satellite properties
        self.SATELLITE_dir = 'icesat2'
        self.SATID         = '1807001'
        self.YR            = 2018
        self.DATA_TYPE     = 'PCE'
        self.grav_id = '' 
        #                  self.g2b_file = 'g2b_pce'
        self.g2b_file = 'icesat2_g2b_PCE_gpstraj'
        self.atgrav_file = 'ATGRAV.glo-3HR_20160101-PRESENT_9999_AOD1B_0006.0090'
        self.ephem_file = 'ephem1430.data_2025'
        self.gravfield_file = 'eigen-6c.gfc_20080101_do_200_fix.grv'
        self.external_attitude = 'EXAT01.2018.287'
        
        ### Modifications that are ICESat2 Specific:
        self.ARC =   self.arc_input
        
        
 # overwrite some methods from CONTROL:

    def print_runparameters_to_notebook(self):
        
        self._EXTATTITUDE_filename = self.EXATDIR +'/' +self.external_attitude

        
        print(self.run_ID,"    Density Model:     " ,self.DEN_DIR)
        print(self.run_ID,"    GEODYN Version:    " ,self.GDYN_version)
        print(self.run_ID,"    Estimate GenAccel: " ,self.ACCELS)
        print(self.run_ID,"    ARC run:           " ,self.ARC)
        print(self.run_ID,"    Output directory:  " ,self.OUTPUTDIR)
        print(self.run_ID,"    Call Options:      " ,self.options_in)

        print(self.run_ID,"    EXAT File:    " ,self._EXTATTITUDE_filename)

        if os.path.exists(self._INPUT_filename):
            self.verboseprint(self.tabtab,"FORT.5  (input) file:  ", self._INPUT_filename)
        else:
            print(self.run_ID,"    FORT.5  (input) file:  ", self._INPUT_filename," not found.")    

        if os.path.exists(self._G2B_filename):
            self.verboseprint(self.tabtab,"FORT.40 (g2b)   file:  ", self._G2B_filename)
        else:
            print(self.run_ID,"    FORT.40 (g2b)   file:  ", self._G2B_filename," not found.")    

            
            
            
    def prepare_tmpdir_for_geodyn_run(self):
        '''  This it the ICESAT2 version of this method.
             
             it is being overridden to INCLUDE the external attitude
        '''

        self.verboseprint(self.tabtab,'Current DIR: ',os.getcwd())
     
        #### Navigate TO the TMPDIR
        os.chdir(self.TMPDIR_arc)
        
        ####-------------------------------------------------------------
        ####     Construct Common Setup of a GEODYN RUN
        ####         this is run in the TMPDIR_arc
        ####-------------------------------------------------------------
        self.verboseprint('-------------------------------------------------')
        self.verboseprint('       Linking files with the command line       ')
        self.verboseprint('-------------------------------------------------')
        
        self.verboseprint(self.tabtab,'Current DIR',os.getcwd())

        #### make copy to the External attitude file and save as EXAT01
        if not os.path.exists(self.TMPDIR_arc +'/EXAT01'):
            shutil.copyfile(self._EXTATTITUDE_filename, self.TMPDIR_arc +'/EXAT01')
#                 os.symlink(self._EXTATTITUDE_filename, self.TMPDIR_arc +'/EXAT01')
#                 self.verboseprint(self.tabtab,'EXAT01:',self._EXTATTITUDE_filename)
            self.verboseprint(self.tabtab,'copied:   exat file  > EXAT01')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: EXAT01 file')

        
        #### make symlink to the G2B file and save as ftn40
        if not os.path.exists(self.TMPDIR_arc +'/ftn40'):
#             os.symlink(self._G2B_filename, self.TMPDIR_arc +'/ftn40')
            shutil.copyfile(self._G2B_filename, self.TMPDIR_arc +'/ftn40')
            self.verboseprint(self.tabtab,'copied:   g2b file   > ftn40')
        else:
            self.verboseprint(self.tabtab,'symlink:  g2b file')

        #### make symlink to the gravity field and save as ftn12
        if not os.path.exists(self.TMPDIR_arc +'/ftn12'):
            shutil.copyfile(self._grav_field_filename, self.TMPDIR_arc +'/ftn12')
#             self.verboseprint(self.tabtab,'gravfield:',self._grav_field_filename)
            self.verboseprint(self.tabtab,'copied:   grav field > ftn12')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: grav_field file')

        #### make symlink to the ephemerides and save as ftn01
        if not os.path.exists(self.TMPDIR_arc +'/ftn01'):
#             os.symlink(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            shutil.copyfile(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            self.verboseprint(self.tabtab,'copied:   ephem file > ftn01')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: ephem file')

        #### make symlink to the gdntable and save as ftn02
        if not os.path.exists(self.TMPDIR_arc +'/ftn02'):
            shutil.copyfile(self._gdntable_filename, self.TMPDIR_arc +'/ftn02')
            self.verboseprint(self.tabtab,'copied:   gdntable   > ftn02')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: gdntable file')


        #### make symlink to the ATGRAVFIL and save as fort.18
        if not os.path.exists(self.TMPDIR_arc +'/fort.18'):
#             os.symlink(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
            shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
#             shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/ftn18')
#             self.verboseprint(self.tabtab,'ATGRAV:',self._ATGRAV_filename)
            self.verboseprint(self.tabtab,'copied:   atgrav     > fort.18')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: atgrav file')

            
        if not os.path.exists(self.TMPDIR_arc+'/ftn05'):
            os.system('cp '+self._INPUT_filename+' '+self.TMPDIR_arc+'/ftn05')
            self.verboseprint(self.tabtab,'copying          : iieout file')
        else:
            self.verboseprint(self.tabtab,'copied           : iieout file')

        if not os.path.exists(self.TMPDIR_arc+'/giis.input'):
            os.system('cp  '+self.TMPDIR_arc+'/ftn05 '+self.TMPDIR_arc+'/giis.input')
            self.verboseprint(self.tabtab,'copying          : giis.input file')
        else:
            self.verboseprint(self.tabtab,'copied              : giis.input file')   

        self.verboseprint('-------------------------------------------------------------------------')
        self.verboseprint('-------------------------------------------------------------------------')
































#=========================================
#-------------STARLETTE CLASS-------------
#=========================================


class Satellite_Starlette(pygeodyn_CONTROL):
    '''
    The ICESat2 class inherets the pygeodyn_CONTROL class and its methods.
    
    This class is used to make modifications to pygeodyn_CONTROL to run the ICESAT2 satellite.
 
    '''
    def __init__(self,params):
        super().__init__(params)

        #### HARD CODE the satellite properties
        self.SATELLITE_dir = 'st'
        self.SATID         = '7501001'
        self.YR            = 2003
        self.DATA_TYPE     = 'SLR'
        # Specifiy filenames
        self.gravfield_file = 'grvfld.goco05s'
        self.g2b_file       = 'starlette_03_slrg2b.rm0'
        self.atgrav_file    = 'ATGRAV.glo-3HR_20030101-20080204_1860_AOD1B_0006.0090'
        self.ephem_file     = 'ephem1421.data'
        
        
        #### ------------------------------------------
        ### Modifications that are Starlette specific:
        ##---------------------------------------------
        
        #  Setup files for starlette naming conveniton:       
        #          st030914_2wk
        #  So we must append the "st" (self.SATELLITE_dir) to the front of the arc
        # 
        self.ARC =  self.SATELLITE_dir + self.arc_input

        
        #### ------------------------------------------

 # overwrite seom methods from control 
    def print_runparameters_to_notebook(self):
        
        self._INPUT_filename = self.INPUTDIR +'/'+ self.ARC+'.bz2'

        
        print(self.run_ID,"    Density Model:     " ,self.DEN_DIR)
        print(self.run_ID,"    GEODYN Version:    " ,self.GDYN_version)
        print(self.run_ID,"    Estimate GenAccel: " ,self.ACCELS)
        print(self.run_ID,"    ARC run:           " ,self.ARC)
        print(self.run_ID,"    Output directory:  " ,self.OUTPUTDIR)
        print(self.run_ID,"    Call Options:      " ,self.options_in)

#         print(self.run_ID,"    EXAT File:    " ,self._EXTATTITUDE_filename)

        if os.path.exists(self._INPUT_filename):
            self.verboseprint(self.tabtab,"FORT.5  (input) file:  ", self._INPUT_filename)
        else:
            print(self.run_ID,"    FORT.5  (input) file:  ", self._INPUT_filename," not found.")    

        if os.path.exists(self._G2B_filename):
            self.verboseprint(self.tabtab,"FORT.40 (g2b)   file:  ", self._G2B_filename)
        else:
            print(self.run_ID,"    FORT.40 (g2b)   file:  ", self._G2B_filename," not found.")    

            
            
            
    def prepare_tmpdir_for_geodyn_run(self):
        '''  This it the Starlette version of this method.

            It is being overridden to not include the External Attitude
             
        '''
        
        self.verboseprint(self.tabtab,'Current DIR: ',os.getcwd())
     
        #### Navigate TO the TMPDIR
        os.chdir(self.TMPDIR_arc)
        
        ####-------------------------------------------------------------
        ####     Construct Common Setup of a GEODYN RUN
        ####         this is run in the TMPDIR_arc
        ####-------------------------------------------------------------
        self.verboseprint('-------------------------------------------------')
        self.verboseprint('       Linking files with the command line       ')
        self.verboseprint('-------------------------------------------------')
        
        self.verboseprint(self.tabtab,'Current DIR',os.getcwd())

        #### make symlink to the G2B file and save as ftn40
        if not os.path.exists(self.TMPDIR_arc +'/ftn40'):
            shutil.copyfile(self._G2B_filename, self.TMPDIR_arc +'/ftn40')
            self.verboseprint(self.tabtab,'copied:   g2b file   > ftn40')
        else:
            self.verboseprint(self.tabtab,'symlink:  g2b file')

        #### make symlink to the gravity field and save as ftn12
        if not os.path.exists(self.TMPDIR_arc +'/ftn12'):
            shutil.copyfile(self._grav_field_filename, self.TMPDIR_arc +'/ftn12')
            self.verboseprint(self.tabtab,'copied:   grav field > ftn12')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: grav_field file')

        #### make symlink to the ephemerides and save as ftn01
        if not os.path.exists(self.TMPDIR_arc +'/ftn01'):
#             os.symlink(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            shutil.copyfile(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            self.verboseprint(self.tabtab,'copied:   ephem file > ftn01')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: ephem file')

        #### make symlink to the gdntable and save as ftn02
        if not os.path.exists(self.TMPDIR_arc +'/ftn02'):
            shutil.copyfile(self._gdntable_filename, self.TMPDIR_arc +'/ftn02')
            self.verboseprint(self.tabtab,'copied:   gdntable   > ftn02')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: gdntable file')


        #### make symlink to the ATGRAVFIL and save as fort.18
        if not os.path.exists(self.TMPDIR_arc +'/fort.18'):
#             os.symlink(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
            shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
#             shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/ftn18')
#             self.verboseprint(self.tabtab,'ATGRAV:',self._ATGRAV_filename)
            self.verboseprint(self.tabtab,'copied:   atgrav     > fort.18')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: atgrav file')

        if not os.path.exists(self.TMPDIR_arc+'/ftn05.bz2'):
            os.system('cp '+self._INPUT_filename+' '+self.TMPDIR_arc+'/ftn05.bz2')
            self.verboseprint(self.tabtab,'copying          : iieout file')
        else:
            self.verboseprint(self.tabtab,'copied           : iieout file')

        if not os.path.exists(self.TMPDIR_arc+'/ftn05'):
            os.system('bunzip2 '+self.TMPDIR_arc+'/ftn05.bz2')
            self.verboseprint(self.tabtab,'file not zipped  : iieout file')
        else:
            self.verboseprint(self.tabtab,'file not zipped  : iieout file')
        
        if not os.path.exists(self.TMPDIR_arc+'/giis.input'):
            os.system('cp  '+self.TMPDIR_arc+'/ftn05 '+self.TMPDIR_arc+'/giis.input')
            self.verboseprint(self.tabtab,'copying          : giis.input file')
        else:
            self.verboseprint(self.tabtab,'copied              : giis.input file')   

        self.verboseprint('-------------------------------------------------------------------------')
        self.verboseprint('-------------------------------------------------------------------------')

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        