#### ===============
#### Import modules:
#### ===============
import pandas as pd


#### Computer/CL functions
import os
import os.path
import sys
import subprocess
import shutil
import linecache
import time

#### --------------------------------------------
#### ============================================


# Import class to be inherited
import sys  
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/pygeodyn_develop/')
from PYGEODYN_Control import PygeodynController
from PYGEODYN_Read    import PygeodynReader

# sys.path.insert(0,'/data/geodyn_proj/pygeodyn/utils_pygeodyn_develop/util_dir/')
# from common_functions          import MJDS_to_YYMMDDHHMMSS, Convert_ET_TDT_to_UTC




#=========================================
#-------------STARLETTE CLASS-------------
#=========================================


class Satellite_Starlette(PygeodynController,PygeodynReader):
    '''
    The Starlette class inherets the pygeodyn_CONTROL class and its methods.
    
    This class is used to make modifications to pygeodyn_CONTROL to run the Starlette satellite.
 
    '''
    def __init__(self):
        print('3 starlette ---- check ---- init Satellite_Starlette class')
#         super().__init__()
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
        
        self.grav_id = 'goco05s' 
        self.empirical_accels =  False  
        self.ACCELS = 'acceloff'
        self.options_in =  {'DRHODZ_update':True}  

        ###### ---------------------------------------------------------------------
        ###### SPECIFY ARC NAMING CONVENTION
        ###### ---------------------------------------------------------------------
        self.arc_length = '2wk'
        
       
    
    def set_file_paths_for_multiple_arcs(self, arc_val, iarc, unzip_and_loadpaths=False):
        '''
        Handles the Arc naming conventions
        Construct a way to read in the satellite specific filenames.
        '''
        
        print('4 starlette ---- check ---- init set_file_paths_for_multiple_arcs class')

        self.run_ID = 'Run # '+ str(iarc+1)
        
        self.arc_name_id = arc_val
        
        self.YR  = self.arc_name_id[0:2]
        month = self.arc_name_id[2:4]
        day = self.arc_name_id[4:]

        self.arcdate_for_files = self.YR  + month + day
        ####
        #### The setup files and the external attitutde files have the same naming convention.
#         print('self.arc_name_id',self.arc_name_id)
        
    
    
    
        self.setup_file_arc    = 'iisset.'+self.arc_name_id
#         self.external_attitude = 'EXAT01.'+self.arc_name_id+'.gz'
        ####
        ### Now specify what we what the output arcs to be named.
        self.ARC = (self.SATELLITE_dir    + '' + 
                    self.arcdate_for_files+ '_' + 
                    self.arc_length + '.' + 
                    self.grav_id)

        
        
        self.path_to_model = ('/data/data_geodyn/results/'+
                                   self.SATELLITE_dir +'/'+
                                   self.den_model+'/'+  
                                   self.den_model+'_'+ self.ACCELS + self.SpecialRun_name +'/')
        file_name =   self.ARC         
#         print('        ')
        if unzip_and_loadpaths == True:
            pass
        elif self.action == 'read':
            print('     Loading ... ', file_name,' ' ,sep = '')
        else:
            pass
        
        
        ####  save the specific file names as "private members" with the _filename convention
        self._asciixyz_filename = self.path_to_model + 'XYZ_TRAJ/'+ file_name
        self._orbfil_filename = self.path_to_model + 'ORBITS/'+ file_name+'_orb1'
        self._iieout_filename   = self.path_to_model + 'IIEOUT/'  + file_name
        self._density_filename  = self.path_to_model + 'DENSITY/' + file_name     
#         self._EXTATTITUDE_filename = self.EXATDIR +'/' +self.external_attitude

        
        time.sleep(1)

        
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

        
        
        
        
        
        
        
        
        
        
        
        
        


        
        
        
        
        
        
        