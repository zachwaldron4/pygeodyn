#### ===============
#### Import modules:
#### ===============
import pandas as pd
import numpy as np
#### Computer/Command Line functions
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

#=======================================
#------------- Spire Class -------------
#=======================================

import logging

class Satellite_Spire(PygeodynController,  PygeodynReader):
    """ Satellite_Spire class documentation
    
    Description: 
    ------------
       Class with satellite specific configuration for running 
       Pygeodyn with Spire cubesats.
       

    Long Description:
    -----------------
       This class hosts all major modifications to the methods 
       in the PygeodynController and PygeodynReader that allow 
       the running of Spire satellites through Pygeodyn.  
       


    Parameters
    ----------
        Inherit PygeodynController : Class
            Used to control and run the GEODYN with Python
        Inherit PygeodynReader : Class
            Used to read and reformat the GEODYN output with Python    
    
    Returns
    -------
        Object
            Returns an object with methods that have been re-written 
            to accomodate the Spire satellites.
            
    """
    
    def __init__(self):
        pass
            
        ###### ---------------------------------------------------------------------
        #### HARD CODE the Spire properties
        ###### ---------------------------------------------------------------------
#         self.SATELLITE_dir = 'Spire'
#         self.SATID         = '1804607'  # 83
# #         self.YR            =  2018
#         self.DATA_TYPE     = 'orbit_propagation'

        #### Spire Data files
#         self.g2b_file = 'g2b_pce_fullset_nomaneuver'  
#         self.atgrav_file = 'ATGRAV.glo-3HR_20160101-PRESENT_9999_AOD1B_0006.0090'
#         self.ephem_file     = 'ephem1430.data_2025'
#         self.gravfield_file = 'eigen-6c.gfc_20080101_do_200_fix.grv'
        

    
    
    
#         self.path_to_binaryrvgs     = '/data/data_geodyn/inputs/icesat2/pre_processing/traj_files_rvg'
#         self.StateVector_epochs_datafile = '/data/data_geodyn/inputs/icesat2/setups/PCE_ascii.txt'
        

        
        
        
        
        
    def set_file_paths_for_multiple_arcs(self, arc_val, iarc, unzip_and_loadpaths=False):
        '''
        Handles the Arc naming conventions
        Construct a way to read in the satellite specific filenames.
        '''
        
        self.run_ID = 'Run # '+ str(iarc+1)
        
#         seen = set()
#         dupes = [x for x in self.arc_input if x in seen or seen.add(x)]         
        
        #### Count how many arcs of this name there are
        for x_arc in self.arc_input:
            if self.arc_input.count(x_arc) == 1:
#                 print('Only one arc of this name', x_arc)
                iarc = 0
#             elif self.arc_input.count(arc_val) > 1:
#                 self.unique_arc_count+=1
#                 iarc = self.unique_arc_count
#                 print('There are multiples of this arc, this is #',iarc)
            else:
#                 print('There are multiples of this arc, this is #',iarc+1)
                pass
                
        self.arc_name_id = arc_val
        self.YR  = self.arc_name_id[0:4]
        doy = self.arc_name_id[5:]
#         self.arcdate_for_files = '%02d.%d%d'   % ( str(iarc+1), self.YR, doy)
#         self.arcdate_v2        = '%02d.%d.%d'   % ( str(iarc+1), self.YR, doy) #str(iarc+1)+'.'+ self.YR + '.' + doy 
        self.arcdate_for_files = '%d%03d.%02d'   % ( int(self.YR), int(doy), (iarc+1))
        self.arcdate_v2        = '%d.%03d.%02d'   % ( int(self.YR), int(doy), (iarc+1)) #str(iarc+1)+'.'+ self.YR + '.' + doy 


        ####
        #### The setup files and the external attitutde files have the same naming convention.
#         print('self.arc_name_id',self.arc_name_id)
        self.setup_file_arc    = 'iisset.'+self.arc_name_id
        self.external_attitude = 'EXAT01.'+self.arc_name_id+'.gz'
        ####
        ### Now specify what we what the output arcs to be named.
        self.ARC = (self.SATELLITE_dir    + '_' + 
                    self.arcdate_for_files+ '_' + 
                    self.arc_length + '.' +  
                    self.DEN_DIR + '.' +
                    self.run_settings['file_string'])

        
#         self.SERIES = self.DEN_DIR + '_' + self.ACCELS + self.directory_name_specifier
        self.SERIES = self.DEN_DIR + '_' + self.cd_model + self.directory_name_specifier

        self.path_to_model = self.run_settings['path_to_output_directory'] + '/'+self.DEN_DIR+'/'+self.SERIES +'/'
                            #('/data/data_geodyn/results/'+
                              #     self.SATELLITE_dir +'/'+
                               #    self.den_model+'/'+  
                                #   self.den_model+'_'+ self.ACCELS + self.directory_name_specifier +'/')
        file_name =   self.ARC         
       
        
        ####  save the specific file names as "private members" with the _filename convention
        self._asciixyz_filename = self.path_to_model + 'XYZ_TRAJ/'+ file_name
        self._orbfil_filename   = self.path_to_model + 'ORBITS/'  + file_name+'_orb1'
        self._iieout_filename   = self.path_to_model + 'IIEOUT/'  + file_name
        self._density_filename  = self.path_to_model + 'DENSITY/' + file_name     
        self._drag_filename     = self.path_to_model + 'DENSITY/' + file_name +'drag_file'    
        self._accel_filename    = self.path_to_model + 'ORBITS/'  + file_name +'_accel_file'    
#         self._EXTATTITUDE_filename = self.EXATDIR +'/' +self.external_attitude

        
        time.sleep(1)
        
    
    
    def make_list_of_arcfilenames(self):
        '''
        Handles the Arc naming conventions for the icesat2 satellite
        Construct a way to read in the satellite specific filenames.
        '''
        
        arc_file_list = []
        
#         print('make_list_of_arcfilenames-- self.arc_input: ',self.arc_input)
            
        for i, val in enumerate(self.arc_input):

            #### Count how many arcs of this name there are
            if self.arc_input.count(val) == 1:
#                 print('Only one arc of this name', x_arc)
                i = 0
#             elif self.arc_input.count(arc_val) > 1:
#                 self.unique_arc_count+=1
#                 i = self.unique_arc_count
#                 print('There are multiples of this arc, this is #',iarc)
            else:
#                 print('filename list #',i+1)
                pass

            arc_name_id = val
            YR  = arc_name_id[0:4]
            doy = arc_name_id[5:]
            arcdate_for_files =  '%d%03d.%02d' % ( int(YR), int(doy), (i+1)) # str(i+1)+'.' +YR + doy 
            ####
            ####
            ### Now specify what we what the output arcs to be named.
#             ARC_file = (self.SATELLITE_dir    + '_' + 
#                         arcdate_for_files+ '_' + 
#                         self.arc_length + '.' +  
#                         self.DEN_DIR)
            ARC_file = (self.SATELLITE_dir    + '_' + 
                        arcdate_for_files+ '_' + 
                        self.arc_length + '.' +  
                        self.DEN_DIR + '.' +
                        self.run_settings['file_string'])


            arc_file_list.append(ARC_file)
        
#         print('make_list_of_arcfilenames-- arc_file_list: ',arc_file_list)

        return(arc_file_list)
    
    
    
        
            
            
            
    def prepare_tmpdir_for_geodyn_run(self):
        '''  This it the Spire version of this method.
             
             it is being overridden to INCLUDE the external attitude
        '''
        
        logger = logging.getLogger(self.execlog_filename)
        logger.info(f"Spire - Construct a tmp directory in which to run IIS and IIE")

        
#         self.verboseprint('Spire -- prepare_tmpdir_for_geodyn_run()')
#         print(self.TMPDIR_arc)
#         self.verboseprint(self.tabtab,'Current DIR: ',os.getcwd())
     
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
        if not os.path.exists(self.TMPDIR_arc +'/EXAT01'+'.gz'):
#             if np.size(self.external_attitude) >= 1:
# #                 print('dict of EXATfilename: ',np.size(self.external_attitude))
#                 shutil.copyfile(self._EXTATTITUDE_filename[1], self.TMPDIR_arc +'/EXAT01'+'.gz')
#                 shutil.copyfile(self._EXTATTITUDE_filename[2], self.TMPDIR_arc +'/EXAT02'+'.gz')
#                 shutil.copyfile(self._EXTATTITUDE_filename[3], self.TMPDIR_arc +'/EXAT03'+'.gz')
#                 shutil.copyfile(self._EXTATTITUDE_filename[4], self.TMPDIR_arc +'/EXAT04'+'.gz')
#                 shutil.copyfile(self._EXTATTITUDE_filename[5], self.TMPDIR_arc +'/EXAT05'+'.gz')
#                 print('Copied 5 EXAT files')
#             else:
            shutil.copyfile(self._EXTATTITUDE_filename, self.TMPDIR_arc +'/EXAT01'+'.gz')
            self.verboseprint(self.tabtab,'copied:   exat file  > EXAT01'+'.gz')
            self.verboseprint(self.tabtab,'copied:   '+self._EXTATTITUDE_filename+' > EXAT01'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy is set up: EXAT01 file')

        
        #### make symlink to the G2B file and save as ftn40
        if not os.path.exists(self.TMPDIR_arc +'/ftn40'+''):
            os.symlink(self._G2B_filename, self.TMPDIR_arc +'/ftn40'+'')
#             shutil.copyfile(self._G2B_filename, self.TMPDIR_arc +'/ftn40'+'')
            self.verboseprint(self.tabtab,'copied:   g2b file   > ftn40'+'')
        else:
            self.verboseprint(self.tabtab,'copy:  g2b file')

        #### make symlink to the gravity field and save as ftn12
        if not os.path.exists(self.TMPDIR_arc +'/ftn12'+''):
            shutil.copyfile(self._grav_field_filename, self.TMPDIR_arc +'/ftn12'+'')
#             self.verboseprint(self.tabtab,'gravfield:',self._grav_field_filename)
            self.verboseprint(self.tabtab,'copied:   grav field > ftn12'+'')
        else:
            self.verboseprint(self.tabtab,'copy is set up: grav_field file')

        #### make symlink to the ephemerides and save as ftn01
        if not os.path.exists(self.TMPDIR_arc +'/ftn01'+''):
#             os.symlink(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            shutil.copyfile(self._ephem_filename, self.TMPDIR_arc +'/ftn01'+'')
            self.verboseprint(self.tabtab,'copied:   ephem file > ftn01'+'')
        else:
            self.verboseprint(self.tabtab,'copy is set up: ephem file'+'')

        #### make symlink to the gdntable and save as ftn02
        if not os.path.exists(self.TMPDIR_arc +'/ftn02'):
            shutil.copyfile(self._gdntable_filename, self.TMPDIR_arc +'/ftn02')
            self.verboseprint(self.tabtab,'copied:   gdntable   > ftn02')
        else:
            self.verboseprint(self.tabtab,'copy is set up: gdntable file')


        #### make symlink to the ATGRAVFIL and save as fort.18
        if not os.path.exists(self.TMPDIR_arc +'/fort.18'+''):
#             os.symlink(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
            shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18'+'')
#             shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/ftn18')
#             self.verboseprint(self.tabtab,'ATGRAV:',self._ATGRAV_filename)
            self.verboseprint(self.tabtab,'copied:  atgrav     > fort.18'+'')
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


        #### GUNZIP the files:  gzip is a very fast compression option.
#         self.verboseprint(self.tabtab, "gunzipping the input data files")
        self.verboseprint(self.tabtab, "gunzipping the input data files")

        os.system('gunzip -vr *.gz')
#         os.system('gunzip -ftn01.gz')


