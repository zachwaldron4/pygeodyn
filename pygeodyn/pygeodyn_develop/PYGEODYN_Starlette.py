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

sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')
from EditSetupFile          import *




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
        self.gravfield_file = 'grvfld.goco05s.gz'
        self.g2b_file       = 'starlette_03_slrg2b.rm0.gz'
        self.atgrav_file    = 'ATGRAV.glo-3HR_20030101-20080204_1860_AOD1B_0006.0090.gz'
        self.ephem_file     = 'ephem1421.data.gz'
        
        self.external_attitude = False
        
        
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
        
    
    
        
        self.setup_file_arc    = self.SATELLITE_dir+self.arc_name_id+'_'+self.arc_length
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

        
            
           
        
        
        
    def clean_iisset_file(self):
        '''
        Overwrite the setup file with the starlette-SLR specific run parameters.
       
        This function does the following:
            - copies setup file to a temporoary file
            - Adds the GLOBAL TITLE Cards (3 strings at top)
            - Removes all instances of the GPS satellites
            - Deletes specified cards in the cards_to_remove list
            - Modifies the cards in card_strings dict
            - Includes the time dependent DRAG options in the card_drag_strings dict
            - Adds cards that are wanted that are not in the file.

        ****************************************************
        REFER TO GEODYN DOCS VOLUME 3 FOR CARD OPTIONS!!!!!
        ****************************************************
        
        
        '''
        
        self.verboseprint('Starlette -- clean_iisset_file()')

        
        
        
        #### --------------------------------------------------------------------
        #### Initialize our variables from user input
        (path_to_setupfiles, 
         setup_file_arc, 
         SAT_ID, 
         den_model_setupval) = ( self.INPUTDIR,  self.setup_file_arc, self.SATID, self.iisset_den)
        
        ORIG_iisset_file = self._INPUT_filename 
        iisset_file      = 'cleaned_setup'+'_'  + self.arcdate_for_files
        
        self.verboseprint('path_to_setupfiles ', path_to_setupfiles)
        self.verboseprint('setup_file_arc     ', setup_file_arc)
        self.verboseprint('SAT_ID             ', SAT_ID)
        self.verboseprint('den_model_setupval ', den_model_setupval)
        
        

        #### --------------------------------------------------------------------
        ##### COPY THE FILE SO THAT YOU DON'T OVERWRITE THE ORIGINAL
        ####    We copy to a temporary file "cleaned_setup_file"            
        shutil.copyfile(ORIG_iisset_file, self.TMPDIR_arc +'/'+iisset_file+'.bz2')
        os.chdir(self.TMPDIR_arc)
        os.system('bunzip2 -v '+ '*.bz2')
        os.chdir('/data/geodyn_proj/pygeodyn')
        iisset_file = self.TMPDIR_arc+'/' +'cleaned_setup'+'_'  + self.arcdate_for_files

        
        
        #### --------------------------------------------------------------------
        #### identify the cards we do not want in the setup file
        cards_to_remove = [ 'ACCEL9',
                            'XEPHEM',
                            'REFRAC',
                            'GPSMOD',
                            'OFFSET',
                            'OFFADJ',
                            'ANTPHC',
                            'ANTPH2',
                            'CGMASS',
                            'DRAG   0 0',
                            'MBIAS',
                            'ORBTVU',
                            'RESID',
                          ] 
        

        
        #### --------------------------------------------------------------------
        ##### Grab the EPOCH start and end times
        
        ### The SAT_ID is broken somehow in some places in the
        ### starlette file, so I added this decimated option:
        decimated_SAT_ID = '75010 1'
        decimated_flag   = True

        (epoch_start,
         epoch_start_YYMMDD,
         epoch_start_HHMM,
         epoch_end,
         epoch_end_YYMMDD,
         epoch_end_HHMM) = EditSetupFile__get_epoch_times(iisset_file, SAT_ID, decimated_SAT_ID, decimated_flag)
        
        ### - Make datetime versions of epoch times for easier time addition
        epoch_start_dt = pd.to_datetime( epoch_start_YYMMDD+epoch_start_HHMM, format='%y%m%d%H%M%S')
        epoch_end_dt = pd.to_datetime( epoch_end_YYMMDD+epoch_end_HHMM, format='%y%m%d%H%M%S')
        
        

        #### --------------------------------------------------------------------
        #### MODIFY CARD INPUTS                           
        
        string_epoch_start = epoch_start_YYMMDD+epoch_start_HHMM+'00.0000000'
        string_epoch_end   = epoch_end_YYMMDD  +epoch_end_HHMM  +'00.0000000'
                                                #          YYMMDDHHMM00.0000000
        card_strings = {}                         #       -56789012345678901234YYMMDDHHMMSS.SS 
        #                          12345678901234567890123456789012345678901234567890123456789012345678901234567890 
         #                         ORBFIL00131      7501001     03091400000.0  030928 24200.00          60           46707
        card_strings['ORBFIL'] =  'ORBFIL00131      '+SAT_ID+string_epoch_start+string_epoch_end[:-5]+'60'
        card_strings['RESID']  =  'RESIDU12'
        card_strings['OBSVU']  =  'OBSVU 2'  # print residuals on First and last iterations only
        card_strings['PRNTVU'] =  'PRNTVU5521111211 121122'  # suppress some IIS/IIE outputs.
        card_strings['ORBTVU'] =  'ORBTVU1201       '+SAT_ID+string_epoch_start+string_epoch_end[:-5]+'.100000D+01'
        card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
        
        
        ##### THESE WILL LIKELY NEED MODIFICATION
        card_strings['REFSYS']  = 'REFSYS19410         '+epoch_start+' '
        #                          12345678901234567890123456789012345678901 
        #                          12345678901234567890123456789012345678901234567890123456789012345678901234567890 
                              #    EPOCH               030914000000.0000000030914000000.0000000030928000000.0000000  
        card_strings['EPOCH']   = 'EPOCH               '+string_epoch_start+string_epoch_start+string_epoch_end
        #                          123456789012345678901234 
        card_strings['SATPAR']  = 'SATPAR00000000000'+SAT_ID+' 4.5240000000000D-02 4.72500000D+01 1.000000D-02 0.0D+00'
        #                          123456789012345678901234 
        card_strings['FLUX  1'] = 'FLUX  0'    
    
    
    
        #### --------------------------------------------------------------------
        #### INPUT THE DRAG OPTIONS FOR TIME DEPENDENT DRAG
                # ------- Starlette drag already 
                # ------- in correct format.
                # ------- Skipping.
            
        
        
        #### --------------------------------------------------------------------
        #### CHECK FOR MISSING CARDS: Search through the file to see if any of 
        ####                           the cards we WANT are NOT in the file
        card_flag = EditSetupFile__identify_missing_cards(iisset_file, card_strings)

        
        
        #### --------------------------------------------------------------------
        #### ADD CARD MODIFICATIONS:     Edit the cards in the file using the 
        ####                             modified `card_strings` dict 
        EditSetupFile__rewrite_file_using_modified_cards(iisset_file, card_strings)

        
        
        #### --------------------------------------------------------------------
        #### ADD TIMEDEP DRAG:      Add time dependent drag estimations. 
                # ------- Starlette drag already 
                # ------- in correct format.
                # ------- Skipping.
        # EditSetupFile__timedep_drag(iisset_file, SAT_ID, epoch_start_dt, epoch_end_dt)
        
        
        
        ####----------------------------------------------------------------------
        #### REMOVE CARDS:  rewrite the file without the cards 
        ####                we specified in the cards_to_remove dict
        EditSetupFile__rewrite_file_and_remove_unwantedcards(iisset_file, cards_to_remove)
        
        

        ####----------------------------------------------------------------------
        #### ADD CARDS: Add any cards that we want that 
        ####            are not in the file
        EditSetupFile__rewrite_file_and_add_missing_cards(iisset_file, card_flag)
        
        
        
        ####----------------------------------------------------------------------
        #### ADD GLOBAL TITLE CARDS: Add three lines to the 
        ####                         start of the file.  
        ####                         This is the GLOBAL TITLE
                # ------- Starlette already have already 
                # ------- Global Title Cards
                # ------- Skipping.
                #         with open(iisset_file, 'r+') as f:
                #             content = f.read()
                #             f.seek(0, 0)   # find the first lines
                #             f.write('### \n') 
                #             f.write('###   '+self.arc_name_id+'  \n') 
                #             f.write('### \n') 
                #             f.write(content) 
            
            
            
        ####----------------------------------------------------------------------
        #### REMOVE GPS:   Remove all GPS satellites.
                # ------- NO GPS in this run
                # ------- Skipping.
                # EditSetupFile__rewrite_file_delete_GPS_sats(iisset_file)

            
            
        
        
        
        
        
        
        


        
        
        
        
        
        
        