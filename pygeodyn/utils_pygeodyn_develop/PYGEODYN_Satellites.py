#### ===============
#### Import modules:
#### ===============

#### Computer/CL functions
import os
import os.path
import sys
import subprocess
import shutil
import linecache


#### ----------------------------------------
#### ============================================


# Import class to be inherited
import sys  
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/utils_pygeodyn_develop/')
from PYGEODYN_Control import PygeodynController
from PYGEODYN_Read    import PygeodynReader


#=======================================
#-------------ICESAT2 CLASS-------------
#=======================================


# class Satellite_ICESat2(PygeodynController, PygeodynReader ):
class Satellite_ICESat2(PygeodynController,  PygeodynReader):


    '''
    The ICESat2 class inherits the PygeodynController class and its methods.
    
     We make necessary modifications to certain methods of the above classes here by over-writing them
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
        self.g2b_file = 'icesat2_g2b_PCE_gpstraj.gz'
        self.atgrav_file = 'ATGRAV.glo-3HR_20160101-PRESENT_9999_AOD1B_0006.0090.gz'
        self.ephem_file = 'ephem1430.data_2025.gz'
        self.gravfield_file = 'eigen-6c.gfc_20080101_do_200_fix.grv.gz'
        self.external_attitude = 'EXAT01.2018.287.gz'
        
        ### Modifications that are ICESat2 Specific:
        self.ARC =   self.arc_input
        
#     def clean_iisset_file(self):
#         '''
#         Overwrite the file cleaner with the ICESAT specific run parameters.
        
#         If one wants to make more major changes to this file, IE implemement a NON-PCE based run, then one may want to construct a new class to inherit this one, and overwrite this method in that class. 
#         '''
       
#         (path_to_setupfiles, setup_file_arc, SAT_ID, den_model_setupval) = ( self.INPUTDIR,  self.ARC, self.SATID, self.iisset_den)
        
        
        
#         ORIG_iisset_file = path_to_setupfiles + '/' + setup_file_arc

#         shutil.copyfile(ORIG_iisset_file, path_to_setupfiles +'/'+'cleaned_setup_file')
#         iisset_file =       path_to_setupfiles +'/'+'cleaned_setup_file'


#         cards_to_remove = [ 'ACCEL9',
#                             'XEPHEM',
#                             'REFRAC',
#                             'GPSMOD',
#                             'OFFSET',
#                             'OFFADJ',
#                             'ANTPHC',
#                             'ANTPH2',
#                             'CGMASS',
#                           ] 

#         ##### Grab the EPOCH start and end times
#         EPOCH_lines = []
#         with open(iisset_file, 'r') as f:
#             for line_no, line_text in enumerate(f):
#                 if 'EPOCH         ' in line_text:
#                     EPOCH_lines.append(line_no) 

#         ##### Identify and save the EPOCH start and end times
#         for i,val in enumerate(EPOCH_lines):
#             satpar_line = linecache.getline(iisset_file,val) # Check the above SATPAR line get the correct satellite ID (i.e. NOT GPS)

#             ##### only do this for the main satellite, so look for the correct SATID in the SATPAR card above EPOCH
#             if SAT_ID in satpar_line:
#                 epoch_start = linecache.getline(iisset_file,val + 1)[20:40].strip() #181013210000.0000000
#                 epoch_start_YYMMDD = linecache.getline(iisset_file,val + 1)[20:26].strip()       # 181013
#                 epoch_start_HHMM = linecache.getline(iisset_file,val + 1)[26:30].strip()         # 210000.0000000
#                 epoch_start_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[30:40].strip()   # 00.0000000     

#                 epoch_end   = linecache.getline(iisset_file,val + 1)[60:80].strip() #1810160300 00.000
#                 epoch_end_YYMMDD = linecache.getline(iisset_file,val + 1)[60:66].strip()       # 181016
#                 epoch_end_HHMM = linecache.getline(iisset_file,val + 1)[66:70].strip()         # 210000.0000000
#                 epoch_end_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[70:80].strip()   # 00.0000000     

#         ##### TO DO : Add conditional for month-day turnover
#         epoch_start_minus2days = epoch_start_YYMMDD[:-2]+str(int(epoch_start_YYMMDD[-2:])-2)+epoch_start_HHMM+epoch_start_SS_SSSSSSS
#         epoch_end_plus1days =  epoch_end_YYMMDD[:-2]+str(int(epoch_end_YYMMDD[-2:])+1)+epoch_end_HHMM+epoch_end_SS_SSSSSSS


#         ##### Putting in the options is one of the hardest parts of using GEODYN
#         #####    They require VERY specific inputs depending on the run type.  
#         card_strings = {}
#         card_strings['ORBFIL'] =  'ORBFIL2 31       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
#         card_strings['ORBTVU'] =  'ORBTVU1021       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
#         card_strings['RESID']  =  'RESIDU12'
#         card_strings['OBSVU']  =  'OBSVU 4'
#         card_strings['PRNTVU'] =  'PRNTVU55212222    22122'  
#         card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
#         card_strings['ATGRAV']  =  'ATGRAV9090              '+epoch_start_minus2days +''+epoch_end_plus1days[:-1]   
#         card_strings['I64G2E']  =  'I64G2E         25'  # using 30 like in st-SLR run maxed out the memory usage
#         card_strings['SATPAR']  =  'SATPAR   13      '+SAT_ID+'          9.53000000       1514.000'
#         card_strings['SIGMA           1']  =  '               1.0                 1.0'    #10.0D+25            10.0D+25'
#         card_strings['SIGMA           2']  =  '               1.0                 1.0'    #10.0D+25            10.0D+25'
#         card_strings['SIGMA           3']  =  '               1.0                 1.0'    #10.0D+25            10.0D+25'
#         card_strings['SIGMA          51']  =  '               10.0D+25             0.10'    #10.0D+25             0.10'
#         card_strings['SIGMA          85']  =  '               0.010000            0.010000'    #0.010000            0.010000'


#         ##### read in all lines of the file and save them
#         with open(iisset_file, "r") as f:
#             lines_all = f.readlines()    
#         ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
#         with open(iisset_file, "w") as f:
#             for line in lines_all:
#                 if any(card in line for card in cards_to_remove):
#                     # IF the any of the cards in the list are in the line, dont add it
#                     pass
#                 else:
#                     f.write(line)                

#         ##### DO IT AGAIN but with the above updates
#         ##### read in all lines of the file and save them
#         with open(iisset_file, "r") as f:
#             lines_all = f.readlines()                

#         ####------------------------------------------------
#         #### Check to see if cards we want are in file first
#         ####------------------------------------------------
#         ##### card flags to see if certain cards are present in the file
#         card_flag = {}
#         for card in card_strings:
#             ### Set the default flag to be False,  if the card is in the file, flip the flag to True
#             card_flag[card] = False
#             for line in lines_all:
#                 if card in line:
#                     card_flag[card] = True


#         ## Re-write the file line-by-line and EDIT the cards that need to be modified    
#         with open(iisset_file, "w") as f:
#             for line in lines_all:

#                 for card in card_strings:

#                     if card in line:
#                         line_replace = card_strings[card]
#                         # Go ahead and re-write the line to be in the desired format
#                         f.write(line_replace+' \n')
#                         print(line_replace)
#                         break
#                     else:
#                         f.write(line)
#                         break

#         ##### DO IT AGAIN but with the above updates
#         ###    read in all lines of the file and save them
#         with open(iisset_file, "r") as f:
#             lines_all = f.readlines()                  
#         ####----------------------------------------------------
#         #### Add any cards that we want that are not in the file
#         ####----------------------------------------------------
#         for card in card_flag:
#             if card_flag[card] == False:

#                 with open(iisset_file, "w") as f:
#                     for line in lines_all:
#                         if 'ALBEDO' in line:
#                             f.write(line)
#                             f.write(card_strings[card] + ' \n')                 
#                         else:
#                             f.write(line)
    def clean_iisset_file(self):
        '''
        Overwrite the file cleaner with the ICESAT specific run parameters.

        If one wants to make more major changes to this file, IE implemement a NON-PCE based run, then one may want to construct a new class to inherit this one, and overwrite this method in that class. 
        '''

        (path_to_setupfiles, setup_file_arc, SAT_ID, den_model_setupval) = ( self.INPUTDIR,  self.ARC, self.SATID, self.iisset_den)



        ORIG_iisset_file = path_to_setupfiles + '/' + setup_file_arc

        shutil.copyfile(ORIG_iisset_file, path_to_setupfiles +'/'+'cleaned_setup_file')
        iisset_file =       path_to_setupfiles +'/'+'cleaned_setup_file'


        cards_to_remove = [ 'ACCEL9',
                            'XEPHEM',
                            'REFRAC',
                            'GPSMOD',
                            'OFFSET',
                            'OFFADJ',
                            'ANTPHC',
                            'ANTPH2',
                            'CGMASS',
                          ] 

        ##### Grab the EPOCH start and end times
        EPOCH_lines = []
        with open(iisset_file, 'r') as f:
            for line_no, line_text in enumerate(f):
                if 'EPOCH         ' in line_text:
                    EPOCH_lines.append(line_no) 

        ##### Identify and save the EPOCH start and end times
        for i,val in enumerate(EPOCH_lines):
            satpar_line = linecache.getline(iisset_file,val) # Check the above SATPAR line get the correct satellite ID (i.e. NOT GPS)

            ##### only do this for the main satellite, so look for the correct SATID in the SATPAR card above EPOCH
            if SAT_ID in satpar_line:
                epoch_start = linecache.getline(iisset_file,val + 1)[20:40].strip() #181013210000.0000000
                epoch_start_YYMMDD = linecache.getline(iisset_file,val + 1)[20:26].strip()       # 181013
                epoch_start_HHMM = linecache.getline(iisset_file,val + 1)[26:30].strip()         # 210000.0000000
                epoch_start_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[30:40].strip()   # 00.0000000     

                epoch_end   = linecache.getline(iisset_file,val + 1)[60:80].strip() #1810160300 00.000
                epoch_end_YYMMDD = linecache.getline(iisset_file,val + 1)[60:66].strip()       # 181016
                epoch_end_HHMM = linecache.getline(iisset_file,val + 1)[66:70].strip()         # 210000.0000000
                epoch_end_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[70:80].strip()   # 00.0000000     

        ##### TO DO : Add conditional for month-day turnover
        epoch_start_minus2days = epoch_start_YYMMDD[:-2]+str(int(epoch_start_YYMMDD[-2:])-2)+epoch_start_HHMM+epoch_start_SS_SSSSSSS
        epoch_end_plus1days =  epoch_end_YYMMDD[:-2]+str(int(epoch_end_YYMMDD[-2:])+1)+epoch_end_HHMM+epoch_end_SS_SSSSSSS


        ##### Putting in the options is one of the hardest parts of using GEODYN
        #####    They require VERY specific inputs depending on the run type.  
        card_strings = {}
        card_strings['ORBFIL'] =  'ORBFIL2 31       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
        card_strings['ORBTVU'] =  'ORBTVU1021       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
        card_strings['RESID']  =  'RESIDU12'
        card_strings['OBSVU']  =  'OBSVU 4'
        card_strings['PRNTVU'] =  'PRNTVU55212222    22122'  
        card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
        card_strings['ATGRAV']  =  'ATGRAV9090              '+epoch_start_minus2days +''+epoch_end_plus1days[:-1]   
        card_strings['I64G2E']  =  'I64G2E         25'  # using 30 like in st-SLR run maxed out the memory usage
        card_strings['SATPAR   13']  =  'SATPAR   13      '+SAT_ID+'          9.53000000       1514.000'
        card_strings['SIGMA           1']  =  'SIGMA           1               1.0                 1.0'    #10.0D+25            10.0D+25'
        card_strings['SIGMA           2']  =  'SIGMA           2               1.0                 1.0'    #10.0D+25            10.0D+25'
        card_strings['SIGMA           3']  =  'SIGMA           3               1.0                 1.0'    #10.0D+25            10.0D+25'
        card_strings['SIGMA          51']  =  'SIGMA          51               10.0D+25             0.10'    #10.0D+25             0.10'
        card_strings['SIGMA          85']  =  'SIGMA          85               0.010000            0.010000'    #0.010000            0.010000'


        ##### read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()    
        ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
        with open(iisset_file, "w") as f:
            for line in lines_all:
                if any(card in line for card in cards_to_remove):
                    # IF the any of the cards in the list are in the line, dont add it
                    pass
                else:
                    f.write(line)                

        ##### DO IT AGAIN but with the above updates
        ##### read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()                

        ####------------------------------------------------
        #### Check to see if cards we want are in file first
        ####------------------------------------------------
        ##### card flags to see if certain cards are present in the file
        card_flag = {}
        for card in card_strings:
            ### Set the default flag to be False,  if the card is in the file, flip the flag to True
            card_flag[card] = False
            for line in lines_all:
                if card in line:
                    card_flag[card] = True

        ########################################################################################
        ###### Re-write the file line-by-line and EDIT the cards that need to be modified    
        lines_replace = {}
        with open(iisset_file, "r") as f:
            lines = f.readlines()
    #         set1 = set(list(card_strings.keys()))

            for line_num, line in enumerate(lines):
    #             set2 = set(line)
                for card in card_strings:
                    if card in line:
                        lines_replace[line_num] = card_strings[card]
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()
        with open(iisset_file, "w") as f:
            for line_num, line in enumerate(lines_all):
                if line_num in lines_replace:
                    f.write(lines_replace[line_num]+'\n')
                else:
                     f.write(line)

    #     print(lines_replace.keys())

        ##### DO IT AGAIN but with the above updates
        ###    read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()                  
        ####----------------------------------------------------
        #### Add any cards that we want that are not in the file
        ####----------------------------------------------------
        for card in card_flag:
            if card_flag[card] == False:

                with open(iisset_file, "w") as f:
                    for line in lines_all:
                        if 'ALBEDO' in line:
                            f.write(line)
                            f.write(card_strings[card] + ' \n')                 
                        else:
                            f.write(line)

    #### overwrite some methods from CONTROL:
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
        if not os.path.exists(self.TMPDIR_arc +'/EXAT01'+'.gz'):
            shutil.copyfile(self._EXTATTITUDE_filename, self.TMPDIR_arc +'/EXAT01'+'.gz')
#                 os.symlink(self._EXTATTITUDE_filename, self.TMPDIR_arc +'/EXAT01')
#                 self.verboseprint(self.tabtab,'EXAT01:',self._EXTATTITUDE_filename)
            self.verboseprint(self.tabtab,'copied:   exat file  > EXAT01'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy is set up: EXAT01 file')

        
        #### make symlink to the G2B file and save as ftn40
        if not os.path.exists(self.TMPDIR_arc +'/ftn40'+'.gz'):
#             os.symlink(self._G2B_filename, self.TMPDIR_arc +'/ftn40')
            shutil.copyfile(self._G2B_filename, self.TMPDIR_arc +'/ftn40'+'.gz')
            self.verboseprint(self.tabtab,'copied:   g2b file   > ftn40'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy:  g2b file')

        #### make symlink to the gravity field and save as ftn12
        if not os.path.exists(self.TMPDIR_arc +'/ftn12'+'.gz'):
            shutil.copyfile(self._grav_field_filename, self.TMPDIR_arc +'/ftn12'+'.gz')
#             self.verboseprint(self.tabtab,'gravfield:',self._grav_field_filename)
            self.verboseprint(self.tabtab,'copied:   grav field > ftn12'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy is set up: grav_field file')

        #### make symlink to the ephemerides and save as ftn01
        if not os.path.exists(self.TMPDIR_arc +'/ftn01'+'.gz'):
#             os.symlink(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            shutil.copyfile(self._ephem_filename, self.TMPDIR_arc +'/ftn01'+'.gz')
            self.verboseprint(self.tabtab,'copied:   ephem file > ftn01'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy is set up: ephem file'+'.gz')

        #### make symlink to the gdntable and save as ftn02
        if not os.path.exists(self.TMPDIR_arc +'/ftn02'):
            shutil.copyfile(self._gdntable_filename, self.TMPDIR_arc +'/ftn02')
            self.verboseprint(self.tabtab,'copied:   gdntable   > ftn02')
        else:
            self.verboseprint(self.tabtab,'copy is set up: gdntable file')


        #### make symlink to the ATGRAVFIL and save as fort.18
        if not os.path.exists(self.TMPDIR_arc +'/fort.18'+'.gz'):
#             os.symlink(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
            shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18'+'.gz')
#             shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/ftn18')
#             self.verboseprint(self.tabtab,'ATGRAV:',self._ATGRAV_filename)
            self.verboseprint(self.tabtab,'copied:  atgrav     > fort.18'+'.gz')
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
        os.system('gunzip -vr *.gz')


        
    def set_file_paths_for_multiple_arcs(self, arc_val):
        '''
        Construct a way to read in the satellite specific filenames.
        
        '''

        self.path_to_model = ('/data/data_geodyn/results/'+
                                   self.SATELLITE_dir +'/'+
                                   self.den_model+'/'+  
                                   self.den_model+'_'+ self.ACCELS + self.SpecialRun_name +'/')
        file_name =   str(arc_val)         
        print('        ')
        print('     File path: ')
        print('     Loading ', self.path_to_model ,'.../',file_name,' ' ,sep = '')

        ####  save the specific file names as "private members" with the _filename convention
        self._asciixyz_filename = self.path_to_model + 'XYZ_TRAJ/'+ file_name
        self._iieout_filename   = self.path_to_model + 'IIEOUT/'  + file_name
        self._density_filename  = self.path_to_model + 'DENSITY/' + file_name     



























#=========================================
#-------------STARLETTE CLASS-------------
#=========================================


class Satellite_Starlette(PygeodynController,PygeodynReader):
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

        
        
        
        
        
        
        
        
        
        
        
        
        


        
        
        
        
        
        
        