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


#### --------------------------------------------
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
    """ Satellite_ICESat2 class documentation
    
    Description: 
    ------------
       Class with satellite specific confiuguration for running 
       Pygeodyn with ICESat2.
       

    Long Description:
    -----------------
       This class hosts all major modifications to the methods 
       in the PygeodynController and PygeodynReader that allow 
       the running of the ICESat2 satellite through Pygeodyn.  
       The setup here is originally for PCE trajectory analysis.


    Parameters
    ----------
        Inherit PygeodynController : Class
            Used to control and run the GEODYN with Python
        Inherit PygeodynReader : Class
            Used to read and reformat the GEODYN output with Python


    Example
    -------


    Notes
    -----
    
    
    Returns
    -------
        Object
            Returns an object with methods that have been re-written 
            to accomodate the Icesat2 satellite, its data, and its 
            configuration on AWS.
            
    """
    
    def __init__(self,params):
        super().__init__(params)
        

        #### HARD CODE the satellite properties
        self.SATELLITE_dir = 'icesat2'
        self.SATID         = '1807001'
        self.YR            =  2018
        self.DATA_TYPE     = 'PCE'
        self.grav_id = '' 
#         self.g2b_file = 'icesat2g2b_pce_287_289A.gz'   # fort.40
        self.g2b_file = 'icesat2g2b_pce_312_328.gz'   # fort.40
        self.atgrav_file = 'ATGRAV.glo-3HR_20160101-PRESENT_9999_AOD1B_0006.0090.gz'
        self.ephem_file = 'ephem1430.data_2025.gz'
        self.gravfield_file = 'eigen-6c.gfc_20080101_do_200_fix.grv.gz'
        
        
        ###### ---------------------------------------------------------------------
        ###### SPECIFY ARC NAMING CONVENTION
        ###### ---------------------------------------------------------------------
        self.arc_length = '54hr'

        
    def set_file_paths_for_multiple_arcs(self, arc_val):
        '''
        Construct a way to read in the satellite specific filenames.
        
        '''
        
        self.arc_name_id = arc_val
        yr  = self.arc_name_id[0:4]
        doy = self.arc_name_id[5:]
        self.arcdate_for_files = yr + doy
        ####
        #### The setup files and the external attitutde files have the same naming convention.
        self.setup_file_arc    = 'iisset.'+self.arc_name_id
        self.external_attitude = 'EXAT01.'+self.arc_name_id+'.gz'
        ####
        ### Now specify what we what the output arcs to be named.
        self.ARC = (self.SATELLITE_dir    + '_' + 
                    self.arcdate_for_files+ '_' + 
                    self.arc_length + '.' +  
                    self.DEN_DIR)

        
        
        self.path_to_model = ('/data/data_geodyn/results/'+
                                   self.SATELLITE_dir +'/'+
                                   self.den_model+'/'+  
                                   self.den_model+'_'+ self.ACCELS + self.SpecialRun_name +'/')
        file_name =   self.ARC         
        print('        ')
        print('     Loading ... ', file_name,' ' ,sep = '')
#         print('     File path: ')
#         print('     Loading ', self.path_to_model ,'.../',file_name,' ' ,sep = '')

        ####  save the specific file names as "private members" with the _filename convention
        self._asciixyz_filename = self.path_to_model + 'XYZ_TRAJ/'+ file_name
        self._iieout_filename   = self.path_to_model + 'IIEOUT/'  + file_name
        self._density_filename  = self.path_to_model + 'DENSITY/' + file_name     

    
    
    
    def clean_iisset_file(self):
        '''
        Overwrite the setup file with the icesat2 specific run parameters.

        To make major changes to this function (i.e. implemement a NON-PCE based run of ICESat2)
            construct a new class to inherit this one, and overwrite this method in that class. 
        
        This function does the following:
            - copies setup file to a temporoary file
            - Adds the GLOBAL TITLE Cards (3 strings at top)
            - Removes all instances of the GPS satellites
            - Deletes specified cards in the cards_to_remove list
            - Modifies the cards in card_strings dict
            - Includes the time dependent DRAG options in the card_drag_strings dict
            - Adds cards that are wanted that are not in the file.

            * this

        '''
        
        self.verboseprint('ICESat2 -- clean_iisset_file()')

        #### --------------------------------------------------------------------
        #### Initialize our variables from user input
        (path_to_setupfiles, setup_file_arc, SAT_ID, den_model_setupval) = ( self.INPUTDIR,  self.setup_file_arc, self.SATID, self.iisset_den)

        ORIG_iisset_file = path_to_setupfiles + '/' + setup_file_arc
        
        #### --------------------------------------------------------------------
        ##### COPY THE FILE SO THAT YOU DON'T OVERWRITE THE ORIGINAL
        ####    We copy to a temporary file "cleaned_setup_file"
        shutil.copyfile(ORIG_iisset_file, path_to_setupfiles +'/'+'cleaned_setup'+'_'  + self.arcdate_for_files)
        iisset_file =       path_to_setupfiles +'/'+'cleaned_setup'+'_'  + self.arcdate_for_files

        
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
                            'OLOAD',
                            'DRAG             5041144 ',       # remove the drag effects on the GPS satellites  
                            'DRAG             5044284',
                            'DRAG             5051204',
                            'DRAG             5154184',
                            'DRAG             5345214',
                            'DRAG             5347224',
                            'DRAG             5356164',
                            'DRAG             5459194',
                            'DRAG             5460234',
                            'DRAG             5461024',
                            'DRAG             5553175',
                            'DRAG             5652315',
                            'DRAG             5658125',
                            'DRAG             5755155',
                            'DRAG             5757295',
                            'DRAG             5848075',
                            'DRAG             5950055',
                            'DRAG             6062256',
                            'DRAG             6163016',
                            'DRAG             6265246',
                            'DRAG             6366276',
                            'DRAG             6464306',
                            'DRAG             6467066',
                            'DRAG             6468096',
                            'DRAG             6469036',
                            'DRAG             6571266',
                            'DRAG             6572086',
                            'DRAG             6573106',
                            'DRAG             6649045',
                            'DRAG             6670326',
                            'DRAG             9743134',
                            'DRAG             9946114',
                            'DRAG   0 0',
                          ] 
        #### --------------------------------------------------------------------
        ##### Grab the EPOCH start and end times
        EPOCH_lines = []
        with open(iisset_file, 'r') as f:
            for line_no, line_text in enumerate(f):
                if 'EPOCH         ' in line_text:
                    EPOCH_lines.append(line_no) 

        #### --------------------------------------------------------------------
        ##### Identify and save the EPOCH start and end times
        for i,val in enumerate(EPOCH_lines):
            satpar_line = linecache.getline(iisset_file,val) # Check the above SATPAR line get the correct satellite ID (i.e. NOT GPS)

            ##### only do this for the main satellite, so look for the correct SATID in the SATPAR card above EPOCH
            if SAT_ID in satpar_line:
                epoch_start = linecache.getline(iisset_file,val + 1)[20:40].strip() #181013210000.0000000
                epoch_start_YYMMDD = linecache.getline(iisset_file,val + 1)[20:26].strip()       # 181013
                epoch_start_HHMM = linecache.getline(iisset_file,val + 1)[26:30].strip()         # 2100
                epoch_start_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[30:40].strip()   # 00.0000000     

                epoch_end   = linecache.getline(iisset_file,val + 1)[60:80].strip() #1810160300 00.000
                epoch_end_YYMMDD = linecache.getline(iisset_file,val + 1)[60:66].strip()       # 181016
                epoch_end_HHMM = linecache.getline(iisset_file,val + 1)[66:70].strip()         # 210000.0000000
                epoch_end_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[70:80].strip()   # 00.0000000     


        #### --------------------------------------------------------------------
        #### Use pandas datetime and time delta to make adjustments to the dates on the ATGRAV and DRAG cards
        #### --------------------------------------------------------------------
        epoch_start_dt = pd.to_datetime( epoch_start_YYMMDD+epoch_start_HHMM, format='%y%m%d%H%M%S')
        epoch_end_dt = pd.to_datetime( epoch_end_YYMMDD+epoch_end_HHMM, format='%y%m%d%H%M%S')

        add_hours_dt = pd.Series(pd.to_timedelta(9,'h'))

        drag_date_1 = (epoch_start_dt+add_hours_dt).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_2 = (epoch_start_dt+add_hours_dt*2).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_3 = (epoch_start_dt+add_hours_dt*3).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_4 = (epoch_start_dt+add_hours_dt*4).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_5 = (epoch_start_dt+add_hours_dt*5).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_6 = (epoch_start_dt+add_hours_dt*6).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_rm = (epoch_start_dt+add_hours_dt*6).dt.strftime('%y%m%d%H%M%S').values[0]

        dt_2days = pd.Series(pd.to_timedelta(48,'h'))
        dt_1days = pd.Series(pd.to_timedelta(24,'h'))
        
        dt_epoch_start_minus2days = (epoch_start_dt - dt_2days).dt.strftime('%y%m%d%H%M%S.0000000').values[0]
        dt_epoch_end_plus1days    = (epoch_end_dt + dt_1days).dt.strftime('%y%m%d%H%M%S.000').values[0]
        
        ##### --------------------------------------------------------------------
        #### --------------------------------------------------------------------
        ####   INPUT THE OPTIONS ON THE SPECIFIC CARDS YOU WANT TO CHANGE
        ##### Putting in the options is one of the hardest parts of using GEODYN
        #####    They require VERY specific inputs depending on the run type.  
        card_strings = {}
        card_strings['ORBFIL'] =  'ORBFIL2 31       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
        card_strings['RESID']  =  'RESIDU12'
        card_strings['OBSVU']  =  'OBSVU 3'  # print residuals on First and last iterations only
        #       card_strings['PRNTVU'] =  'PRNTVU55212222    22122'  # original
        card_strings['PRNTVU'] =  'PRNTVU5521111211 121122'  # suppress some IIS/IIE outputs.
        card_strings['ORBTVU'] =  'ORBTVU1001       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'

        ##### --------------------------------------------------------------------
            ####   PRNTVU KEY ------ Used to control the IIS and IIE printed content 
            ####                             Be warned that if you include a bunch, the file 
            ####                             will be huge, and the run time will increase 
            ####                             (printing to ascii is slow)
            ####    columns      IIS output option
            ####      9          Simple list of GEODYN -IIS setup. Interpretive
            ####     10          Interpretive list of GEODYN -IIS setup.
            ####     11          Observation block selection report.
            ####     12          Gravity model coefficients. Global  
            ####     13          Global parameter values and sigmas.                             
            ####     14          Arc parameter values and sigmas.
            ####     15          Sea surface topography. Ocean
            ####     16          Ocean Tide Model.  
            ####    columns      IIE output option
            ####     18          Simple list of GEODYN -IIS setup.
            ####     19          Values of estimated E-biases.
            ####     20          E-matrix labels in Summary Page.
            ####     21          Adjusted station baselines  
            ####     22          Correlations for adjusted parameters.                             
            ####     23          Shadow crossing. 
            ####-----------------------------------------------------------------------------------------------
            ##### ORBTVU KEY ------ Controls the printing of ASCII 
            #####                           orbit info to a separate file.
            ####   columns       Orbit output option
            #####    7           Frequency of trajectory output.
            #####                    0 - vwd btwn times in cols 25-59 and at 
            #####                          intvl specfd in columns 60-72.
            #####                  **1 - vwd btwn times in cols 25-59 at 
            #####                          data points only.
            #####                    2 - vwd btwn times in cols 25-59 at data
            #####                          points and at the intvl in columns 60-72.
            #####    8           Coordinate system of output
            #####                  **0 - True of date
            #####                    1 - True of ref date 
            #####                    2 - Mean of year 2000
            #####    9           Trajectory type indicator.
            #####                  **0 - Cartesian ephemeris 
            #####                    1 - Keplerian ephemeris 
            #####                    2 - Both Cartesian and Keplerian ephemerides.
            #####    1           Iterations on which trajectory will be printed.
            #####                    0 - First arc iter of first global iter 
            #####                  **1 - Last arc iter of last global iter 
            #####                    2 - Both first first and last last 
            #####                    3 - All iterations
       
        card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
        card_strings['ATGRAV']  =  'ATGRAV9090              '+dt_epoch_start_minus2days +''+dt_epoch_end_plus1days[:-1]   
        card_strings['I64G2E']  =  'I64G2E         25'  # using 30 like in st-SLR run maxed out the memory usage
        card_strings['SATPAR   13']  =  'SATPAR   13      '+SAT_ID+'          9.53000000       1514.000'
        card_strings['SIGMA           1']  =  'SIGMA           1               1.0                 1.0'    
        card_strings['SIGMA           2']  =  'SIGMA           2               1.0                 1.0'    
        card_strings['SIGMA           3']  =  'SIGMA           3               1.0                 1.0'   
        card_strings['SIGMA          51']  =  'SIGMA          51               10.0D+25             0.10'  
        card_strings['SIGMA          85']  =  'SIGMA          85               0.010000            0.010000'  
        
        ### Fix the coordinate system... PCE Data was in J2000
        card_strings['REFSYS1933 0        ']  = 'REFSYS193310        '+epoch_start+'0'
                    # 123456789012
            
        #### Suppress the printing of the flux model
        card_strings['FLUX  1']  =  'FLUX  0'
    
        #### Suppress the printing of the gravity model
        card_strings['PLANET    0100          2.20320900000000D+13      2439700.0 10000000.000'] = 'PLANET 2  0100          2.20320900000000D+13      2439700.0 10000000.000'
        card_strings['PLANET    0200          3.24858592000000D+14      6051900.0 10000000.000'] = 'PLANET 2  0200          3.24858592000000D+14      6051900.0 10000000.000'
        card_strings['PLANET    0301          4.90280007600000D+12      1738000.0 10000000.000'] = 'PLANET 2  0301          4.90280007600000D+12      1738000.0 10000000.000'
        card_strings['PLANET    0400          4.28283752140000D+13      3397000.0      154.409'] = 'PLANET 2  0400          4.28283752140000D+13      3397000.0      154.409'
        card_strings['PLANET    0500          1.26712764800000D+17     71492000.0       15.414'] = 'PLANET 2  0500          1.26712764800000D+17     71492000.0       15.414'
        card_strings['PLANET    0600          3.79405852000000D+16     60268000.0       10.208'] = 'PLANET 2  0600          3.79405852000000D+16     60268000.0       10.208'
        card_strings['PLANET    0700          5.79454860000000D+15     25559000.0       43.616'] = 'PLANET 2  0700          5.79454860000000D+15     25559000.0       43.616'
        card_strings['PLANET    0800          6.83653500000000D+15     24764000.0       58.543'] = 'PLANET 2  0800          6.83653500000000D+15     24764000.0       58.543'
        card_strings['PLANET    0900          9.77000000000000D+11      1151000.0 10000000.000'] = 'PLANET 2  0900          9.77000000000000D+11      1151000.0 10000000.000'
        card_strings['PLANET    9999          1.32712440040944D+20    696000000.0 10000000.000'] = 'PLANET 2  9999          1.32712440040944D+20    696000000.0 10000000.000'
    
#         if self.fast_run == True:
#             card_strings['PRNTVU'] =  'PRNTVU5511111111 111111'  # over-suppress for fast run?
#             card_strings['OBSVU']  =  'OBSVU 5'  # dont print residuals to IIE
#         else:
#             pass

        #### --------------------------------------------------------------------
        ####   INPUT THE DRAG OPTIONS  for time dependent drag
        card_drag_strings={}
        card_drag_strings['CONDRG']  =  'CONDRG  1        '+SAT_ID+'     '+str(epoch_start[:-5])+str(epoch_end[:-5])+'         0.50000  28800.'
#         card_drag_strings['DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00']  =  'DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00'
        card_drag_strings[drag_date_1]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_1[:10]+' 0.00    0.100D+02'
        card_drag_strings[drag_date_2]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_2[:10]+' 0.00    0.100D+02'
        card_drag_strings[drag_date_3]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_3[:10]+' 0.00    0.100D+02'
        card_drag_strings[drag_date_4]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_4[:10]+' 0.00    0.100D+02'
        card_drag_strings[drag_date_5]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_5[:10]+' 0.00    0.100D+02'
        card_drag_strings[drag_date_6]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_6[:10]+' 0.00    0.100D+02'

        #### --------------------------------------------------------------------
        ####    Search through the file to see if any of the cards we WANT are NOT in the file
        #### --------------------------------------------------------------------
        ##### read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()                
        ##### card flags to see if certain cards are present in the file
        card_flag = {}
        for card in card_strings:
            ### Set the default flag to be False,  if the card is in the file, flip the flag to True
            card_flag[card] = False
            for line in lines_all:
                if card in line:
                    card_flag[card] = True

        #### --------------------------------------------------------------------
        ####    Edit the cards that exist in the file that we want to modify
        #### --------------------------------------------------------------------
        ###### Re-write the file line-by-line and EDIT the cards that need to be modified    
        lines_replace = {}
        with open(iisset_file, "r") as f:
            lines = f.readlines()
            for line_num, line in enumerate(lines):
                for card in card_strings:
                    if card in line:
                        lines_replace[line_num] = card_strings[card]
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()
        with open(iisset_file, "w") as f:
            for line_num, line in enumerate(lines_all):
                if line_num in lines_replace:
#                     print('replacing line',lines_replace[line_num])
                    f.write(lines_replace[line_num]+'\n')
                else:
                     f.write(line)


        #### for adding time dependent drag estimations.  We need to do a few things:
        ###       Find the drag card that is already in the file:
        ###       Add CONDRAG before all drag cards
        ###       Add DRAG cards with TIME periods after the first drag card
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()                
        with open(iisset_file, "w") as f:
            for line in lines_all:
                if 'DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00' in line:  #this finds the DRAG line.  
                    f.write(card_drag_strings['CONDRG'] + ' \n')
                    f.write('DRAG             '+SAT_ID+' 2.3000000000000E+00'+ ' \n')
                    f.write(card_drag_strings[drag_date_1] + ' \n')                 
                    f.write(card_drag_strings[drag_date_2] + ' \n')                 
                    f.write(card_drag_strings[drag_date_3] + ' \n')                 
                    f.write(card_drag_strings[drag_date_4] + ' \n')                 
                    f.write(card_drag_strings[drag_date_5] + ' \n')                 
                    f.write(card_drag_strings[drag_date_6] + ' \n')                 
                else:
                    f.write(line)
                    
              
        #####-----------------------------------------------------------------------------
        ##### Delete the SATPAR GPS, EPOCH, ELEMS110, and ELEMS2 lines after the SATPAR GPS
        #####  Do this by finding the SATPAR for our sat and then saving it and the next 3 lines
        ##### then delete all the SATPAR,EPOCH,ELEMS110, ELEMS2 and restore the ones we saved
        #####-----------------------------------------------------------------------------

        ##### read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()    
        ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
        with open(iisset_file, "w") as f:
            for iline, line in enumerate(lines_all):
                if 'SATPAR' in line:
                    if SAT_ID in line:
                        save_SATPAR = iline+1
                        save_EPOCH = iline+2
                        save_ELEMS11 = iline+3
                        save_ELEMS2 = iline+4
                       
                elif 'SATPAR'in line:
                    pass
                elif 'EPOCH'in line:
                    pass
                elif 'ELEMS1'in line:
                    pass
                elif 'ELEMS2'in line:
                    pass
                elif 'MBIAS'in line:
                    pass
                
                else:
                    f.write(line)
                    
        line_SATPAR  = linecache.getline(iisset_file, save_SATPAR)
        line_EPOCH   = linecache.getline(iisset_file, save_EPOCH)
        line_ELEMS11 = linecache.getline(iisset_file, save_ELEMS11)
        line_ELEMS2  = linecache.getline(iisset_file, save_ELEMS2)

        ####----------------------------------------------------------------------
        #### REMOVE CARDS:: rewrite the file without the cards we specified in the cards_to_remove dict
        ####----------------------------------------------------------------------

        ##### read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()    
        ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
        with open(iisset_file, "w") as f:
            for iline, line in enumerate(lines_all):
                if any(card in line for card in cards_to_remove):
                    # IF the any of the cards in the list are in the line, dont add it
                    pass
                else:
                    f.write(line)                
        
                        
        
        ####----------------------------------------------------
        #### Add any cards that we want that are not in the file
        ##### this INCLUDES our saved 
        #####      SATPAR, EPOCH, ELEMS1, ELEMS2
        #####---------------------------------------------------------
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()                  

        # use some switches to determine if things have already been written in the loop and avoid writing too many
        switch_cardcount = 0
        switch_2     = True
        
        for card in card_flag:
            if card_flag[card] == False:
                with open(iisset_file, "w") as f:
                    for line in lines_all:
                        if 'ALBEDO' in line:  #this overwrites the line after albedo. 
                            # MAYBE TODO:  this may not write multiple cards that aren't in the file
                            if switch_cardcount == 0:
                                f.write(line)
                                f.write(card_strings[card] + ' \n') 
                            else: 
                                f.write(card_strings[card] + ' \n')
                                switch_cardcount += 1

                        elif (('REFSYS' in line) and (switch_2 == True)):
                            f.write(line)
                            f.write(line_SATPAR)
                            f.write(line_EPOCH)
                            f.write(line_ELEMS11)
                            f.write(line_ELEMS2)
                            switch_2 = False
                            
                        else:
                            f.write(line)
        ####----------------------------------------------------------------------
        #### Add three lines to the start of the file.  This is the GLOBAL TITLE
        ####----------------------------------------------------------------------

        with open(iisset_file, 'r+') as f:
            content = f.read()
            f.seek(0, 0)   # find the first lines
            f.write('### \n') 
            f.write('###   '+self.arc_input+'  \n') 
            f.write('### \n') 
            f.write(content) 
            
            
        ####----------------------------------------------------------------------
        #### Try doing a complete job of removing the GPS satellites.
        ####----------------------------------------------------------------------
            
        delete_gps_sats = [ '5041144',
                            '5044284',
                            '5051204',
                            '5154184',
                            '5345214',
                            '5347224',
                            '5356164',
                            '5459194',
                            '5460234',
                            '5461024',
                            '5553175',
                            '5652315',
                            '5658125', 
                            '5755155',
                            '5757295',
                            '5848075',
                            '5950055',
                            '6062256',
                            '6163016',
                            '6265246',
                            '6366276',
                            '6464306',
                            '6467066',
                            '6468096',
                            '6469036',
                            '6571266',            
                            '6572086',
                            '6573106',
                            '6649045',
                            '6670326',
                            '9743134',
                            '9946114',        
                            ]
        ##### read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()    
        ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
        with open(iisset_file, "w") as f:
            for iline, line in enumerate(lines_all):
                if any(gps in line for gps in delete_gps_sats):
                    # IF the any of GPS IDs in the list are in the line, dont add it the line
                    pass
                else:
                    f.write(line)      


    #### overwrite some methods from CONTROL:
    def print_runparameters_to_notebook(self):
        '''
        This method just prints to the run parameters to the notebook for user tracking.
        The original method is overwritten to account for the fact that we have a external attitude file
        
        '''
        self.verboseprint('ICESat2 -- print_runparameters_to_notebook()')

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
        self.verboseprint('ICESat2 -- prepare_tmpdir_for_geodyn_run()')
#         print(self.TMPDIR_arc)
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
            self.verboseprint(self.tabtab,'copied:   '+self._EXTATTITUDE_filename+' > EXAT01'+'.gz')
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
#         self.verboseprint(self.tabtab, "gunzipping the input data files")
        print(self.tabtab, "gunzipping the input data files")

        os.system('gunzip -vr *.gz')
#         os.system('gunzip -ftn01.gz')















#===========================================================================================================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#=========================================
#===========================================================================================================================











#=========================================
#-------------STARLETTE CLASS-------------
#=========================================


class Satellite_Starlette(PygeodynController,PygeodynReader):
    '''
    The Starlette class inherets the pygeodyn_CONTROL class and its methods.
    
    This class is used to make modifications to pygeodyn_CONTROL to run the Starlette satellite.
 
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

        
        
        
        
        
        
        
        
        
        
        
        
        


        
        
        
        
        
        
        