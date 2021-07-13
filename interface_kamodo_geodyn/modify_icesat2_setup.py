import pandas as pd

#### Computer/Command Line functions
import os
import os.path
import sys
import subprocess
import shutil
import linecache
import time

sys.path.insert(0, '/data/geodyn_proj/pygeodyn/pygeodyn_develop/')
from PYGEODYN import Pygeodyn


class edit_ICESat2_setup(Pygeodyn):
#     def __init__(self):
#         pass
    
    
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
#         print('REDIFINE WORKED!')
        self.verboseprint('ICESat2 -- clean_iisset_file()')

        #### --------------------------------------------------------------------
        #### Initialize our variables from user input
        (path_to_setupfiles, setup_file_arc, SAT_ID, den_model_setupval) = ( self.INPUTDIR,  self.setup_file_arc, self.SATID, self.iisset_den)      
        
        ORIG_iisset_file = self._INPUT_filename 
        iisset_file      = 'cleaned_setup'+'_'  + self.arcdate_for_files

        #### --------------------------------------------------------------------
        ##### COPY THE FILE SO THAT YOU DON'T OVERWRITE THE ORIGINAL
        ####    We copy to a temporary file "cleaned_setup_file"
        
            
        shutil.copyfile(ORIG_iisset_file, self.TMPDIR_arc +'/'+iisset_file+'.bz2')
        
        os.chdir(self.TMPDIR_arc)
        os.system('bunzip2 -v '+ '*.bz2')
        os.chdir('/data/geodyn_proj/pygeodyn')
        
        iisset_file = self.TMPDIR_arc+'/' +'cleaned_setup'+'_'  + self.arcdate_for_files
#         print(iisset_file)
        
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
                            'MBIAS',
                           # 
                            'SATPAR',
                            'EPOCH',
                            'ELEMS1',
                            'ELEMS2',
                           #
                            'ORBTVU',
                            'RESID',
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

        ##### ICESat2 has an orbit period of 94.22 minutes
        #### Lets adjust the drag coefficient every 96 minutes
        
        

        dt_2days = pd.Series(pd.to_timedelta(48,'h'))
        dt_1days = pd.Series(pd.to_timedelta(24,'h'))
        
        dt_epoch_start_minus2days = (epoch_start_dt - dt_2days).dt.strftime('%y%m%d%H%M%S.0000000').values[0]
        dt_epoch_end_plus1days    = (epoch_end_dt + dt_1days).dt.strftime('%y%m%d%H%M%S.000').values[0]
        
        ##### -------------------------------------------------------------------------------------------
        ##### -------------------------------------------------------------------------------------------
        ##### -------------------------------------------------------------------------------------------
        ###       FIND THE X,Y,Z,Xdot,Ydot,Zdot for this epoch start in the PCE data.
        ##### -------------------------------------------------------------------------------------------
#         os.system('bunzip2'+' '+self.StateVector_epochs_datafile+'.bz2')
        
        epoch_start_dt_STR = str(epoch_start_dt)
        date_in_file_flag = False
        
        print("Epoch Start: ", epoch_start_dt_STR)

        with open(self.StateVector_epochs_datafile, 'r') as f:
            for line_no, line_text in enumerate(f):
                
                if epoch_start_dt_STR in line_text:
                    date_in_file_flag= True
#                     print('    ','xyzline',line_no,line_text)

                    break
           
        if date_in_file_flag == False:
            change_elems_flag = False
            print(epoch_start_dt_STR,'not found in file.  Leaving ELEMS as is.')
#             print('Check that the start date:',epoch_start_dt_STR)
#             print('    is within the PCE date range saved in the file')
#             print('       ',self.StateVector_epochs_datafile)
#                     os.system('bzip2'+' '+'/data/data_geodyn/inputs/icesat2/setups/StateVector_epochs.txt')
#             sys.exit()

        else:
            change_elems_flag = True
            xyzline = pd.read_csv(self.StateVector_epochs_datafile, 
                        skiprows = line_no, 
                        nrows=1,           
                        sep = '\s+',
                        dtype=str,
                        names = [
                            'Date',
                            'MJDSECs', 
                            'RSECS', #(fractional secs)
                            'GPS offset', # (UTC - GPS offset (secs))
                            'X',
                            'Y',
                            'Z',
                            'X_dot',
                            'Y_dot',
                            'Z_dot',
                            'YYMMDDhhmmss',
                                ],)

            X     =  xyzline['X'].values[0].ljust(20)     #'  -745933.8926940708'
            Y     =  xyzline['Y'].values[0].ljust(20)     #'  -4864983.834066438'
            Z     =  xyzline['Z'].values[0].ljust(20)     #'    4769954.60524261'
            X_dot =  xyzline['X_dot'].values[0].ljust(20) #'  457.44564954037634'
            Y_dot =  xyzline['Y_dot'].values[0].ljust(20) #'   5302.381564886811'
            Z_dot =  xyzline['Z_dot'].values[0].ljust(20) #'    5463.55571622269'

    #         os.system('bzip2'+' '+self.StateVector_epochs_datafile)
        ##### -------------------------------------------------------------------------------------------
        
        ####   INPUT THE OPTIONS ON THE SPECIFIC CARDS YOU WANT TO CHANGE
        ##### Putting in the options is one of the hardest parts of using GEODYN
        #####    They require VERY specific inputs depending on the run type.  
        card_strings = {}
    
            
#                                  12345678901234567 
        card_strings['ORBFIL'] =  'ORBFIL20131      '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00          60'
        card_strings['RESID']  =  'RESIDU12'
        card_strings['OBSVU']  =  'OBSVU 2'  # print residuals on last iteration only
        #       card_strings['PRNTVU'] =  'PRNTVU55212222    22122'  # original
        card_strings['PRNTVU'] =  'PRNTVU5521111211 121122'  # suppress some IIS/IIE outputs.
#                                  1234567890 
        card_strings['ORBTVU'] =  'ORBTVU1201       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
        card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
        card_strings['ATGRAV']  =  'ATGRAV9090              '+dt_epoch_start_minus2days +''+dt_epoch_end_plus1days[:-1]   
        card_strings['I64G2E']  =  'I64G2E         25'  # using 30 like in st-SLR run maxed out the memory usage
        card_strings['SIGMA           1']  =  'SIGMA           1               1.0                 1.0'    
        card_strings['SIGMA           2']  =  'SIGMA           2               1.0                 1.0'    
        card_strings['SIGMA           3']  =  'SIGMA           3               1.0                 1.0'   
        card_strings['SIGMA          51']  =  'SIGMA          51               10.0D+25             0.10'  
        card_strings['SIGMA          85']  =  'SIGMA          85               0.010000            0.010000'  
        
        ##### MODIFY THE INTEGRATION STEP SIZE
#                                 123456789012345678901234567890 
        card_strings['STEP']  =  'STEP             '+SAT_ID+'           60.' # modify this from 10s -> 60s


        ### Fix the coordinate system... PCE Data was in J2000
#         card_strings['REFSYS1933 0        ']  = 'REFSYS193310        '+epoch_start+'0'
#         card_strings['SATPAR   13']  =  'SATPAR   139     '+SAT_ID+'          9.53000000       1514.000'
        card_strings['REFSYS']  = 'REFSYS193310        '+epoch_start+'0'
        card_strings['EPOCH']   = 'EPOCH               '+epoch_start+epoch_start+epoch_end
        card_strings['SATPAR']  =  'SATPAR   139     '+SAT_ID+'          9.53000000       1514.000'
        
        if change_elems_flag == True:
            card_strings['ELEMS1']  = 'ELEMS11             '+X+''+Y+''+Z+''   
            card_strings['ELEMS2']  = 'ELEMS2              '+X_dot+''+Y_dot+''+Z_dot+''
                
        #### Suppress the printing of the flux model
        card_strings['FLUX  1']  =  'FLUX  0'

    

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

#         add_hours_dt = pd.Series(pd.to_timedelta(9,'h'))
#         add_mins_dt = pd.Series(pd.to_timedelta(94.22,'m'))
        add_mins_dt = pd.Series(pd.to_timedelta(9,'h'))
    
        drag_date_1 =  (epoch_start_dt+ add_mins_dt*1).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_2 =  (epoch_start_dt+ add_mins_dt*2).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_3 =  (epoch_start_dt+ add_mins_dt*3).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_4 =  (epoch_start_dt+ add_mins_dt*4).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_5 =  (epoch_start_dt+ add_mins_dt*5).dt.strftime('%y%m%d%H%M%S').values[0]
        drag_date_6 =  (epoch_start_dt+ add_mins_dt*6).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_7 =  (epoch_start_dt+ add_mins_dt*7).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_8 =  (epoch_start_dt+ add_mins_dt*8).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_9 =  (epoch_start_dt+ add_mins_dt*9).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_10 = (epoch_start_dt+add_mins_dt*10).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_11 = (epoch_start_dt+add_mins_dt*11).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_12 = (epoch_start_dt+add_mins_dt*12).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_13 = (epoch_start_dt+add_mins_dt*13).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_14 = (epoch_start_dt+add_mins_dt*14).dt.strftime('%y%m%d%H%M%S').values[0]
#         drag_date_15 = (epoch_start_dt+add_mins_dt*15).dt.strftime('%y%m%d%H%M%S').values[0]
        
        
        
        print('epoch end   :',epoch_end[:-5])
        print('epoch start :',epoch_start[:-5])
                
        card_drag_strings={}                                                                                                           #11307.
        card_drag_strings['CONDRG']  =  'CONDRG  1        '+SAT_ID+'     '+str(epoch_start[:-5])+str(epoch_end[:-5])+'         0.50000  28800.'
#         card_drag_strings['DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00']  =  'DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00'
        card_drag_strings[drag_date_1]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_1[:10]+' 0.00    1.000000E+00'
        card_drag_strings[drag_date_2]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_2[:10]+' 0.00    1.000000E+00'
        card_drag_strings[drag_date_3]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_3[:10]+' 0.00    1.000000E+00'
        card_drag_strings[drag_date_4]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_4[:10]+' 0.00    1.000000E+00'
        card_drag_strings[drag_date_5]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_5[:10]+' 0.00    1.000000E+00'
        card_drag_strings[drag_date_6]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_6[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_7]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_7[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_8]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_8[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_9]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_9[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_10]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_10[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_11]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_11[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_12]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_12[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_13]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_13[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_14]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_14[:10]+' 0.00    1.000000E+00'
#         card_drag_strings[drag_date_15]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_15[:10]+' 0.00    1.000000E+00'
        
        
        print(drag_date_1)
        print(drag_date_2)
        print(drag_date_3)
        print(drag_date_4)
        print(drag_date_5)
        print(drag_date_6)
#         print(drag_date_7)
#         print(drag_date_8)
#         print(drag_date_9)
#         print(drag_date_10)
#         print(drag_date_11)
#         print(drag_date_12)
#         print(drag_date_13)
#         print(drag_date_14)
#         print(drag_date_15)
        
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()                
        with open(iisset_file, "w") as f:
            for line in lines_all:
                if 'DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00' in line:  #this finds the DRAG line.  
                    f.write(card_drag_strings['CONDRG'] + ' \n')
                    f.write('DRAG             '+SAT_ID+' 2.2000000000000E+00'+ ' \n')
                    f.write(card_drag_strings[drag_date_1] + ' \n')                 
                    f.write(card_drag_strings[drag_date_2] + ' \n')                 
                    f.write(card_drag_strings[drag_date_3] + ' \n')                 
                    f.write(card_drag_strings[drag_date_4] + ' \n')                 
                    f.write(card_drag_strings[drag_date_5] + ' \n')                 
                    f.write(card_drag_strings[drag_date_6] + ' \n') 
#                     f.write(card_drag_strings[drag_date_7] + ' \n')                 
#                     f.write(card_drag_strings[drag_date_8] + ' \n')                 
#                     f.write(card_drag_strings[drag_date_9] + ' \n')                 
#                     f.write(card_drag_strings[drag_date_10] + ' \n')                 
#                     f.write(card_drag_strings[drag_date_11] + ' \n')                 
#                     f.write(card_drag_strings[drag_date_12] + ' \n')                 
#                     f.write(card_drag_strings[drag_date_13] + ' \n')                 
#                     f.write(card_drag_strings[drag_date_14] + ' \n')                 
#                     f.write(card_drag_strings[drag_date_15] + ' \n')                 

                else:
                    f.write(line)
                    
              
        #####-----------------------------------------------------------------------------
        #####   Delete the SATPAR GPS, EPOCH, ELEMS110, and ELEMS2 lines after the SATPAR GPS
        #####   Do this by finding the SATPAR for our sat and then saving it and the next 3 lines
        #####   then delete all the SATPAR,EPOCH,ELEMS110, ELEMS2 and restore the ones we saved
        #####-----------------------------------------------------------------------------

        if change_elems_flag == False:
            ##### read in all lines of the file and save them
            with open(iisset_file, "r") as f:
                lines_all = f.readlines()    
            
            for iline, line in enumerate(lines_all):
                if 'ELEMS1'in line:
                    save_ELEMS1 = iline+1
                elif 'ELEMS2' in line:
                    save_ELEMS2 = iline+1
                
            line_ELEMS1 = linecache.getline(iisset_file, save_ELEMS1)
            line_ELEMS2  = linecache.getline(iisset_file, save_ELEMS2)
            
            card_strings['ELEMS1']  = line_ELEMS1
            card_strings['ELEMS2']  = line_ELEMS2
            
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
                        else:
                            f.write(line)
                            
        ##### Write our satellite parameters back in         
        with open(iisset_file, "w") as f:
            for line in lines_all:
                if (('REFSYS' in line) and (switch_2 == True)):
                    f.write(card_strings['REFSYS']  + ' \n')
                    f.write(card_strings['SATPAR']  + ' \n')
                    f.write(card_strings['EPOCH']   + ' \n')
                    if change_elems_flag == True:
                        f.write(card_strings['ELEMS1']  + ' \n')
                        f.write(card_strings['ELEMS2']  + ' \n')
                    elif change_elems_flag == False:
                        f.write(card_strings['ELEMS1'])
                        f.write(card_strings['ELEMS2'])

                    switch_2 = False
                else:
                    f.write(line)

        self.verboseprint('    ','PCE Update:')
        self.verboseprint('    ','    ',card_strings['ELEMS1'])
        self.verboseprint('    ','    ',card_strings['ELEMS2'])
        
        ####----------------------------------------------------------------------
        #### Add three lines to the start of the file.  This is the GLOBAL TITLE
        ####----------------------------------------------------------------------

        with open(iisset_file, 'r+') as f:
            content = f.read()
            f.seek(0, 0)   # find the first lines
            f.write('### \n') 
            f.write('###   '+self.arc_name_id+'  \n') 
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

