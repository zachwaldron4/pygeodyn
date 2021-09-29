
#### ===============
#### Import modules:
#### ===============
import pandas as pd


#### Computer/CL functions
import sys
import linecache



##### Get the epoch start and end

def EditSetupFile__get_epoch_times(iisset_file, SAT_ID, decimated_SAT_ID = False, decimated_flag= False):
    
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
        
        if decimated_flag == True:
            if decimated_SAT_ID in satpar_line or SAT_ID in satpar_line:

                epoch_start = linecache.getline(iisset_file,val + 1)[20:40].strip() #181013210000.0000000
                epoch_start_YYMMDD = linecache.getline(iisset_file,val + 1)[20:26].strip()       # 181013
                epoch_start_HHMM = linecache.getline(iisset_file,val + 1)[26:30].strip()         # 2100
                epoch_start_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[30:40].strip()   # 00.0000000     

                epoch_end   = linecache.getline(iisset_file,val + 1)[60:80].strip() #1810160300 00.000
                epoch_end_YYMMDD = linecache.getline(iisset_file,val + 1)[60:66].strip()       # 181016
                epoch_end_HHMM = linecache.getline(iisset_file,val + 1)[66:70].strip()         # 210000.0000000
                epoch_end_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[70:80].strip()   # 00.0000000    


    #### Sometimes the date is in the wrong format if the YY is only one digit, add 
    ####        (i.e.     "30914"   --> "030914")
    if len(epoch_start_YYMMDD) == 5:
        epoch_start_YYMMDD = '0'+epoch_start_YYMMDD
    if len(epoch_end_YYMMDD) == 5:
        epoch_end_YYMMDD = '0'+epoch_end_YYMMDD

    ####  The weird decimation of zeros made it so that some zreos are missing
    if len(epoch_start_HHMM) < 4:
        if int(epoch_start_HHMM) == 0:
            epoch_start_HHMM = '0000'
    if len(epoch_end_HHMM) < 4:
        if int(epoch_end_HHMM) == 0:
            epoch_end_HHMM = '0000'

    epoch_start= epoch_start_YYMMDD + epoch_start_HHMM + epoch_start_SS_SSSSSSS
    epoch_end  = epoch_end_YYMMDD   + epoch_end_HHMM   + epoch_end_SS_SSSSSSS

#     print('epoch_start', epoch_start)
#     print('epoch_start_YYMMDD', epoch_start_YYMMDD)
#     print('epoch_start_HHMM', epoch_start_HHMM)
#     print('epoch_end', epoch_end)
#     print('epoch_end_YYMMDD', epoch_end_YYMMDD)
#     print('epoch_end_HHMM' , epoch_end_HHMM)
    
    return( epoch_start,
            epoch_start_YYMMDD,
            epoch_start_HHMM,
            epoch_end,
            epoch_end_YYMMDD,
            epoch_end_HHMM) 



##### Change the time dependent drag inputs?

    #### --------------------------------------------------------------------
    #### Use pandas datetime and time delta to make adjustments to the dates on the ATGRAV and DRAG cards
    #### --------------------------------------------------------------------

def EditSetupFile__timedep_drag(iisset_file, SAT_ID, epoch_start_dt, epoch_end_dt):

    #### for adding time dependent drag estimations.  We need to do a few things:
    ###       Find the drag card that is already in the file:
    ###       Add CONDRAG before all drag cards
    ###       Add DRAG cards with TIME periods after the first drag card

    
    add_hours_dt = pd.Series(pd.to_timedelta(6,'h'))

    drag_date_1 = (epoch_start_dt+add_hours_dt).dt.strftime(  '%y%m%d%H%M%S').values[0]
    drag_date_2 = (epoch_start_dt+add_hours_dt*2).dt.strftime('%y%m%d%H%M%S').values[0]
    drag_date_3 = (epoch_start_dt+add_hours_dt*3).dt.strftime('%y%m%d%H%M%S').values[0]
    drag_date_4 = (epoch_start_dt+add_hours_dt*4).dt.strftime('%y%m%d%H%M%S').values[0]
    drag_date_5 = (epoch_start_dt+add_hours_dt*5).dt.strftime('%y%m%d%H%M%S').values[0]
    drag_date_6 = (epoch_start_dt+add_hours_dt*6).dt.strftime('%y%m%d%H%M%S').values[0]
    drag_date_rm = (epoch_start_dt+add_hours_dt*6).dt.strftime('%y%m%d%H%M%S').values[0]
    card_drag_strings={}
    card_drag_strings['CONDRG']  =  'CONDRG  1        '+SAT_ID+'     '+str(epoch_start[:-5])+str(epoch_end[:-5])+'         0.50000  28800.'
    #         card_drag_strings['DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00']  =  'DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00'
    card_drag_strings[drag_date_1]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_1[:10]+' 0.00    0.100D+02'
    card_drag_strings[drag_date_2]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_2[:10]+' 0.00    0.100D+02'
    card_drag_strings[drag_date_3]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_3[:10]+' 0.00    0.100D+02'
    card_drag_strings[drag_date_4]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_4[:10]+' 0.00    0.100D+02'
    card_drag_strings[drag_date_5]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_5[:10]+' 0.00    0.100D+02'
    card_drag_strings[drag_date_6]  = 'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_date_6[:10]+' 0.00    0.100D+02'

    
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
                
    return(card_drag_strings)


##### Change the ELEMS (Initial conditions)  # SPECIFIC TO PCE BASED RUNS AS OF NOW

#         ##### -------------------------------------------------------------------------------------------
#         ##### -------------------------------------------------------------------------------------------
#         ##### -------------------------------------------------------------------------------------------
#         ###       FIND THE X,Y,Z,Xdot,Ydot,Zdot for this epoch start in the PCE data.
#         ##### -------------------------------------------------------------------------------------------
# #         os.system('bunzip2'+' '+self.StateVector_epochs_datafile+'.bz2')
        
#         epoch_start_dt_STR = str(epoch_start_dt)
#         date_in_file_flag = False
        
#         print("Epoch Start: ", epoch_start_dt_STR)

#         with open(self.StateVector_epochs_datafile, 'r') as f:
#             for line_no, line_text in enumerate(f):
                
#                 if epoch_start_dt_STR in line_text:
#                     date_in_file_flag= True
# #                     print('    ','xyzline',line_no,line_text)

#                     break
           
#         if date_in_file_flag == False:
#             change_elems_flag = False
#             print(epoch_start_dt_STR,'not found in file.  Leaving ELEMS as is.')
# #             print('Check that the start date:',epoch_start_dt_STR)
# #             print('    is within the PCE date range saved in the file')
# #             print('       ',self.StateVector_epochs_datafile)
# #                     os.system('bzip2'+' '+'/data/data_geodyn/inputs/icesat2/setups/StateVector_epochs.txt')
# #             sys.exit()

#         else:
#             change_elems_flag = True
#             xyzline = pd.read_csv(self.StateVector_epochs_datafile, 
#                         skiprows = line_no, 
#                         nrows=1,           
#                         sep = '\s+',
#                         dtype=str,
#                         names = [
#                             'Date',
#                             'MJDSECs', 
#                             'RSECS', #(fractional secs)
#                             'GPS offset', # (UTC - GPS offset (secs))
#                             'X',
#                             'Y',
#                             'Z',
#                             'X_dot',
#                             'Y_dot',
#                             'Z_dot',
#                             'YYMMDDhhmmss',
#                                 ],)

#             X     =  xyzline['X'].values[0].ljust(20)     #'  -745933.8926940708'
#             Y     =  xyzline['Y'].values[0].ljust(20)     #'  -4864983.834066438'
#             Z     =  xyzline['Z'].values[0].ljust(20)     #'    4769954.60524261'
#             X_dot =  xyzline['X_dot'].values[0].ljust(20) #'  457.44564954037634'
#             Y_dot =  xyzline['Y_dot'].values[0].ljust(20) #'   5302.381564886811'
#             Z_dot =  xyzline['Z_dot'].values[0].ljust(20) #'    5463.55571622269'

#     #         os.system('bzip2'+' '+self.StateVector_epochs_datafile)
#             ##### -------------------------------------------------------------------------------------------
#             #### --------------------------------------------------------------------------------------------





##### Change cards to specific values

# def EditSetupFile__modify_cards(iisset_file):

#     ####   INPUT THE OPTIONS ON THE SPECIFIC CARDS YOU WANT TO CHANGE
#     ##### Putting in the options is one of the hardest parts of using GEODYN
#     #####    They require VERY specific inputs depending on the run type.  
#     card_strings = {}


#         #####  ORBFIL KEY ------ Requests output of trajectory file(s) on specified unit(s) 
#         #####                           on the last iteration of the run.
#         #####
#         #####   columns      Orbit output option
#         #####    7           Coordinate system of output
#         #####                      0 - True of date (default)
#         #####                      1 - True of reference date 
#         #####                   ** 2 - Mean of year 2000    
#         #####    8           Switch indicating whether trajectory file is for a single 
#         #####                  satellite or a set of satellites.
#         #####                   ** 0 - Single satellite 0 0
#         #####                      1 - Set of satellites. This option has meaning 
#         #####                            only when used in conjunction with sets of 
#         #####                            satellites (See EPOCH and SLAVE option cards
#         #####                            for more details ). If satellite ID in columns
#         #####                            18-24 is a master satellite , then the trajectory
#         #####                          for all satellites in the set will be output.
#         #####  9-11           Mandatory unit number for trajectory file. All trajectory 
#         #####                  files within an arc must have unique unit numbers. 
#         #####                  The suggested unit number starts at 130.
#         #####  18-25        Satellite ID. This field must contain a valid ID.
#         #####  25-44        START date and time for trajectory output (YYMMDDHHMMSS.SS).
#         #####  45-59        STOP  date and time for trajectory output (YYMMDDHHMMSS.SS).
#         #####  60-72        Time interval between successive trajectory outputs.


#     #                                  12345678901234567 
#     card_strings['ORBFIL'] =  'ORBFIL20131      '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00          60'
#     card_strings['RESID']  =  'RESIDU12'
#     card_strings['OBSVU']  =  'OBSVU 3'  # print residuals on First and last iterations only
#     #       card_strings['PRNTVU'] =  'PRNTVU55212222    22122'  # original
#     card_strings['PRNTVU'] =  'PRNTVU5521111211 121122'  # suppress some IIS/IIE outputs.
#     #                                  1234567890 
#     card_strings['ORBTVU'] =  'ORBTVU1201       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'

#     ##### --------------------------------------------------------------------
#         ####   PRNTVU KEY ------ Used to control the IIS and IIE printed content 
#         ####                             Be warned that if you include a bunch, the file 
#         ####                             will be huge, and the run time will increase 
#         ####                             (printing to ascii is slow)
#         ####    columns      IIS output option
#         ####      9          Simple list of GEODYN -IIS setup. Interpretive
#         ####     10          Interpretive list of GEODYN -IIS setup.
#         ####     11          Observation block selection report.
#         ####     12          Gravity model coefficients. Global  
#         ####     13          Global parameter values and sigmas.                             
#         ####     14          Arc parameter values and sigmas.
#         ####     15          Sea surface topography. Ocean
#         ####     16          Ocean Tide Model.  
#         ####    columns      IIE output option
#         ####     18          Simple list of GEODYN -IIS setup.
#         ####     19          Values of estimated E-biases.
#         ####     20          E-matrix labels in Summary Page.
#         ####     21          Adjusted station baselines  
#         ####     22          Correlations for adjusted parameters.                             
#         ####     23          Shadow crossing. 
#         ####-----------------------------------------------------------------------------------------------
#         ##### ORBTVU KEY ------ Controls the printing of ASCII 
#         #####                           orbit info to a separate file.
#         ####   columns       Orbit output option
#         #####    7           Frequency of trajectory output.
#         #####                    0 - vwd btwn times in cols 25-59 and at 
#         #####                          intvl specfd in columns 60-72.
#         #####                  **1 - vwd btwn times in cols 25-59 at 
#         #####                          data points only.
#         #####                    2 - vwd btwn times in cols 25-59 at data
#         #####                          points and at the intvl in columns 60-72.
#         #####    8           Coordinate system of output
#         #####                  **0 - True of date
#         #####                    1 - True of ref date 
#         #####                    2 - Mean of year 2000
#         #####    9           Trajectory type indicator.
#         #####                  **0 - Cartesian ephemeris 
#         #####                    1 - Keplerian ephemeris 
#         #####                    2 - Both Cartesian and Keplerian ephemerides.
#         #####    1           Iterations on which trajectory will be printed.
#         #####                    0 - First arc iter of first global iter 
#         #####                  **1 - Last arc iter of last global iter 
#         #####                    2 - Both first first and last last 
#         #####                    3 - All iterations

#     card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
#     card_strings['ATGRAV']  =  'ATGRAV9090              '+dt_epoch_start_minus2days +''+dt_epoch_end_plus1days[:-1]   
#     card_strings['I64G2E']  =  'I64G2E         25'  # using 30 like in st-SLR run maxed out the memory usage
#     card_strings['SIGMA           1']  =  'SIGMA           1               1.0                 1.0'    
#     card_strings['SIGMA           2']  =  'SIGMA           2               1.0                 1.0'    
#     card_strings['SIGMA           3']  =  'SIGMA           3               1.0                 1.0'   
#     card_strings['SIGMA          51']  =  'SIGMA          51               10.0D+25             0.10'  
#     card_strings['SIGMA          85']  =  'SIGMA          85               0.010000            0.010000'  


#     ### Fix the coordinate system... PCE Data was in J2000
#     #         card_strings['REFSYS1933 0        ']  = 'REFSYS193310        '+epoch_start+'0'
#     #         card_strings['SATPAR   13']  =  'SATPAR   139     '+SAT_ID+'          9.53000000       1514.000'
#     card_strings['REFSYS']  = 'REFSYS193310        '+epoch_start+'0'
#     card_strings['EPOCH'] = 'EPOCH               '+epoch_start+epoch_start+epoch_end
#     card_strings['SATPAR']  =  'SATPAR   139     '+SAT_ID+'          9.53000000       1514.000'

#     if change_elems_flag == True:
#         card_strings['ELEMS1']  = 'ELEMS11             '+X+''+Y+''+Z+''   
#         card_strings['ELEMS2']  = 'ELEMS2              '+X_dot+''+Y_dot+''+Z_dot+''


#     #### Suppress the printing of the flux model
#     card_strings['FLUX  1']  =  'FLUX  0'

#     return(card_strings)

    
    
    
def EditSetupFile__identify_missing_cards(iisset_file, card_strings):

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

    return(card_flag)







def EditSetupFile__rewrite_file_using_modified_cards(iisset_file, card_strings):

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
#                 print('replacing line',lines_replace[line_num])
                f.write(lines_replace[line_num]+'\n')
            else:
                 f.write(line)

                

                
                
                
def EditSetupFile__rewrite_file_and_remove_unwantedcards(iisset_file, cards_to_remove):
    
    ##### read in all lines of the file and save them
    with open(iisset_file, "r") as f:
        lines_all = f.readlines()    
    ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
    with open(iisset_file, "w") as f:
        for iline, line in enumerate(lines_all):
            if any(card in line for card in cards_to_remove):
#                 print('removing card: ', card)
                # IF the any of the cards in the list are in the line, dont add it
                pass
            else:
                f.write(line)  
                
                
                
                
                
def EditSetupFile__rewrite_file_and_add_missing_cards(iisset_file, card_flag, card_strings):
                
    with open(iisset_file, "r") as f:
        lines_all = f.readlines()                  

    # use some switches to determine if things have already been written in the loop and avoid writing too many
    switch_2     = True
    switch_cardcount = 0

    with open(iisset_file, "w") as f:
        for line in lines_all:
            if 'ALBEDO' in line:  #this overwrites the line after albedo. 
                # MAYBE TODO:  this may not write multiple cards that aren't in the file
                for card in card_flag:
                    if card_flag[card] == False:
                        if switch_cardcount == 0:
                            f.write(line)
                            f.write(card_strings[card] + ' \n')
#                             print('0 Adding the card', card)
                            switch_cardcount += 1
                        else: 
                            f.write(card_strings[card] + ' \n')
#                             print('1 Adding the card', card)
                            switch_cardcount += 1

            else:
                f.write(line)                
                
                
                
                
def EditSetupFile__rewrite_file_delete_GPS_sats(iisset_file):

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
                # If the any of GPS IDs in the list are in the line, dont add it the line
                pass
            else:
                f.write(line)      

    return
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
