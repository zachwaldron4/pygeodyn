# # import shutil
# # import numpy as np
# # import pandas as pd
# # import linecache




# def clean_iisset_file(path_to_setupfiles, setup_file_arc, SAT_ID, den_model_setupval):

#     ORIG_iisset_file = path_to_setupfiles + '/' + setup_file_arc
    
#     shutil.copyfile(ORIG_iisset_file, path_to_setupfiles +'/'+'cleaned_setup_file')
#     iisset_file =       path_to_setupfiles +'/'+'cleaned_setup_file'

    
#     cards_to_remove = [ 'ACCEL9',
#                         'XEPHEM',
#                         'REFRAC',
#                         'GPSMOD',
#                         'OFFSET',
#                         'OFFADJ',
#                         'ANTPHC',
#                         'ANTPH2',
#                         'CGMASS',
#                       ] 

#     ##### Grab the EPOCH start and end times
#     EPOCH_lines = []
#     with open(iisset_file, 'r') as f:
#         for line_no, line_text in enumerate(f):
#             if 'EPOCH         ' in line_text:
#                 EPOCH_lines.append(line_no) 

#     ##### Identify and save the EPOCH start and end times
#     for i,val in enumerate(EPOCH_lines):
#         satpar_line = linecache.getline(iisset_file,val) # Check the above SATPAR line get the correct satellite ID (i.e. NOT GPS)

#         ##### only do this for the main satellite, so look for the correct SATID in the SATPAR card above EPOCH
#         if SAT_ID in satpar_line:
#             epoch_start = linecache.getline(iisset_file,val + 1)[20:40].strip() #181013210000.0000000
#             epoch_start_YYMMDD = linecache.getline(iisset_file,val + 1)[20:26].strip()       # 181013
#             epoch_start_HHMM = linecache.getline(iisset_file,val + 1)[26:30].strip()         # 210000.0000000
#             epoch_start_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[30:40].strip()   # 00.0000000     

#             epoch_end   = linecache.getline(iisset_file,val + 1)[60:80].strip() #1810160300 00.000
#             epoch_end_YYMMDD = linecache.getline(iisset_file,val + 1)[60:66].strip()       # 181016
#             epoch_end_HHMM = linecache.getline(iisset_file,val + 1)[66:70].strip()         # 210000.0000000
#             epoch_end_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[70:80].strip()   # 00.0000000     

#     ##### TO DO : Add conditional for month-day turnover
#     epoch_start_minus2days = epoch_start_YYMMDD[:-2]+str(int(epoch_start_YYMMDD[-2:])-2)+epoch_start_HHMM+epoch_start_SS_SSSSSSS
#     epoch_end_plus1days =  epoch_end_YYMMDD[:-2]+str(int(epoch_end_YYMMDD[-2:])+1)+epoch_end_HHMM+epoch_end_SS_SSSSSSS


#     ##### Putting in the options is one of the hardest parts of using GEODYN
#     #####    They require VERY specific inputs depending on the run type.  
#     card_strings = {}
#     card_strings['ORBFIL'] =  'ORBFIL2 31       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
#     card_strings['ORBTVU'] =  'ORBTVU1021       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
#     card_strings['RESID']  =  'RESIDU12'
#     card_strings['OBSVU']  =  'OBSVU 4'
#     card_strings['PRNTVU'] =  'PRNTVU55212222    22122'  
#     card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
#     card_strings['ATGRAV']  =  'ATGRAV9090              '+epoch_start_minus2days +''+epoch_end_plus1days[:-1]   
#     card_strings['I64G2E']  =  'I64G2E         25'  # using 30 like in st-SLR run maxed out the memory usage
#     card_strings['SATPAR']  =  'SATPAR   13      '+SAT_ID+'          9.53000000       1514.000'
#     card_strings['SIGMA           1']  =  '               10.0D+25            10.0D+25'
#     card_strings['SIGMA           2']  =  '               10.0D+25            10.0D+25'
#     card_strings['SIGMA           3']  =  '               10.0D+25            10.0D+25'
#     card_strings['SIGMA          51']  =  '               10.0D+25             0.10'
#     card_strings['SIGMA          85']  =  '               0.010000            0.010000'


#     ##### read in all lines of the file and save them
#     with open(iisset_file, "r") as f:
#         lines_all = f.readlines()    
#     ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
#     with open(iisset_file, "w") as f:
#         for line in lines_all:
#             if any(card in line for card in cards_to_remove):
#                 # IF the any of the cards in the list are in the line, dont add it
#                 pass
#             else:
#                 f.write(line)                

#     ##### DO IT AGAIN but with the above updates
#     ##### read in all lines of the file and save them
#     with open(iisset_file, "r") as f:
#         lines_all = f.readlines()                

#     ####------------------------------------------------
#     #### Check to see if cards we want are in file first
#     ####------------------------------------------------
#     ##### card flags to see if certain cards are present in the file
#     card_flag = {}
#     for card in card_strings:
#         ### Set the default flag to be False,  if the card is in the file, flip the flag to True
#         card_flag[card] = False
#         for line in lines_all:
#             if card in line:
#                 card_flag[card] = True


#     ## Re-write the file line-by-line and EDIT the cards that need to be modified    
#     with open(iisset_file, "w") as f:
#         for line in lines_all:
#     #         if (card in line for card in card_strings):
#     #             line_replace = card_strings[card]
#     #             f.write(line_replace+' \n')
#     #         else:
#     #             f.write(line)

#             for card in card_strings:
#                 line_replace = card_strings[card]

#                 if card in line:
#                     # Go ahead and re-write the line to be in the desired format
#                     f.write(line_replace+' \n')
#                     break
#                 else:
#                     f.write(line)
#                     break

#     ##### DO IT AGAIN but with the above updates
#     ###    read in all lines of the file and save them
#     with open(iisset_file, "r") as f:
#         lines_all = f.readlines()                  
#     ####----------------------------------------------------
#     #### Add any cards that we want that are not in the file
#     ####----------------------------------------------------
#     for card in card_flag:
#         if card_flag[card] == False:

#             with open(iisset_file, "w") as f:
#                 for line in lines_all:
#                     if 'ALBEDO' in line:
#                         f.write(line)
#                         f.write(card_strings[card] + ' \n')                 
#                     else:
#                         f.write(line)

# #     return
