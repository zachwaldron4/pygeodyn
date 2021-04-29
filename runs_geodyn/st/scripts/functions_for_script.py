
# def EditScript_and_DONTRUN(model, arc, SpecialRun_name=''):

#     # Import packages
#     import linecache
#     import sys

#     file = 'geodyn_st.sh'
    
#     if model == 'msis86':
#         geodyn_mod_version=' '
#         name_SETUP_DEN_DIR='msis'

#     elif model == 'msis00':
#         geodyn_mod_version='_msis00_f90'
#         name_SETUP_DEN_DIR='msis'

#     elif model == 'msis2':
#         geodyn_mod_version='_msis2'
#         name_SETUP_DEN_DIR='msis'

#     elif model == 'jaachia71':
#         geodyn_mod_version=' '
#         name_SETUP_DEN_DIR='jaachia71'

#     elif model == 'dtm87':
#         geodyn_mod_version=' '
#         name_SETUP_DEN_DIR='dtm87'
#     else:
#         warn1 = 'No density model by that name: '
#         warn2 = model
#         warn  = warn1+warn2
#         sys.exit(warn)


#     # Edit the script
#     with open(file, 'r') as f:
#         lines_all = f.readlines()
#     with open(file, "w") as f:
#         for line in lines_all:
#             #-----------------------------------------------
#             # Edit the SETUP_DEN_DIR= file 
#             if 'SETUP_DEN_DIR=' in line:
#                 line_replace = 'SETUP_DEN_DIR='+name_SETUP_DEN_DIR
#                 # Check if line is in correct format
#                 if line_replace in line:
#                     f.write(line)
#                 else:
#                     f.write(line_replace+' \n')
#             #-----------------------------------------------
#             # Edit the SpecialRun_name= file 
#             elif 'SpecialRun_name=' in line:
#                 line_replace = 'SpecialRun_name='+SpecialRun_name
#                 # Check if line is in correct format
#                 if line_replace in line:
#                     f.write(line)
#                 else:
#                     f.write(line_replace+' \n')
#             #-----------------------------------------------
#             # Edit the DEN_LINE= file 
#             elif 'DEN_DIR=' in line:
#                 line_replace = 'DEN_DIR='+model
#                 # Check if line is in correct format
#                 if line_replace in line:
#                     f.write(line)
#                 else:
#                     f.write(line_replace+' \n')
#             #-----------------------------------------------
#             # Edit the GDYN_version= file 
#             elif 'GDYN_version=' in line:
#                 line_replace = 'GDYN_version='+geodyn_mod_version
#                 # Check if line is in correct format
#                 if line_replace in line:
#                     f.write(line)
#                 else:
#                     f.write(line_replace+' \n')
#             #-----------------------------------------------
#             # Edit the ARC_name= file 
#             elif 'ARC_name=' in line:
#                 line_replace = 'ARC_name='+arc
#                 # Check if line is in correct format
#                 if line_replace in line:
#                     f.write(line)
#                 else:
#                     f.write(line_replace+' \n')            
#             #-----------------------------------------------
#             else:
#                 f.write(line)
#     return()
    
    
    
    
    
    
    
    
    
    
    
    
    
def EditScript_and_RUN(model, arc, SpecialRun_name=''):
    # Import packages
    import subprocess
    import linecache
    import sys
    import shutil

    orig_file = 'geodyn_st.sh'
#     arc = 'st030914_2wk'
#     model = 'msis00'
#     SpecialRun_name = '_drhodzOrig'

    
    scripts_path = './script_runs/'
    custom_script_name = 'script'+'_'+arc+'_'+model+SpecialRun_name+'.sh'
    custom_script = scripts_path+custom_script_name
    shutil.copy(orig_file, custom_script)   
    
    
    if model == 'msis86':
        geodyn_mod_version=' '
        name_SETUP_DEN_DIR='msis'

    elif model == 'msis00':
        geodyn_mod_version='_msis00_f90'
        name_SETUP_DEN_DIR='msis'

    elif model == 'msis2':
        geodyn_mod_version='_msis2'
        name_SETUP_DEN_DIR='msis'

    elif model == 'jaachia71':
        geodyn_mod_version=' '
        name_SETUP_DEN_DIR='jaachia71'

    elif model == 'dtm87':
        geodyn_mod_version=' '
        name_SETUP_DEN_DIR='dtm87'
    else:
        warn1 = 'No density model by that name: '
        warn2 = model
        warn  = warn1+warn2
        sys.exit(warn)
    
    print('Script:',custom_script)

    # Edit the script
    with open(custom_script, 'r') as f:
        lines_all = f.readlines()
    with open(custom_script, "w") as f:
        for line in lines_all:
            #-----------------------------------------------
            # Edit the SETUP_DEN_DIR= file 
            if 'SETUP_DEN_DIR=' in line:
                line_replace = 'SETUP_DEN_DIR='+name_SETUP_DEN_DIR
                # Check if line is in correct format
                if line_replace in line:
                    f.write(line)
                else:
                    f.write(line_replace+' \n')
            #-----------------------------------------------
            # Edit the SpecialRun_name= file 
            elif 'SpecialRun_name=' in line:
                line_replace = 'SpecialRun_name='+SpecialRun_name
                # Check if line is in correct format
                if line_replace in line:
                    f.write(line)
                else:
                    f.write(line_replace+' \n')
            #-----------------------------------------------
            # Edit the DEN_LINE= file 
            elif 'DEN_DIR=' in line:
                line_replace = 'DEN_DIR='+model
                # Check if line is in correct format
                if line_replace in line:
                    f.write(line)
                else:
                    f.write(line_replace+' \n')
            #-----------------------------------------------
            # Edit the GDYN_version= file 
            elif 'GDYN_version=' in line:
                line_replace = 'GDYN_version='+geodyn_mod_version
                # Check if line is in correct format
                if line_replace in line:
                    f.write(line)
                else:
                    f.write(line_replace+' \n')
            #-----------------------------------------------
            # Edit the ARC_name= file 
            elif 'ARC_name=' in line:
                line_replace = 'ARC_name='+arc
                # Check if line is in correct format
                if line_replace in line:
                    f.write(line)
                else:
                    f.write(line_replace+' \n')            
            #-----------------------------------------------
            else:
                f.write(line)
       
    
    
    command = custom_script+" 2> "+scripts_path+"output_text.txt"
    process = subprocess.run(command, shell=True)
    print(process)
    return()
    
    
    
    
    
    
    