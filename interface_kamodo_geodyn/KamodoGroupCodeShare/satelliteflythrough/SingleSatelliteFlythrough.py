# -*- coding: utf-8 -*-
"""
Created on Wed Apr  7 14:57:55 2021
author: rringuet

Code to be called from other languages for a single trajectory point.
All desired model data should be in a single directory.
"""


def _ChooseSingleModelWrapper(model):
    '''choose and return proper model wrapper'''
    
    #as other flythrough codes are written, add more options here
    if model == 'CTIPe':  #need to add these as part of kamodo
        from kamodo.readers.CTIPe_wrapper_single import CTIPe_Single_FlyAway as SF
    elif model == 'GITM':  #later: from kamodo.readers.GITM_wrapper import ...
        from GITM_wrapper import GITM_Single_FlyAway as SF
    elif model == 'IRI':
        from kamodo.readers.IRI_wrapper import IRI_Single_FlyAway as SF
        
    return SF


#want to enable call of this from C++ for flexibility, so return only one value
#keep so users can call this if they have their own satellite trajectory data
def SingleModelFlythrough(model, file_dir, variable_list, sat_time, sat_height, 
                                 sat_lat, sat_lon, z_dependence, dz = ''):  
    '''Call satellite flythrough wrapper specific to the model chosen.
    Parameters:    
        Name of model: model (Options: 'CTIPe', ...)
        Absolute path to where model data is stored: file_dir
        List of desired standardized variable names: variable_list
        Single float of satellite trajectory timestamps: sat_time
            (in number of seconds since 1970-01-01)
        Single float of satellite trajectory heights in meters: sat_height
        Single float of satellite trajectory latitudes in degrees: sat_lat
        Single float of satellite trajectory longitudes in degrees: sat_lon   
        List of vertical dependence for each variable in variable_list in same
            order as variables: z_dependence. Options for each variable: 'none', 'height', 'ilev'
            for no dependence on height, dependence on height, dependence on pressure
            altitude (ilev)
        List of booleans or 0's and 1's: dz. Indicates whether partial derivative
            of each variable with height in variable_list is requested. If
            z_dependence for that variable is 'none', then this term is ignored.
    Returns a dictionary with keys: sat_time, sat_height, sat_lat, sat_lon, 
    and keys naming the requested variables.
        sat_time is a single float in seconds since 1970-01-01.
        sat_height is a single float in meters.
        sat_lat and sat_lon are single floats in degrees.
        model variable keys are returned in the units printed out.
    ''' 
    
    wrapper = _ChooseSingleModelWrapper(model)
    results = wrapper(file_dir, variable_list, sat_time, sat_height, sat_lat, 
                      sat_lon, z_dependence=z_dependence, dz=dz) 

    return results  #not sure that than C++ can take more than one return variable

def ModelVariables(model=''):
    '''Give users an option to see what variables are available from any model'''
    
    if model == '':  #Give a list of possible values
        print("Possible models are: 'CTIPe' and 'IRI'") #, 'GITM'
        return
    
    #choose the model-specific function to retrieve the variables
    if model == 'CTIPe':  #need to add these as part of kamodo
        from CTIPe_wrapper import CTIPeVariables as Var
        
    elif model == 'GITM':  #later: from kamodo.readers.GITM_wrapper import ...
        from GITM_wrapper import GITMVariables as Var
        
    elif model == 'IRI':
        from IRI_wrapper import IRIVariables as Var
    
    #retrieve and print model specific and standardized variable names
    variable_list = Var()
    print('\nThe functions accept the standardized variable names listed below.')
    print('Units for the chosen variables are printed during the satellite flythrough if available.')
    print(f'Possible variables for {model} model (description = standard variable name):')
    print('-----------------------------------------------------------------------------------')
    for key, value in variable_list.items(): print(f"{key} : '{value}'")
    print()
    return     

