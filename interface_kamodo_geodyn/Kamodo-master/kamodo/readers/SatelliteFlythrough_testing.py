# -*- coding: utf-8 -*-
"""
Created on Wed Apr  7 14:57:55 2021
author: rringuet

Code to be called from other languages. Retrieves satellite trajectory from HAPI,
executes flythrough of chosen model data.
All desired model data should be in a single directory.
"""
import os
import numpy as np
#from kamodo.readers.hapi import HAPI
from kamodo.readers import FlythroughPlots as FPlot

def SatelliteTrajectory(server, dataset, parameters, start, stop):
    '''Retrieve and return satellite trajectory from HAPI/CDAWeb'''
    
    print(server, dataset, parameters, start, stop)  #for user confirmation
    '''#for use after hapi/cdaweb works
    sat_client = 'hapi'  #or cdaweb
    start = '2012-07-07T00:00:00'
    stop  = '2012-07-08T00:00:00'
    
    #example parameters to get data from hapi client
    server     = 'http://hapi-server.org/servers/SSCWeb/hapi'
    dataset    = 'grace1'
    parameters = 'X_GEO,Y_GEO,Z_GEO,X_GSE,Y_GSE,Z_GSE'
    
    #example parameters to get data from cda web
    server = 'https://cdaweb.gsfc.nasa.gov/hapi'
    dataset = 'GOES12_K0_MAG'
    parameters = 'B_GSE_c,SC_pos_eo,SC_pos_se'
    
    #retrieve satellite trajectory
    hapi = HAPI(server, dataset, parameters, start, stop)
    
    #need to separate time, lat, lon, and altitude    
    #need to convert altitude to meters, possibly other coordinates to proper units too
    #this will likely involve additional functions, possibly geodyn?
    satellite_dict = {'sat_time': sat_time, 'sat_height': sat_height,
                'sat_lat': sat_lat, 'sat_lon': sat_lon}  #store in dict
    
    #add requested variables to dictionary, print out units and names of these extra variables
    '''
    return satellite_dict

def SampleTrajectory(start_time, stop_time, plot_dir='', max_lat=65., min_lat=-65.,
                     lon_perorbit=363., max_height=450000., min_height=400000., 
                     p=0.01, n=2., plots=False, plot_close=True, plot_sampling=5000):
    '''
    Given start and stop times in timestamp form, return a test satellite trajectory.
    Parameters:
        start_time = timestamp in seconds since 1970-01-01 when trajectory begins.
        stop_time = timestamp in seconds since 1970-01-01 when trajectory ends.
        plot_dir = location and where trajectory plot will be saved, including
            final '/' (default='' to not save a plot).
        max_lat = the highest latitude desired in degrees (default=65.).
        min_lat = the lowest latitude desired in degrees  (default=-65.).
        lon_perorbit = the number of longitude degrees covered in 90 min. (default=363. to precess)
        max_height = the highest altitude above the surface in meters for the first orbit (default=450000.).
        min_height = the lowest altitude above the surface in meters for the first orbit (default=400000.).
        p = a rough precession variable, applied as an overall height decrease 
            as a percentage of the min_height value (default=0.01).
        n = desired number of seconds between timestamps (default=2.).
        plots = option to make plots of whole flythrough (default=True).
        plot_close = whether to close plot if generated (default=True).
        plot_sampling = max number of points to include in 3D plot (default=5000).
    Returns a dictionary with keys: sat_time, sat_height, sat_lat, and sat_lon.
        sat_time is an array in seconds since 1970-01-01.
        sat_height is an array in meters.
        sat_lat and sat_lon are arrays in degrees.
    '''
    
    #determine basic parameters
    orbit_seconds = int(90.*60./float(n))  #determine number of samples per 90min orbit
    n_orbits = (stop_time-start_time)/float(orbit_seconds*n) #orbits of 90 min each
    h_scale, h_offset = (max_height-min_height)/2., np.mean([max_height,min_height])
    lat_scale, lat_offset = (max_lat-min_lat)/2., np.mean([max_lat,min_lat])
    time_left = (stop_time-start_time)/float(n)-int(n_orbits)*orbit_seconds
    
    #create orbital tracks 
    pi_arr = np.linspace(0.,2.*np.pi,orbit_seconds)  
    lat, height = np.tile(np.cos(pi_arr), int(n_orbits)), np.tile(np.sin(pi_arr), int(n_orbits))
    if time_left>0:
        lat = np.append(lat, np.cos(pi_arr[0:int(time_left)]))  #add partial last orbit
        height = np.append(height, np.sin(pi_arr[0:int(time_left)]))
    lon = np.linspace(0.,float(lon_perorbit)*n_orbits,int(n_orbits*orbit_seconds))
    while max(lon)>360.: lon[np.where(lon>360.)[0]]-=360.
    while max(lon)<0.: lon[np.where(lon<0.)[0]]+=360.
    height = height*h_scale+h_offset-np.linspace(0.,p,int(n_orbits*orbit_seconds))*min_height
    
    #store results in dictionary to return
    sample_dict={'sat_time': np.linspace(start_time,stop_time,int(n_orbits*orbit_seconds)),
                 'sat_lon': lon, 'sat_height': height, 'sat_lat': lat*lat_scale+lat_offset}   
    
    #generate plot if desired
    if plot_dir != '': 
        if not os.path.isdir(plot_dir+'Plots/'): os.mkdir(plot_dir+'Plots/')
        FPlot.Plot4D('Time', sample_dict['sat_time'], sample_dict['sat_lat'], 
                     sample_dict['sat_lon'], sample_dict['sat_height'],
                     sample_dict['sat_time'], 's', plot_dir+'Plots/SampleTrajectory',
                     'km', plot_close=plot_close, plot_sampling=plot_sampling)
    
    print(f'Attribute/Key names of return dictionary: {sample_dict.keys()}')
    print('Units are given in the function description. Type: help(SampleTrajectory)')    
    return sample_dict

def _ChooseModelWrapper(model):
    '''choose and return proper model wrapper'''
    
    #as other flythrough codes are written, add more options here
    if model == 'CTIPe':  #need to add these as part of kamodo
        from kamodo.readers.CTIPe_wrapper import CTIPe_SatelliteFlythrough as SF
    elif model == 'GITM':  #later: from kamodo.readers.GITM_wrapper import ...
        from GITM_wrapper import GITM_SatelliteFlythrough as SF
    elif model == 'IRI':
        from kamodo.readers.IRI_wrapper import IRI_SatelliteFlythrough as SF
        
    return SF

#want to enable call of this from C++ for flexibility, so return only one value
#keep so users can call this if they have their own satellite trajectory data
def ModelFlythrough(model, file_dir, variable_list, sat_time, sat_height, 
                                 sat_lat, sat_lon, plots=False, daily_plots=False, 
                                 plot_close=True, plot_sampling=5000, verbose=False):  
    '''Call satellite flythrough wrapper specific to the model chosen.
    Parameters:    
        Name of model: model (Options: 'CTIPe', ...)
        Absolute path to where model data is stored: file_dir
        List of desired standardized variable names: variable_list
        Array of satellite trajectory timestamps: sat_time
            (in number of seconds since 1970-01-01)
        Array of satellite trajectory heights in meters: sat_height
        Array of satellite trajectory latitudes in degrees: sat_lat
        Array of satellite trajectory longitudes in degrees: sat_lon    
        Option to make plots of whole flythrough: plots (default=False)
        Option to make plots of daily sections: daily_plots (default=False)
        Option to close plots whole flythrough plots: plot_close (default=True)
        Max number of points to include in 2D/3D plots: plot_sampling (default=5000)
    Returns a dictionary with keys: sat_time, sat_height, sat_lat, sat_lon, net_idx,
    and keys naming the requested variables.
        sat_time is an array in seconds since 1970-01-01.
        sat_height is an array in meters.
        sat_lat and sat_lon are arrays in degrees.
        model variable keys are returned in the units printed out.
    ''' 

    #if input types are lists, correct to be numpy arrays (important for calling from C++)
    if isinstance(sat_time, list): sat_time = np.array(sat_time)
    if isinstance(sat_height, list): sat_height = np.array(sat_height)
    if isinstance(sat_lat, list): sat_lat = np.array(sat_lat)
    if isinstance(sat_lon, list): sat_lon = np.array(sat_lon)
    
    wrapper = _ChooseModelWrapper(model)
    results, results_units = wrapper(file_dir, variable_list, sat_time, sat_height, 
                                 sat_lat, sat_lon, plots=plots, daily_plots=daily_plots, 
                                 plot_close=plot_close, plot_sampling=plot_sampling,
                                 verbose=verbose)  #make this the call for all?
    if verbose: 
        print(f'Units from the {model} model by variable name:\n{results_units}')
        print(f'Dictionary key names in results:\n{results.keys()}')
        print('The units of the trajectory variables are unchanged from the inputs.')
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

def RealFlight(server, dataset, parameters, start, stop, model, file_dir, 
                 variable_list, time_offset=0., plots=True, daily_plots=False, 
                 plot_close=True, plot_sampling=5000, verbose=False):
    '''
    Master function that executes all functions of flythrough for real trajectory. 
    Parameters:
    HAPI/CDAWeb parameter: server
    HAPI/CDAWeb parameter: dataset
    HAPI/CDAWeb parameter: parameters
    HAPI/CDAWeb parameter (timestamp for now): start
    HAPI/CDAWeb parameter (timestamp for now): stop
    Location where model data is stored: file_dir
    List of standardized variable names: variable_list
    Offset between model time and trajectory time: time_offset (default=0.)
    Name of model: model (Options: 'CTIPe', ...)
    Option to make plots of whole flythrough: plots (default=True)
    Option to make plots of daily sections: daily_plots (default=False)
    Option to close plots whole flythrough plots: plot_close (default=True)
    Max number of points to include in 2D/3D plots: plot_sampling (default=5000)
    Number of seconds between satellite data for sample trajectory: n (default=2)
    '''
    #retrieve satellite trajectory from HAPI/CDAWeb
    sat_dict = SatelliteTrajectory(server, dataset, parameters, start, stop)
    #call satellite flythrough code
    results = ModelFlythrough(model, file_dir, variable_list, sat_dict['sat_time'], 
                              sat_dict['sat_height'], sat_dict['sat_lat'], sat_dict['sat_lon'], 
                              plots=plots, daily_plots=daily_plots, 
                              plot_close=plot_close, plot_sampling=plot_sampling)
    
    #add new data to sat_dict and return
    for key in results.keys():
        if key not in sat_dict.keys(): sat_dict[key] = results[key]
    print(f'Attribute/Key names of return dictionary:\n{sat_dict.keys()}')
    return sat_dict    

def FakeFlight(start_time, stop_time, model, file_dir, variable_list, max_lat=65., 
               min_lat=-65., lon_perorbit=363., max_height=450000., min_height=400000., 
               p=0.01, n=2., plots=True, daily_plots=False, plot_close=True, 
               plot_sampling=5000, trajplot_close=True, verbose=False):
    '''
    Master function that executes all functions of flythrough for sample trajectory. 
    Parameters:    
        Timestamp for start of trajectory (number of seconds since 1970-01-01): start
        Timestamp for stop of trajectory (number of seconds since 1970-01-01): stop
        Name of model: model (Options: 'CTIPe', ...)
        Location where model data is stored: file_dir
        List of standardized variable names: variable_list
        The highest desired latitude in degrees: max_lat (default=65.)
        The highest desired longitude in degrees: min_lat (default=-65.)
        The number of longitude degrees covered in 90 min.: lon_perorbit 
            (default=363. to precess)
        The highest altitude above the surface in meters for the first orbit:
            max_height (default=450000.)
        The lowest altitude above the surface in meters for the first orbit:    
            min_height (default=400000.)
        A rough precession variable, applied as an overall height decrease 
            as a percentage of the min_height value: p =  (default=0.01).  
        The desired number of seconds between timestamps: n (default=2.)        
        Option to make plots of whole flythrough: plost (default=True)
            (both sample trajectory and model interpolation results)
        Option to make plots of daily sections: daily_plots (default=False)
        Option to close plots whole flythrough plots: plot_close (default=True)
        Max number of points to include in 2D/3D plots: plot_sampling (Default=5000)
        Number of seconds between satellite data for sample trajectory: n (default=2)
        trajplot_close = whether to close trajectory plot if generated (default=True)
    Returns a dictionary with keys: sat_time, sat_height, sat_lat, sat_lon, net_idx,
    and keys naming the requested variables (e.g. 'T', 'T_n', etc.).
        sat_time is an array in seconds since 1970-01-01.
        sat_height is an array in meters.
        sat_lat and sat_lon are arrays in degrees.
        Model variable data are returned as arrays in the units printed out.
    ''' 
    #for testing with CTIPe: start=1426660000.0, stop=1426880700.0, model='CTIPe'
    #variable_list = ['rho', 'T', 'T_e', 'T_i', 'H', 'Vn_lat', 'Vn_lon', 'Vn_H', 
    #                 'T_n', 'Rmt', 'N_e', 'N_n', 'Q_Solar', 'Q_Joule', 'Q_radiation', 
    #                 'N_O', 'N_O2', 'N_N2', 'N_NO', 'N_NOplus', 'N_N2plus', 'N_O2plus', 
    #                 'N_Nplus', 'N_Oplus', 'N_Hplus', 'sigma_P', 'sigma_H', 'Vi_lon', 
    #                 'Vi_lat', 'W_Joule', 'Eflux_precip', 'Eavg_precip', 'TEC', 
    #                 'E_theta140km', 'E_lambda140km', 'E_theta300km', 'E_lambda300km']
    #for testing with IRI: start=1495929960., stop=1496014100., model='IRI'
    #variable_list = ['N_n','T_e','T_i','T_n','N_Oplus','N_Hplus','N_Heplus','N_O2plus',
    #                 'N_NOplus','N_Nplus','NmF2','HmF2']

    '''#print input parameters for C++ testing
    print(start_time, stop_time, model, file_dir, variable_list, max_lat, min_lat,
          lon_perorbit, max_height, min_height, p, n, plots, daily_plots, plot_close,
          plot_sampling, trajplot_close)
    '''
    
    #generate a sample satellite trajectory
    sat_dict = SampleTrajectory(start_time, stop_time, plot_dir=file_dir,
                                max_lat=max_lat, min_lat=min_lat, lon_perorbit=lon_perorbit, 
                                max_height=max_height, min_height=min_height, p=p, n=n,  
                                plots=plots, plot_close=trajplot_close, plot_sampling=plot_sampling)
    
    #call satellite flythrough code
    results = ModelFlythrough(model, file_dir, variable_list, sat_dict['sat_time'], 
                              sat_dict['sat_height'], sat_dict['sat_lat'], sat_dict['sat_lon'],
                              plots=plots, daily_plots=daily_plots, plot_close=plot_close, 
                              plot_sampling=plot_sampling, verbose=verbose)    
    return results    
