# -*- coding: utf-8 -*-
"""
Created on Tue Mar 30 15:21:50 2021

@author: rringuet
"""
#coding imports
import numpy as np
from kamodo.readers.iri_faster import IRI, iri_varnames
import kamodo.readers.wrapper_utilities as U
 
#need input times to be timestamps since 1970 to pull data from the correct files
   
def IRIVariables():
    '''return a list of all the possible variables in IRI'''
    
    return iri_varnames

def IRI_FlyAway(filename, variable_list, sat_time, sat_height, sat_lat, sat_lon, 
                  plot_sampling=4000, plot_file='', verbose=False):
    '''fly satellite through IRI model data, per file'''
    
    #Check that sat data is all the same length
    if max(np.diff(np.array([len(sat_time),len(sat_height),len(sat_lat),len(sat_lon)])))>0:
        raise AttributeError (f'Satellite time, height, latitude, and longitude\
                              arrays or lists must all be the same length.\
                              Current array lengths are {len(sat_time)}, {len(sat_height)},\
                               {len(sat_lat)}, and {len(sat_lon)}')
    
    
    #create iri kamodo object, initialize some variables
    iri = IRI(filename, variables_requested=variable_list, printfiles=verbose)
    filedate = iri.timerange['max'][0:10]
    var_test = {'0':[], '1':[]}
    for var in variable_list:  #determine which variables require ilev(2), height(1), neither(0)
        input_var_list = iri.variables[var]['xvec']
        if 'height' in input_var_list: var_test['1'].append(var)
        else: var_test['0'].append(var)
    
    #Create satellite tracks with appropriate inputs
    sat_track = {}  #initialize list of satellite tracks
    if len(var_test['0'])>0:
        if verbose: print('Building height-independent satellite track.')
        sat_track['0']=[[t, sat_lat, sat_lon] for t, sat_lat, sat_lon in zip(sat_time,sat_lat,sat_lon)]
    if len(var_test['1'])>0:  #if function requires height (in km)
        if verbose: print('Building height-dependent satellite track (km).')
        sat_track['1']=[[t, h/1000., lat, lon] for t, h, lat, lon in zip(sat_time,sat_height,
                                                                     sat_lat,sat_lon)]

    #retrieve interpolator and interpolate data for each variable. Time it just for fun.
    results = {}
    for var in variable_list:
        #choose correct satellite track and interpolate data
        idx = [key for key, value in var_test.items() if var in value][0]
        intp = iri[var]   #gridded interpolator way too slow (the one with _ijk in name)
        results[var] = intp(sat_track[idx])
        results_units = intp.meta['units']  #give entire track of correct type 
    
        #make correct set of prints depending on the dimensionality of the function
        if plot_file != '':       
            U.make_daily_plots(var, idx, U.hrs_to_ts(sat_time, filedate), 
                               sat_lat, sat_lon, results[var], results_units, 
                               plot_file, plot_sampling, sat_height=sat_height)
        
    if verbose: print(f'Done for {filename}\n')
    return results

def IRI_SatelliteFlythrough(file_dir, variable_list, sat_time, sat_height, sat_lat, 
                            sat_lon, plots=False, daily_plots=False, plot_close=True, 
                            plot_sampling=4000, verbose=False):
    '''
    Execute flythrough for IRI model data. Returns results_dict, results_units.
    results_dict is a dictionary of the interpolated data for the entire data set
        sorted by variable name.
    results_units is a dictionary of the units from the model for each variable.
    file_dir is a string indicating where the data files are located.
    variable_list is a list of strings of the desired variables. Choose from 
        ['rho', 'T', 'T_e', 'T_i', 'H', 'Vn_lat', 'Vn_lon', 'Vn_H', 
         'T_n', 'Rmt', 'N_e', 'N_n', 'Q_Solar', 'Q_Joule', 'Q_radiation', 
         'N_O', 'N_O2', 'N_N2', 'N_NO', 'N_NOplus', 'N_N2plus', 'N_O2plus', 
         'N_Nplus', 'N_Oplus', 'N_Hplus', 'sigma_P', 'sigma_H', 'Vi_lon', 
         'Vi_lat', 'W_Joule', 'Eflux_precip', 'Eavg_precip', 'TEC', 
         'E_theta140km', 'E_lambda140km', 'E_theta300km', 'E_lambda300km'].
    sat_time is an array of timestamp values.
    sat_height is an array of heights in meters.
    sat_lat and sat_lon ar arrays of latitudes and longitudes in degrees.
    Set plots=True to get plots of the entire data set for each variable.
    Set daily_plots=True to get plots of the data set for each variable and each file.
    Set plot_close=False to keep plots of the entire data set open.
    '''
    
    #retrieve time ranges for each file in directory 
    #e.g. times['2015-03-20'] = [filename, '2015-03-19 20:15:00','2015-03-20 20:00:00'',
    #                            1426810500.0, 1426896000.0]  last two values are timestamps in UTC    
    sat_time, times, net_idx = U.save_times(file_dir+'IRI.3D.*.nc', sat_time, IRI, 
                                            dt=450., verbose=verbose)    
        
    #interpolate requested data for each file
    if verbose: print('\nInterpolating data for each file.')
    if daily_plots:  #interpolate data using idx list from before and make daily plots
        U.check_plot_dir(file_dir+'Plots/')  #create proper directory if DNE
        list_results = [IRI_FlyAway(times[file_date][0], variable_list, 
                                      U.ts_to_hrs(sat_time[times[file_date][5]], file_date),
                                      sat_height[times[file_date][5]], sat_lat[times[file_date][5]], 
                                      sat_lon[times[file_date][5]], plot_sampling=plot_sampling,
                                      plot_file=file_dir+'Plots/'+file_date+'-', verbose=verbose) \
                        for file_date in times.keys() if len(sat_time[times[file_date][5]])>0]
    else:  #interpolate data using idx list from before without making daily plots
        list_results = [IRI_FlyAway(times[file_date][0], variable_list, 
                                      U.ts_to_hrs(sat_time[times[file_date][5]], file_date),
                                      sat_height[times[file_date][5]], sat_lat[times[file_date][5]], 
                                      sat_lon[times[file_date][5]], plot_file='', verbose=verbose) \
                        for file_date in times.keys() if len(sat_time[times[file_date][5]])>0]
            
    #determine units for all variables
    results_units = {value[0]:value[-1] for key, value in iri_varnames.items() \
                     if value[0] in variable_list}    
    
    #combine idx lists for plotting, collect filtered trajectory
    results_dict = {'sat_time': sat_time[net_idx], 'sat_height': sat_height[net_idx],
                    'sat_lat': sat_lat[net_idx], 'sat_lon': sat_lon[net_idx],
                    'net_idx': net_idx}  #index for comparison with other data from real satellite
    
    #collect interpolated data into the same dictionary
    varlist_4d = ['N_e', 'T_e', 'T_i', 'T_n', 'N_Oplus', 'N_Hplus', 'N_Heplus',
       'N_O2plus', 'N_NOplus', 'N_Nplus']
    varlist_3d = ['TEC', 'NmF2', 'HmF2']
    for var in variable_list:  #sort and combine arrays for the same variable
        results_dict[var] = np.concatenate(tuple([results[var] for results in list_results]))
        if plots:  #make a net plot per variable if desired, based function dependencies (height or no)
            U.make_net_plots(var, results_dict, results_units, varlist_4d, varlist_3d, file_dir,
              plot_close, plot_sampling)
        
    
    return results_dict, results_units

if __name__=='__main__':
    ''' Begin program '''
    #initialize input parameters (filename, variable_name)
    file_dir = 'C:/Users/rringuet/Kamodo_WinDev1/IRI/'
    variable_list = ['T_e','TEC']  #Test ilev with N_n, without ilev with T_e, 3D with TEC
    
    #generate a fake satellite track    
    from kamodo.readers.SatelliteFlythrough import SampleTrajectory as ST
    traj_dict = ST(1495945560.0-30000., 1496014100.0, n=1)
    
    results_dict, results_units = IRI_SatelliteFlythrough(file_dir, variable_list, 
                                    traj_dict['sat_time'], traj_dict['sat_height'], 
                                    traj_dict['sat_lat'], traj_dict['sat_lon'], 
                                    plots=True, daily_plots=True, plot_close=True,
                                    verbose=False)
