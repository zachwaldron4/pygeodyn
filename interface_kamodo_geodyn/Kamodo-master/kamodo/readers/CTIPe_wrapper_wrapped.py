# -*- coding: utf-8 -*-
"""
Created on Tue Mar 30 15:21:50 2021

@author: rringuet
"""
#coding imports
import numpy as np
import time as ti
from kamodo.readers.ctipe_faster_wrapped import CTIPe, ctipe_varnames
import kamodo.readers.wrapper_utilities as U
 
#need input times to be timestamps since 1970 to pull data from the correct files
   
'''
#speed test for CalcIlev section, for two step process, one satellite position at a time
#outputs n_stepd, bounds of height as delta_h, bounds of ilev for delta_ilev, ilev, 
#    and processing time
t, height, lat, lon = sat_track[0]
sample_ilev = np.linspace(1,15,15,dtype=float)   #global variable
for i in [10,50,100,250,500]:
    start = ti.time()
    rough_height = np.array([ctipe.H([t, ilev, lat, lon])[0] for ilev in sample_ilev])
    ilev_range = np.sort(sample_ilev[np.argsort(abs(height-rough_height))[0:2]])
    test_ilev = np.linspace(ilev_range[0],ilev_range[1],i,dtype=float)
    finer_height = np.array([ctipe.H([t, ilev, lat, lon])[0] for ilev in test_ilev])
    ilev_idx = np.argmin(abs(height-finer_height))
    print(i, finer_height[ilev_idx+1]-finer_height[ilev_idx-1], 
          test_ilev[ilev_idx+1]-test_ilev[ilev_idx-1], test_ilev[ilev_idx],ti.time()-start)

OUTPUT: (i, delta_h, delta_ilev, ilev, calc_time)
10 10061.973765432136 0.2222222222222232 12.777777777777779 0.01804208755493164
50 1848.1176303854445 0.040816326530611846 12.775510204081632 0.04254770278930664
100 914.7248877665261 0.020202020202019 12.787878787878787 0.07263469696044922
250 363.68579875055 0.008032128514056325 12.783132530120483 0.20418334007263184
500 181.47848474729108 0.004008016032063466 12.783567134268537 0.3631880283355713
-> Too slow to execute repeatedly
   

#speed test with one step process, same outputs
t, height, lat, lon = sat_track[0]
for i in [10,50,100,500,1000,5000,10000]:
    test_ilev = np.linspace(1,15,n,dtype=float)  #global variable each time
    start = ti.time()
    test_track = np.array([[t, ilev, lat, lon] for ilev in test_ilev])
    finer_height = ctipe.H(test_track)
    ilev_idx = np.argmin(abs(height-finer_height))
    print(i, finer_height[ilev_idx+1]-finer_height[ilev_idx-1], 
          test_ilev[ilev_idx+1]-test_ilev[ilev_idx-1], test_ilev[ilev_idx],ti.time()-start)
    
OUTPUT: (i, delta_h, delta_ilev, ilev, calc_time)
10 152055.1091820988 3.1111111111111107 13.444444444444445 0.0
50 25873.64682539692 0.571428571428573 12.714285714285714 0.0025177001953125
100 12806.148428731773 0.28282828282828376 12.737373737373737 0.0010514259338378906
500 2540.6987864619005 0.056112224448899184 12.783567134268537 0.0
1000 1269.0777722166968 0.02802802802802873 12.785785785785786 0.006573677062988281
5000 253.6124613811844 0.005601120224044465 12.784756951390278 0.002038717269897461
10000 126.79354879941093 0.0028002800280031437 12.783578357835783 0.010225057601928711  *chosen method
-> Not accurate to within 200 km until i=10000, but better speed than 1st option


#faster 2-step process?
t, height, lat, lon = sat_track[0]
sample_ilev = np.linspace(1,15,15,dtype=float)   #global variable
for i in [10,50,100,250,500,1000,5000,10000]:
    start = ti.time()
    rough_track = np.array([[t, ilev, lat, lon] for ilev in sample_ilev])
    rough_height = ctipe.H(rough_track)
    ilev_range = np.sort(sample_ilev[np.argsort(abs(height-rough_height))[0:2]])
    test_ilev = np.linspace(ilev_range[0],ilev_range[1],i,dtype=float)
    finer_track = np.array([[t, ilev, lat, lon] for ilev in test_ilev])
    finer_height = ctipe.H(finer_track)
    ilev_idx = np.argmin(abs(height-finer_height))
    print(i, finer_height[ilev_idx+1]-finer_height[ilev_idx-1], 
          test_ilev[ilev_idx+1]-test_ilev[ilev_idx-1], test_ilev[ilev_idx],ti.time()-start)
    
OUTPUT: (i, delta_h, delta_ilev, ilev, calc_time)
10 10061.973765432136 0.2222222222222232 12.777777777777779 0.0
50 1848.1176303854445 0.040816326530611846 12.775510204081632 0.0
100 914.7248877665261 0.020202020202019 12.787878787878787 0.0
250 363.68579875055 0.008032128514056325 12.783132530120483 0.015614032745361328
500 181.47848474729108 0.004008016032063466 12.783567134268537 0.0   
1000 90.64841230114689 0.0020020020020012907 12.783783783783784 0.0
5000 18.115175812970847 0.0004000800160035567 12.783956791358271 0.0
10000 9.056682057096623 0.00020002000199959014 12.783978397839784 0.015622377395629883

-> Accurate to within 200 km for i=500, and too fast to time it. Best option out of first 3.
Freezes when including i values above 500. Not sure why. Gives answer if I hit enter a few times.

-> Comparing method 2 (i=10000) and 3 (i=500) in execution showed a large time difference.
Method 2 took about 150 seconds while method 3 took about 16 seconds. Choosing method 3.

#Method 2
sample_ilev = np.linspace(1,15,10000, dtype=float)
def CalcIlev2(H, t, height, lat, lon):
    
    finer_height = H(np.array([[t, ilev, lat, lon] for ilev in sample_ilev]))
    ilev_idx = np.argmin(abs(height-finer_height))
    return sample_ilev[ilev_idx] 
'''
def CTIPeVariables():
    '''return a list of all the possible variables in CTIPe'''
    
    return ctipe_varnames

sample_ilev = np.linspace(1,15,75,dtype=float)   #global variable
def CalcIlev(H, t, height, lat, lon):
    '''Approximate ilev by inverting the gridded height function CTIPe.H for one sat point'''
    
    rough_height = H(np.array([[t, ilev, lat, lon] for ilev in sample_ilev]))
    ilev_range = np.sort(sample_ilev[np.argsort(abs(height-rough_height))[0:2]])
    test_ilev = np.linspace(ilev_range[0],ilev_range[1],100,dtype=float)
    finer_height = H(np.array([[t, ilev, lat, lon] for ilev in test_ilev]))
    return test_ilev[np.argmin(abs(height-finer_height))]

def CTIPe_FlyAway(filename, variable_list, sat_time, sat_height, sat_lat, sat_lon, 
                  plot_sampling=4000, plot_file='', verbose=False):
    '''fly satellite through CTIPe model data, per file'''
    
    #Check that sat data is all the same length
    if max(np.diff(np.array([len(sat_time),len(sat_height),len(sat_lat),len(sat_lon)])))>0:
        raise AttributeError (f'Satellite time, height, latitude, and longitude\
                              arrays or lists must all be the same length.\
                              Current array lengths are {len(sat_time)}, {len(sat_height)},\
                               {len(sat_lat)}, and {len(sat_lon)}')
    
    #create ctipe kamodo object, initialize some variables
    ctipe = CTIPe(filename, variables_requested=variable_list, printfiles=verbose)
    filedate = ctipe.timerange['max'][0:10]
    var_test = {'0':[], '1':[], '2':[]}
    if 'H' in variable_list: variable_list.remove('H')  #H only needed if other functions require ilev
    for var in variable_list:  #determine which variables require ilev(2), height(1), neither(0)
        input_var_list = ctipe.variables[var]['xvec']
        if 'ilev' in input_var_list.keys(): var_test['2'].append(var)
        elif 'height' in input_var_list.keys(): var_test['1'].append(var)
        else: var_test['0'].append(var)
    
    #Create satellite tracks with appropriate inputs
    sat_track = {}  #initialize list of satellite tracks
    if len(var_test['0'])>0:
        if verbose: print('Building height-independent satellite track.')
        sat_track['0']=[[t, sat_lat, sat_lon] for t, sat_lat, sat_lon in zip(sat_time,sat_lat,sat_lon)]
        sat_ilev=0
    if len(var_test['1'])>0:  #if function requires height (in km)
        if verbose: print('Building height-dependent satellite track (km).')
        sat_track['1']=[[t, h/1000., lat, lon] for t, h, lat, lon in zip(sat_time,sat_height,
                                                                     sat_lat,sat_lon)]
        sat_ilev=0
    if len(var_test['2'])>0:  #if ilev is required for at least one variable
        if verbose: print('Converting height to ilev and building ilev-dependent satellite track.')
        start = ti.time()  #The H function outputs in m, so input h in meters
        sat_track0 = [[t, h, lat, lon] for t, h, lat, lon in zip(sat_time,sat_height,
                                                                     sat_lat,sat_lon)]
        sat_ilev = np.array([CalcIlev(ctipe.H, *sat_position) for 
                               sat_position in sat_track0])  #try adding a parallel idea here?
        sat_track['2']=[[t, ilev, lat, lon] for t, ilev, lat, lon in zip(sat_time,
                                                    sat_ilev,sat_lat,sat_lon)]
        if verbose: print(f'Conversion took {ti.time()-start} s for {len(sat_time)} positions.')

    #retrieve interpolator and interpolate data for each variable. 
    results = {}
    for var in variable_list:
        #choose correct satellite track and interpolate data
        idx = [key for key, value in var_test.items() if var in value][0]
        intp = ctipe[var]   #gridded interpolator way too slow (the one with _ijk in name)
        results[var] = intp(sat_track[idx])
        results_units = intp.meta['units']  #give entire track of correct type 
    
        #make correct set of prints depending on the dimensionality of the function
        if plot_file != '':       
            U.make_daily_plots(var, idx, U.hrs_to_ts(sat_time, filedate), 
                               sat_lat, sat_lon, results[var], results_units, 
                               plot_file, plot_sampling, sat_height=sat_height, 
                               sat_ilev=sat_ilev)
        
    if verbose: print(f'Done for {filename}\n')
    return results

def CTIPe_SatelliteFlythrough(file_dir, variable_list, sat_time, sat_height, sat_lat, 
                              sat_lon, plots=False, daily_plots=False, plot_close=True, 
                              plot_sampling=4000, verbose=False):
    '''
    Execute flythrough for CTIPe model data. Returns results_dict, results_units.
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
    sat_time, times, net_idx = U.save_times(file_dir+'*-plot-density-wrapped.nc', sat_time, CTIPe, 
                                            dt=450., verbose=verbose)
        
    #interpolate requested data for each file
    if verbose: print('\nInterpolating data for each file.')
    if daily_plots:  #interpolate data using idx list from before and make daily plots
        U.check_plot_dir(file_dir+'Plots/')  #create proper directory if DNE
        list_results = [CTIPe_FlyAway(times[file_date][0], variable_list, 
                                      U.ts_to_hrs(sat_time[times[file_date][5]], file_date),
                                      sat_height[times[file_date][5]], sat_lat[times[file_date][5]], 
                                      sat_lon[times[file_date][5]], plot_sampling=plot_sampling,
                                      plot_file=file_dir+'Plots/'+file_date+'-',
                                      verbose=verbose) \
                        for file_date in times.keys() if len(sat_time[times[file_date][5]])>0]
    else:  #interpolate data using idx list from before without making daily plots
        list_results = [CTIPe_FlyAway(times[file_date][0], variable_list, 
                                      U.ts_to_hrs(sat_time[times[file_date][5]], file_date),
                                      sat_height[times[file_date][5]], sat_lat[times[file_date][5]], 
                                      sat_lon[times[file_date][5]], plot_file='',
                                      verbose=verbose) \
                        for file_date in times.keys() if len(sat_time[times[file_date][5]])>0]
            
    #determine units for all variables
    results_units = {value[0]:value[-1] for key, value in ctipe_varnames.items() \
                     if value[0] in variable_list}
    
    #combine idx lists for plotting, collect filtered trajectory
    #net_idx = np.concatenate(tuple([times[file_date][5] for file_date in times.keys()]))
    results_dict = {'sat_time': sat_time[net_idx], 'sat_height': sat_height[net_idx],
                    'sat_lat': sat_lat[net_idx], 'sat_lon': sat_lon[net_idx],
                    'net_idx': net_idx}  #index for comparison with other data from real satellite
    
    #collect interpolated data into the same dictionary
    varlist_4d = ['rho', 'T', 'T_e', 'T_i', 'H', 'Vn_lat', 'Vn_lon', 'Vn_H', 'T_n',
       'Rmt', 'N_e', 'N_n', 'Q_Solar', 'Q_Joule', 'Q_radiation', 'N_O',
       'N_O2', 'N_N2', 'N_NO', 'N_NOplus', 'N_N2plus', 'N_O2plus',
       'N_Nplus', 'N_Oplus', 'N_Hplus', 'Sigma_P', 'Sigma_H', 'Vi_lon',
       'Vi_lat']
    varlist_3d = ['W_Joule', 'Eflux_precip', 'Eavg_precip', 'TEC', 'E_theta140km',
       'E_lambda140km', 'E_theta300km', 'E_lambda300km']
    for var in variable_list:  #sort and combine arrays for the same variable
        results_dict[var] = np.concatenate(tuple([results[var] for results in list_results]))
        if plots:  #make a net plot per variable if desired, based function dependencies (height or no)
            U.make_net_plots(var, results_dict, results_units, varlist_4d, varlist_3d, file_dir,
              plot_close, plot_sampling)
    
    return results_dict, results_units

if __name__=='__main__':
    ''' Begin program '''
    #initialize input parameters (filename, variable_name)
    file_dir = 'C:/Users/rringuet/Kamodo_WinDev1/CTIPe/'
    variable_list = ['T_e']  #Test ilev with N_n, without ilev with T_e, 3D with TEC
    #variable_list = ['rho', 'T', 'T_e', 'T_i', 'H', 'Vn_lat', 'Vn_lon', 'Vn_H', 
    #                 'T_n', 'Rmt', 'N_e', 'N_n', 'Q_Solar', 'Q_Joule', 'Q_radiation', 
    #                 'N_O', 'N_O2', 'N_N2', 'N_NO', 'N_NOplus', 'N_N2plus', 'N_O2plus', 
    #                 'N_Nplus', 'N_Oplus', 'N_Hplus', 'sigma_P', 'sigma_H', 'Vi_lon', 
    #                 'Vi_lat', 'W_Joule', 'Eflux_precip', 'Eavg_precip', 'TEC', 
    #                 'E_theta140km', 'E_lambda140km', 'E_theta300km', 'E_lambda300km']
    
    #generate a fake satellite track    
    from kamodo.readers.SatelliteFlythrough import SampleTrajectory as ST
    traj_dict = ST(1426660000.0-30000., 1426791280.0)
    
    tic = ti.perf_counter()
    results_dict, results_units = CTIPe_SatelliteFlythrough(file_dir, variable_list, 
                                    traj_dict['sat_time'], traj_dict['sat_height'], 
                                    traj_dict['sat_lat'], traj_dict['sat_lon'], 
                                    plots=True, daily_plots=True, plot_close=True,
                                    verbose=False)
    print(ti.perf_counter()-tic, 's', len(traj_dict['sat_time']))