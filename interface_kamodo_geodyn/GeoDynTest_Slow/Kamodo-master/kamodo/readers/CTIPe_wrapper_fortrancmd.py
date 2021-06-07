# -*- coding: utf-8 -*-
"""
Created on Tue Mar 30 15:21:50 2021
@author: rringuet
Adapted for speed to execute for a single point 
"""
#coding imports
import numpy as np
import glob, sys
from datetime import datetime, timedelta, timezone
#from kamodo.readers.ctipe_fast import CTIPe as CTIPe_fast
#from kamodo.readers.ctipe_time import CTIPe, ctipe_varnames
from kamodo.readers.ctipe_faster_wrapped import CTIPe, ctipe_varnames 

#need input times to be timestamps since 1970 to pull data from the correct files
#modified to reduce number of times the ctipe reader is called to reduce calc time
   
def CTIPeVariables():
    '''return a list of all the possible variables in CTIPe'''
    
    return ctipe_varnames

def CTIPe_Single_Prerun(file_dir, variable_list):
    '''Return a list of the required height input for each variable'''

    #check for wrapped data output in dir, create if necessary
    wrapped_files = glob.glob(file_dir+'*-plot-density-wrapped.nc')
    files = glob.glob(file_dir+'*-plot-density.nc')
    if len(wrapped_files)!=len(files):  #not all files are wrapped, if any
        print('Wrapped files not found. Generating...')
        from kamodo.readers.ctipe_data_wrapper import ctipe_wrap_files as wrap
        filename = [wrap(f) for f in files if f.split('.nc')[0]+'-wrapped.nc' not in wrapped_files][0]
        #produce the wrapped files for all not in file_dir, returns the new filenames, takes first                
    else:
        filename = wrapped_files[0]
    
    #create ctipe object, return vertical dependencies for variables requested
    ctipe = CTIPe(filename, variables_requested=variable_list, printfiles=False)
    var_test = []
    if 'H' in variable_list: variable_list.remove('H')  #H only needed if other functions require ilev
    for var in variable_list:  #determine which variables require ilev(2), height(1), neither(0)
        input_var_list = ctipe.variables[var]['xvec']
        if 'ilev' in input_var_list.keys(): var_test.append('ilev')
        elif 'height' in input_var_list.keys(): var_test.append('height')
        else: var_test.append('none')
    
    return var_test

def ts_to_hrs(time_val, filedate):
    '''Convert array of timestamps to hours since midnight of filedate string'''
    
    file_datetime = datetime.strptime(filedate+' 00:00:00', '%Y-%m-%d %H:%M:%S')
    return (datetime.utcfromtimestamp(time_val)-file_datetime).total_seconds()/3600.

def hrs_to_ts(time_val, filedate):
    '''Convert array of hours since midnight of filedate string to timestamps'''
    
    file_datetime = datetime.strptime(filedate+' 00:00:00', '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)    
    return datetime.timestamp(file_datetime+timedelta(hours=time_val))

sample_ilev = np.linspace(1,15,75,dtype=float)   #global variable
def CalcIlev(H, t, height, lat, lon):
    '''Approximate ilev by inverting the gridded height function CTIPe.H for one sat point'''
    
    rough_height = H(np.array([[t, ilev, lat, lon] for ilev in sample_ilev]))
    ilev_range = np.sort(sample_ilev[np.argsort(abs(height-rough_height))[0:2]])
    test_ilev = np.linspace(ilev_range[0],ilev_range[1],100,dtype=float)
    finer_height = H(np.array([[t, ilev, lat, lon] for ilev in test_ilev]))
    return test_ilev[np.argmin(abs(height-finer_height))]

def test_ctipevalid(ctipe, sat_time):
    ''' Determine if a new ctipe object is needed bsed on the time given'''
    
    if isinstance(ctipe, list):  
        return True  #if a list, then ctipe DNE, get a new one
    elif (sat_time>=ctipe.filetimes[0]) and (sat_time<=ctipe.filetimes[1]):
        return False  #sat_time is within known time range of file, use current ctipe
    else:  #sat_time is not within known time range of file, get a new ctipe object
        return True

def new_ctipe_object(file_dir, variable_list, sat_time):
    ''' Return a new ctipe object valid for the given time'''
    
    files, times = glob.glob(file_dir+'*-plot-density-wrapped.nc'), []
    for f in files:
        file_date = f.split('\\')[-1].split('/')[-1][:10]
        beg_ts = hrs_to_ts(0.25, file_date)  #ctipe data starts at 0.25
        end_ts = hrs_to_ts(24.0, file_date)  #and ends at 24.00
        times.extend([beg_ts,end_ts])
        if (sat_time>=beg_ts) and (sat_time<=end_ts):
            print(f'New CTIPe object is from {f}')
            return CTIPe(f, variables_requested=variable_list, printfiles=False) #fast
    return times
    

def CTIPe_Single_FlyAway(ctipe, variable_list, sat_time, sat_height, sat_lat, sat_lon, 
                  z_dependence=['none'], dz=''):
    '''fly satellite through CTIPe model data, per position
    sat_time, sat_height, sat_lat, and sat_lon are all floats, not arrays
    z_dependence = ["none","height","ilev"] if variables depend on all three options
       dependence must be in same order as variable_list to match with variable names'''
    
        
    #Create satellite tracks with appropriate inputs
    file_date = ctipe.datetimes[0][0:10]
    model_sat_time = ts_to_hrs(sat_time, file_date)
    if 'H' in variable_list: variable_list.remove('H')  #H only needed if other functions require ilev
    sat_track = {}  #initialize list of satellite tracks
    if "none" in z_dependence:
        sat_track['none']=[model_sat_time,sat_lat,sat_lon]
    if "height" in z_dependence:  #if function requires height (in km)
        sat_track['height']=[model_sat_time,sat_height/1000.,sat_lat,sat_lon]
    if "ilev" in z_dependence:  #if ilev is required for at least one variable
        sat_ilev = CalcIlev(ctipe.H, *[sat_time,sat_height,sat_lat,sat_lon])  
        sat_track['ilev']=[model_sat_time,sat_ilev,sat_lat,sat_lon]

    #retrieve interpolator and interpolate data for each variable. 
    results = {variable_list[i]: ctipe[variable_list[i]](sat_track[z_dependence[i]])[0] for i
               in range(len(variable_list))}
    
    #determine vertical derivatives for each variable if requested
    if dz!='':
        for i in range(len(variable_list)):
            if dz[i] and z_dependence[i]!='none':  #if dz requested and variable has a vertical dependence
                #generate tracks with slightly larger and smaller heights
                if z_dependence[i]=='height':
                    #stay within CTIPe height (km) boundaries
                    sat_height_low = sat_height/1000.-100
                    if sat_height_low <= 140.: sat_height_low = 140.
                    sat_height_high = sat_height/1000.+100.
                    if sat_height_high>=2000.: sat_height_high = 2000.
                    dz_track = [[model_sat_time,sat_height_low,sat_lat,sat_lon],
                                [model_sat_time,sat_height_high,sat_lat,sat_lon]]
                if z_dependence[i]=='ilev':
                    #stay within CTIPe ilev boundaries
                    sat_ilev_low = sat_ilev-1.
                    if sat_ilev_low<=1.: sat_ilev_low = 1.
                    sat_ilev_high = sat_ilev+1.
                    if sat_ilev_high >= 15.: sat_ilev_high = 15.
                    dz_track =  [[model_sat_time,sat_ilev_low,sat_lat,sat_lon],
                                [model_sat_time,sat_ilev_high,sat_lat,sat_lon]]
                dz_result = ctipe[variable_list[i]](dz_track)  #returns two values
                results[variable_list[i]+'_dz'] = dz_result[1]-dz_result[0]
    return results

def find_singlefile(file_pattern, sat_time, reader, dt=450., verbose=False):
    '''Find file containing given single time. Adjust if within dt seconds.'''
    
    #build lookup table
    files, times, ts_list, filename = glob.glob(file_pattern), {}, [], ''
    for f in files:
        k = reader(f, variables_requested=[], filetimes=True)
        file_date = k.datetimes[0][0:10]
        times[file_date] = [f,k.timerange['min'],k.timerange['max'],
                            k.filetimes[0], k.filetimes[1]]
        ts_list.extend([k.filetimes[0], k.filetimes[1]])
    
    #choose file time is in
    for file_date in times.keys():
        if ((sat_time>=times[file_date][3]) & (sat_time<=times[file_date][4])):
            filename = times[file_date][0]
            break
    if filename=='':  #if not in a file, try closest file if within dt
        sat_time = [ts_list[abs(np.array(ts_list)-sat_time).argmin()] if \
                          (abs(np.array(ts_list)-sat_time).min() < dt) else 0][0]
        if sat_time==0:  #error if not within dt
            raise AttributeError('Files in dir do not contain the given time.')
        elif verbose:
            print(f'Adjusting time to nearest file with {dt} seconds.')
        for file_date in times.keys():  #otherwise, find file
            if ((sat_time>=times[file_date][3]) & (sat_time<=times[file_date][4])):
                filename = times[file_date][0]
                break 
            
    return filename, sat_time

if __name__=='__main__':
    ''' Begin program '''
   
    '''#initialize input parameters (filename, variable_name)
    file_dir = 'C:/Users/rringuet/Kamodo_WinDev1/CTIPe/'
    variable_list = ['rho']  #Test ilev with N_n, without ilev with T_e, 3D with TEC
    #variable_list = ['rho', 'T', 'T_e', 'T_i', 'H', 'Vn_lat', 'Vn_lon', 'Vn_H', 
    #                 'T_n', 'Rmt', 'N_e', 'N_n', 'Q_Solar', 'Q_Joule', 'Q_radiation', 
    #                 'N_O', 'N_O2', 'N_N2', 'N_NO', 'N_NOplus', 'N_N2plus', 'N_O2plus', 
    #                 'N_Nplus', 'N_Oplus', 'N_Hplus', 'sigma_P', 'sigma_H', 'Vi_lon', 
    #                 'Vi_lat', 'W_Joule', 'Eflux_precip', 'Eavg_precip', 'TEC', 
    #                 'E_theta140km', 'E_lambda140km', 'E_theta300km', 'E_lambda300km']
    
    #generate iterator for sat_time testing, first position, and begin timing
    n, ctipe = 80640, []  #80640
    sat_time_arr = np.linspace(1426638400.0, 1426638400.0+80640.*2.5, n)  
    sat_lat, sat_lon, sat_height = -20., 120., 400000.  #height in m 
    
    
    #determine vertical variable dependency for each variable in variable_list
    z_dependence = CTIPe_Single_Prerun(file_dir, variable_list)
    print(f'Vertical dependence determined: {ti.perf_counter()-tic:.6f}')
    #returns list of string(s) indicating vertical dependency of each variable   
    '''
    
    #collect arguments from the command line
    #example: python C:\Users\rringuet\Kamodo_WinDev1\Kamodo-master\kamodo\readers\CTIPe_wrapper_fortrancmd.py
    #       C:/Users/rringuet/Kamodo_WinDev1/CTIPe/ rho ilev 1426637500.0 400.0 -25.0 10.0
    #syntax: file_dir, variable, z_dependence, sat_time, sat_height, sat_lat, sat_lon
    file_dir = str(sys.argv[1]) #'C:/Users/rringuet/Kamodo_WinDev1/CTIPe/'
    variable_list = [sys.argv[2]] #['rho']
    z_dependence = [sys.argv[3]] #['ilev']
    sat_time = float(sys.argv[4]) #1426637500.0
    sat_height = float(sys.argv[5]) #400. (in km)
    sat_lat = float(sys.argv[6]) #-25.
    sat_lon = float(sys.argv[7]) #90.
    #print(file_dir, variable_list, z_dependence, sat_time, sat_height, sat_lat, sat_lon)

    #find file nearest sat_time (within 450 seconds), correct sat_time if needed
    filename, sat_time = find_singlefile(file_dir+'*plot-density-wrapped.nc', 
                                         sat_time, CTIPe, dt=450., verbose=False)
    
    #get ctipe object for requested variable (+H too)
    ctipe = CTIPe(filename, variables_requested=variable_list, printfiles=False)
            
    #get results requested for single position given
    results_dict = CTIPe_Single_FlyAway(ctipe, variable_list, 
                                    sat_time, sat_height, sat_lat, sat_lon, 
                                    z_dependence=z_dependence, 
                                    dz=[1])
    #print results to a csv file
    file = open(file_dir+'results.txt', 'w')
    file.write(f"{results_dict[variable_list[0]]:.15f}, {results_dict[variable_list[0]+'_dz']:.15f}")
    file.close()
    
    
            
