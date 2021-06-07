# -*- coding: utf-8 -*-
"""
Created on Fri Apr 23 17:09:14 2021

@author: rringuet
"""
import glob, os
import numpy as np
from datetime import datetime, timedelta, timezone
import kamodo.readers.FlythroughPlots as FPlot



@np.vectorize
def ts_to_hrs(time_val, filedate):
    '''Convert array of timestamps to hours since midnight of filedate string'''
    
    file_datetime = datetime.strptime(filedate+' 00:00:00', '%Y-%m-%d %H:%M:%S')
    return (datetime.utcfromtimestamp(time_val)-file_datetime).total_seconds()/3600.


@np.vectorize
def hrs_to_ts(time_val, filedate):
    '''Convert array of hours since midnight of filedate string to timestamps'''
    
    file_datetime = datetime.strptime(filedate+' 00:00:00', '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)    
    return datetime.timestamp(file_datetime+timedelta(hours=time_val))

def check_plot_dir(plot_file):
    '''If plot_dir does not exist, create it'''
    
    if '-' in plot_file.split('/')[-1].split('\\')[-1]: 
        plot_dir = plot_file[0:-10]  #cut off date 
    else:
        plot_dir = plot_file
    if not os.path.isdir(plot_dir): os.mkdir(plot_dir)
    return


def save_times(file_pattern, sat_time, reader, dt=450., verbose=False):
    '''Adjust times between files to filetime within half of dt in seconds (7.5min by default)'''

    files, times, ts_list = glob.glob(file_pattern), {}, []
    for f in files:
        k = reader(f, variables_requested=[], filetimes=True)
        file_date = k.datetimes[0][0:10]
        times[file_date] = [f,k.timerange['min'],k.timerange['max'],
                            k.filetimes[0], k.filetimes[1]]
        ts_list.extend([k.filetimes[0], k.filetimes[1]])
    
    #look for sat_times not in files
    sat_time_mask = np.zeros(len(sat_time), bool)  #mask of indices to not change
    idx_arr = np.linspace(0,len(sat_time)-1,len(sat_time), dtype=int)  #list of all indices
    for file_date in times.keys():  #filter out times in files
        idx = np.where((sat_time>=times[file_date][3]) & (sat_time<=times[file_date][4]))[0]
        sat_time_mask[idx] = True  #True to not change times in files
    change_idx = np.delete(idx_arr, sat_time_mask)  #indices of times not in files
        
    #correct times not in files to best time within 7.5 minutes, else leave as is
    new_times = np.array([ts_list[abs(np.array(ts_list)-s).argmin()] if \
                          (abs(np.array(ts_list)-s).min() < dt) \
                 else -1. for s in sat_time[change_idx]])
    new_idx = np.where(new_times > -1.)[0]
    print(f'\nAdjusted {len(new_idx)} times within 7.5 minutes of a file to be in the nearest file.')
    if verbose:
        for i in new_idx: 
            print(f'Given: {sat_time[change_idx[i]]:.3f}, Nearest: {new_times[i]:.3f}, '+\
                  f'Time diff (minutes): {(sat_time[change_idx[i]]-new_times[i])/60.:.3f}')
    sat_time[change_idx[new_idx]] = new_times[new_idx]
    #for i in new_idx: print(f'{sat_time[change_idx[i]]:.3f}')  #shows that replacement actually happened
    bad_idx = change_idx[np.where(new_times == -1.)[0]]
    net_idx = np.delete(idx_arr, bad_idx)
    
    #print errors for any remaining 'bad' times
    if len(bad_idx)>0:
        print(f'{len(bad_idx)} data points are farther than 7.5 minutes from model times and are excluded.')
        if verbose: print(sat_time[bad_idx])  #print 'bad' sat_times
        print('\nCheck that data fits in file time ranges:')
        print(f'Data time range (UTC): {min(sat_time)} {max(sat_time)}')
        print('Filename, Min DateTime, Max DateTime, Min Time (UTC), Max Time (UTC)')
        for file_date in times.keys(): 
            print (file_date+times[file_date][0].split(file_date)[-1], times[file_date][3:5])
            
    #distribute indices of sat_time times for each file
    for file_date in times.keys():  #filter out times in files
        idx = np.where((sat_time>=times[file_date][3]) & (sat_time<=times[file_date][4]))[0]
        times[file_date].append(idx)
        
    return sat_time, times, net_idx

def make_net_plots(var, results_dict, results_units, varlist_4d, varlist_3d, file_dir,
              plot_close, plot_sampling):
    '''Make net plots for a flythrough of one variable'''
    
    check_plot_dir(file_dir+'Plots/')  #create proper directory if DNE
    if var in varlist_4d:
        FPlot.Plot4D(var, results_dict['sat_time'], results_dict['sat_lat'], 
                     results_dict['sat_lon'], results_dict['sat_height'], 
                     results_dict[var], results_units[var], 
                     sat_zunits='km', plot_file=file_dir+'Plots/', 
                     plot_close=plot_close, plot_sampling=plot_sampling)
    elif var in varlist_3d:
        FPlot.Plot3D(var, results_dict['sat_time'], results_dict['sat_lat'], 
                     results_dict['sat_lon'], results_dict[var], results_units[var], 
                     plot_file=file_dir+'Plots/', plot_close=plot_close, 
                     plot_sampling=plot_sampling)    
        
def make_daily_plots(var, idx, sat_time, sat_lat, sat_lon, results, results_units, 
                     plot_file, plot_sampling, sat_height=0, sat_ilev=0):
    '''Make daily plots for a flythrough of one variable'''
    
    #Choose correct plotting routing  
    if idx=='0':  #independent of height
        FPlot.Plot3D(var, sat_time, sat_lat, sat_lon, results, 
                     results_units, plot_file, plot_sampling=plot_sampling)
    elif idx=='1': #ALL functions that require height have input height in km
        FPlot.Plot4D(var, sat_time, sat_lat, sat_lon, sat_height, 
                     results, results_units, plot_file, 
                     sat_zunits='km', plot_sampling=plot_sampling) 
    elif idx=='2':  #all others require ilev
        FPlot.Plot4D_ilev(var, sat_time, sat_lat, sat_lon, sat_height, 
                          sat_ilev, results, results_units, plot_file,
                          sat_zunits='km', sat_ilevunits='', 
                          sat_ilevname='Pressure\nLevel', plot_sampling=plot_sampling)

