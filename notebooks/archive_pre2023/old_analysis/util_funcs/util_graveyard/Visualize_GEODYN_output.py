# from netCDF4 import Dataset
import numpy as np 
import pandas as pd
# import os
import datetime
import matplotlib.pyplot as plt
from matplotlib import rc
import matplotlib as mpl
import os.path
from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)

def plot_params():
  mpl.rcParams['lines.markersize'] = 6
  plt.rcParams['axes.grid'] = True
  plt.rcParams['grid.alpha'] = 0.5
  plt.rcParams['grid.color'] = "#cccccc"
  plt.rcParams.update({'font.size': 17})
  rc('font',**{'family':'sans-serif','sans-serif':['Times']})
  plt.rcParams['axes.titlesize']='large' 
  plt.rcParams['axes.titlepad']= 3
  rc('text', usetex=False)
  plt.rcParams["legend.loc"] = 'best'

  return


def SingleSat_XYZ(isat,data_dict, iteration ):
    plot_params()
    fig, ( axs) = plt.subplots(3,2, figsize=(12,10), sharex=True)
    fig.suptitle('Satellite #'+str(isat)+' from Iteration '+ str(int(iteration)), y=1.0)
    axs[0,0].set_title(r'$X$' )
    axs[0,0].plot(data_dict[isat]['Date'], data_dict[isat]['X'].values.astype(float),'.', label = 'Residuals iter 6')
    axs[0,0].set(ylabel=  'meters') 

    axs[1,0].set_title(r'$Y$' )
    axs[1,0].plot(data_dict[isat]['Date'], data_dict[isat]['Y'].values.astype(float),'.', label = 'Residuals iter 6')
    axs[1,0].set(ylabel=  'meters') 

    axs[2,0].set_title(r'$Z$' )
    axs[2,0].plot(data_dict[isat]['Date'], data_dict[isat]['Z'].values.astype(float),'.', label = 'Residuals iter 6')
    axs[2,0].set(ylabel=  'meters') 

    axs[0,1].set_title(r'$\dot{X}$' )
    axs[0,1].plot(data_dict[isat]['Date'], data_dict[isat]['XDOT'].values.astype(float),'.', label = 'Residuals iter 6')
    axs[0,1].set(ylabel=  r'$\frac{m}{s}$') 

    axs[1,1].set_title(r'$\dot{Y}$' )
    axs[1,1].plot(data_dict[isat]['Date'], data_dict[isat]['YDOT'].values.astype(float),'.', label = 'Residuals iter 6')
    axs[1,1].set(ylabel=  r'$\frac{m}{s}$') 

    axs[2,1].set_title(r'$\dot{Z}$' )
    axs[2,1].plot(data_dict[isat]['Date'], data_dict[isat]['ZDOT'].values.astype(float),'.', label = 'Residuals iter 6')
    axs[2,1].set(ylabel=  r'$\frac{m}{s}$') 

    import matplotlib.dates as mdates
    myFmt = mdates.DateFormatter('%m/%d')

    fig.tight_layout( pad=1.5)

    for ax in fig.axes:
        plt.sca(ax)
        plt.xticks(rotation=45)
        locator = mdates.AutoDateLocator(minticks=3, maxticks=7)
        formatter = mdates.ConciseDateFormatter(locator)
        ax.xaxis.set_major_locator(locator)
        ax.xaxis.set_major_formatter(formatter)
        
        
        # ax.xaxis.set_major_formatter(myFmt)
        # ax.xaxis.set_major_locator(MultipleLocator(1))
        # ax.xaxis.set_major_locator(AutoMinorLocator(1))



    return fig

    # images_dir = '/content/drive/MyDrive/GEODYN-KAMODO Project/RUNS/adding_resid_orbit_printout/plots'
    # plt.savefig(f"{images_dir}/trajectory_xyz_sat%diter%d.png" % (isat, int(iteration) )) 




def Residuals_Observ_All(observ_resids, iteration):
      plot_params()

      fig, ( ax1,ax2) = plt.subplots(2,1, figsize=(12,10), sharex=False)
      ax1.set_title('IIEOUT Observation Residuals from Iteration '+iteration)
      ax1.plot(pd.to_datetime(observ_resids['Date']), observ_resids['Residual'].values.astype(float) ,'.', label = 'Residuals iter 5')
      ax1.set(ylabel=  'resids')  
      handles, labels = ax1.get_legend_handles_labels()
      ax1.legend(handles[::-1], labels[::-1],  loc='center left', bbox_to_anchor=(1,0.5), markerscale = 2 )

      ax2.set_title('Ratio to sigma ')
      ax2.set(ylabel=  'Ratio to sigma')  
      ax2.plot(pd.to_datetime(observ_resids['Date']), observ_resids['Ratio_to_sigma_fixed'].values.astype(float) ,'.', label = 'ratio to sigma')
      # ax2.plot(pd.to_datetime(observ_resids['Date']), observ_resids['Residual'].values.astype(float)*1e2 ,'.', label = 'Residuals iter 5')

      handles2, labels2 = ax2.get_legend_handles_labels()
      ax2.legend(handles2[::-1], labels2[::-1],  loc='center left', bbox_to_anchor=(1,0.5), markerscale = 2 )


      # # ax2.set(xlabel= 'Modified Julian Date') 
      # ax2.set(xlabel= 'Date') 

      fig.tight_layout( pad=5.0)


      for ax in fig.axes:
          plt.sca(ax)
          plt.xticks(rotation=45)
      return fig









def SingleSat_XYZ_multiple_runs(isat, iteration , data_dict_list):

    plot_params()
    fig, ( axs) = plt.subplots(3,2, figsize=(12,10), sharex=True)

    for i,val in enumerate(data_dict_list):
        # print(i, data_dict_list[val])



        fig.suptitle('Compare DEN Models: Trajectory of GPS-Satellite #'+str(isat)+' from Iteration '+ str(int(iteration)), y=1.0)
        axs[0,0].set_title(r'$X$' )
        axs[0,0].plot(data_dict_list[val][isat]['Date'], data_dict_list[val][isat]['X'].values.astype(float),'.', label = val )
        axs[0,0].set(ylabel=  'meters') 

        axs[1,0].set_title(r'$Y$' )
        axs[1,0].plot(data_dict_list[val][isat]['Date'], data_dict_list[val][isat]['Y'].values.astype(float),'.', label = val )
        axs[1,0].set(ylabel=  'meters') 

        axs[2,0].set_title(r'$Z$' )
        axs[2,0].plot(data_dict_list[val][isat]['Date'], data_dict_list[val][isat]['Z'].values.astype(float),'.', label = val )
        axs[2,0].set(ylabel=  'meters') 

        axs[0,1].set_title(r'$\dot{X}$' )
        axs[0,1].plot(data_dict_list[val][isat]['Date'], data_dict_list[val][isat]['XDOT'].values.astype(float),'.', label = val )
        axs[0,1].set(ylabel=  r'$\frac{m}{s}$') 

        axs[1,1].set_title(r'$\dot{Y}$' )
        axs[1,1].plot(data_dict_list[val][isat]['Date'], data_dict_list[val][isat]['YDOT'].values.astype(float),'.', label = val )
        axs[1,1].set(ylabel=  r'$\frac{m}{s}$') 

        axs[2,1].set_title(r'$\dot{Z}$' )
        axs[2,1].plot(data_dict_list[val][isat]['Date'], data_dict_list[val][isat]['ZDOT'].values.astype(float),'.', label = val )
        axs[2,1].set(ylabel=  r'$\frac{m}{s}$') 

    import matplotlib.dates as mdates
    myFmt = mdates.DateFormatter('%m/%d')

    fig.tight_layout( pad=2.5)

    for ax in fig.axes:
        plt.sca(ax)
        plt.xticks(rotation=45)
        locator = mdates.AutoDateLocator(minticks=3, maxticks=7)
        formatter = mdates.ConciseDateFormatter(locator)
        ax.xaxis.set_major_locator(locator)
        ax.xaxis.set_major_formatter(formatter)
    handles, labels = axs[1,1].get_legend_handles_labels()
    axs[1,1].legend(handles[::-1], labels[::-1],  loc='center left', bbox_to_anchor=(1,0.5), markerscale = 2 )

        
        # ax.xaxis.set_major_formatter(myFmt)
        # ax.xaxis.set_major_locator(MultipleLocator(1))
        # ax.xaxis.set_major_locator(AutoMinorLocator(1))



    return fig




def plot_density(den_df, choose_model, choose_sat):

    plot_params()
    fig, (ax1, ax2) = plt.subplots(2,1, figsize=(12,10), sharex=False)
    ax1.set_title(r'$\rho$ along the orbit of the '+choose_sat+' as estimated by '+ choose_model)
    ax1.semilogy(den_df['Date'], den_df['rho (kg/m**3)'] ,'.',  ms=2 ,label = r'$\rho')
    ax1.set(ylabel=  r'$\frac{kg}{m^3}$')  
    ax1.set(xlabel=  r'Date')  

    ax2.set_title(r'$\frac{\delta \rho}{\delta z}$ along the orbit of the '+choose_sat+' as from GEODYN', y=1.11)
    ax2.plot(den_df['Date'], (den_df['delta_rho (kg/m**3/m)']) ,'.',  ms=2 ,label = r'$\delta \rho')
    ax2.set(ylabel=  r'$\frac{kg}{m^3/m}$')  
    ax2.set(xlabel=  r'Date')
    ax2.set(yscale = 'symlog')
    # ax2.set_ylim(1e-17, 1e-18])
    # handles, labels = ax1.get_legend_handles_labels()
    # ax1.legend(handles[::-1], labels[::-1],  loc='center left', bbox_to_anchor=(1,0.5), markerscale = 2 )



    # # # ax2.set(xlabel= 'Modified Julian Date') 
    # # ax2.set(xlabel= 'Date') 

    fig.tight_layout( pad=4.0)


    for ax in fig.axes:
        plt.sca(ax)
        plt.xticks(rotation=45)
        # plt.ticklabel_format(axis ='y')
        # plt.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
    return fig



def compare_multiple_density_models(dens_list, choose_sat):
    x = 10661

    mpl.rcParams['lines.markersize'] = 2
    plt.rcParams['axes.grid'] = True
    plt.rcParams['grid.alpha'] = 1
    plt.rcParams['grid.color'] = "#cccccc"
    plt.rcParams.update({'font.size': 16})
    rc('font',**{'family':'sans-serif','sans-serif':['Calibri']})
    plt.rcParams['axes.titlesize']='large' 
    plt.rcParams['axes.titlepad']= 3
    rc('text', usetex=False)
    plt.rcParams["legend.loc"] = 'best'


    fig, ( ax1,ax2) = plt.subplots(2,1, figsize=(12,10), sharex=False)
    for i,val in enumerate(dens_list):
        print(i,val)
        if val== 'MSIS86':
          color_val = 'tab:blue'
        elif val == 'DTM 87':
          color_val = 'tab:green'
        elif val == 'Jacchia 71':
          color_val = 'tab:red'

        ax1.set_title(r'Compare Density Models: $\rho$ along '+choose_sat+' Orbit')
        ax1.semilogy(dens_list[val]['Date'][:x], dens_list[val]['rho (kg/m**3)'][:x] ,'.', color = color_val, label = val)
        ax1.set(ylabel=  r'$\frac{kg}{m^3}$')  
        handles, labels = ax1.get_legend_handles_labels()
        ax1.legend(handles[::-1], labels[::-1],  loc='center left', bbox_to_anchor=(1,0.5), markerscale = 2 )

        ax2.set_title(r'$\frac{\delta \rho}{\delta z}$ along the orbit of the '+choose_sat+' as from GEODYN', y=1.11)
        ax2.set(ylabel=  r'$\frac{kg}{m^3/m}$')  
        ax2.plot(dens_list[val]['Date'][:x], dens_list[val]['delta_rho (kg/m**3/m)'][:x] ,'.',color = color_val,  label = val)
        ax2.set(yscale = 'symlog')

        handles2, labels2 = ax2.get_legend_handles_labels()
        ax2.legend(handles2[::-1], labels2[::-1],  loc='center left', bbox_to_anchor=(1,0.5), markerscale = 8 )


        # # ax2.set(xlabel= 'Modified Julian Date') 
        # ax2.set(xlabel= 'Date') 

        fig.tight_layout( pad=2.0)


        for ax in fig.axes:
            plt.sca(ax)
            plt.xticks(rotation=45)

        # images_dir = '/content/drive/MyDrive/GEODYN-KAMODO Project/RUNS/adding_resid_orbit_printout/plots'
        # plt.savefig(f"{images_dir}/ObservationResidualsIter%d_shortTiem.png" % int(iteration) ) 
            
    return fig




