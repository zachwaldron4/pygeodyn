import plotly.graph_objects as go
from plotly.offline import plot, iplot
from plotly.subplots import make_subplots
import plotly.express as px
import plotly.io as pio   ### Allows you to save plotly figs
import pandas as pd
import numpy as np

# import sys
# sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')


# arc_listlist=[  ['2018.292', '2018.293', '2018.294', '2018.295', '2018.296', 
#                  '2018.297', '2018.298', '2018.299' ],                  
#                 #
#                 ['2018.304', '2018.305', '2018.306', '2018.307', '2018.308' ],  
#                 #
#                 ['2018.313', '2018.314', '2018.315', '2018.316', '2018.317',
#                  '2018.318', '2018.319', '2018.320', '2018.321', '2018.322',
#                  '2018.323', '2018.324', '2018.325', '2018.326', '2018.327' ],  
#                 #
#                 ['2018.335', '2018.336', '2018.337' ],  
#                 #
#                 ['2018.349', '2018.350', '2018.351', '2018.352' ],  
#                 #
#                 ['2018.356', '2018.357', '2018.358' ],  
#                 #
#                 ['2018.365', '2019.001', '2019.002', '2019.003', '2019.004', 
#                  '2019.005', '2019.006', '2019.007', '2019.008',
#                 '2019.009'],  
#                 ]

config = dict({
                'displayModeBar': False,
                'responsive': False,
                'staticPlot': False,
                'displaylogo': False,
                'showTips': False,
                })




# Simplify Plotting Schemes:
col_msis2     =  "#2ca02c"  # 'tab:green'
col_jb2008    =  "#ff7f0e"  # 'tab:orange'
col_dtm2020   =  "#d62728"  # 'tab:red'
col_tiegcm_oc =  "#17becf"  # 'tab:cyan'
col_hasdm_oc  =  "#1f77b4"  # 'tab:blue'
col_ctipe_oc  =  "#9467bd"  # 'tab:purple'
col_gitm      =  '#e377c2'  # 'tab:pink'

x_annot_val = 1.1
font_dict=dict(family='Arial',size=14,color='black')
font_annot=dict(family='Arial',size=11,color='black')


def get_plot_params(model_name_string):
    '''
    INPUT:   
        Plot number, model_name string, x_annot_val
    
    RETURN:
        col, x_annot, y_annot1, y_annot2, m_size,   
    '''
        
    if model_name_string == 'msis2':
        col=col_msis2
    if model_name_string == 'manual_msis2':
        col=col_msis2
    elif model_name_string == 'dtm2020_o':
        col=col_dtm2020
    elif model_name_string == 'manual_dtm2020_o':
        col=col_dtm2020
    elif model_name_string == 'jb2008':
        col=col_jb2008
    elif model_name_string == 'manual_jb2008':
        col=col_jb2008
    elif model_name_string == 'tiegcm_oc':
        col=col_tiegcm_oc
    elif model_name_string == 'manual_tiegcm_oc':
        col=col_tiegcm_oc
    elif model_name_string == 'hasdm_oc':
        col=col_hasdm_oc
    elif model_name_string == 'ctipe_oc':
        col=col_ctipe_oc
    elif model_name_string == 'manual_ctipe_oc':
        col=col_ctipe_oc
    elif model_name_string == 'gitm':
        col=col_gitm
    elif model_name_string == 'manual_gitm':
        col=col_gitm
    elif model_name_string == 'manual_ensembleAvg':
        col='black'
    elif model_name_string == 'manual_ensembleWgtAvg':
        col='black'
    elif model_name_string == 'manual_ensembleMed':
        col='blue'

    ### Old Models
    elif model_name_string == 'dtm87':
        col='grey'
    elif model_name_string == 'jaachia71':
        col='grey'
    elif model_name_string == 'msis86':
        col='grey'
    elif model_name_string == 'msis00':
        col='tan'
        
    return(col)


def orb_avg(den_df, arc):
    
    
    #### Find the index for the correct date
#     vals  = np.arange(den_df[arc].index[0],den_df[arc].index[-1]+1)
#     df = den_df[arc].set_index('Date',drop=False ) 
#     df['i_vals'] = vals
#     index_date = df.loc[df.index.max()]['i_vals'].min()
#     print('index_date', index_date)
  
    den_df[arc] = den_df[arc].reset_index(drop=True)  

    
    lat = np.asarray(den_df[arc]['Lat'][:])
    time_pd = pd.to_datetime(den_df[arc]['Date'][:])
    # i = np.nonzero( lat[1:]*lat[0:-1]  <  np.logical_and(0 , lat[1:] > lat[0:-1] )  )
    # i = i[0]
    #### FIXED orbit average..... start of each orbit is ascending equatorial pass
    a =   np.logical_and( lat[1:]*lat[0:-1]  < 0,  lat[1:] > lat[0:-1] )
    tup_i = a.ravel().nonzero( )
    i = np.squeeze(tup_i)

    # print(len(tup_i))
    # print(len(i))
    d_avg = np.zeros(np.size(i))
    height_avg = np.zeros(np.size(i))
    
#     print('time_pd',time_pd)

    time_avg = []
    d_avg_rolling = []
    
    roll_avg_count = 0
    # for j in np.arange(0, np.size(i)):
    for j in range(np.size(i)-1):
        d_avg[j]      = np.mean(den_df[arc]['rho (kg/m**3)'  ][i[j] : i[j+1]  ]  )
        height_avg[j] = np.mean(den_df[arc]['Height (meters)'][i[j] : i[j+1]  ]  )
#         mean_time      = np.mean(time_pd[   i[j] : i[j+1]-1  ])
        t1 = pd.to_datetime(time_pd[ i[j]    ])
        t2 = pd.to_datetime(time_pd[ i[j+1]])
        # datemiddle = pd.Timestamp(t1) + (pd.Timestamp(t2) - pd.Timestamp(t1)) / 2
        datemiddle = pd.Series([t1, t2]).mean()
        time_avg.append(datemiddle)

        if roll_avg_count ==1:
            d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))
            roll_avg_count =0
            
        roll_avg_count+=1 
    # d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))
        
    return(time_avg, d_avg, d_avg_rolling )
    
def orb_avg_param(DFin, arc, param_str):
    
    
    DFin[arc] = DFin[arc].reset_index(drop=True)  

    
    lat = np.asarray(DFin[arc]['Lat'][:])
    time_pd = pd.to_datetime(DFin[arc]['Date'][:])
    i = np.nonzero( lat[1:]*lat[0:-1]  <  np.logical_and(0 , lat[1:] > lat[0:-1] )  )
    i = i[0]

    d_avg = np.zeros(np.size(i))
    height_avg = np.zeros(np.size(i))
    
#     print('time_pd',time_pd)

    time_avg = []
    d_avg_rolling = []
    
    roll_avg_count = 0
    for j in range(np.size(i)-1):
        d_avg[j]      = np.mean(DFin[arc][param_str][i[j] : i[j+1]-1  ]  )
#         height_avg[j] = np.mean(DFin[arc]['Height (meters)'][i[j] : i[j+1]-1  ]  )
#         mean_time      = np.mean(time_pd[   i[j] : i[j+1]-1  ])
        t1 = pd.to_datetime(time_pd[ i[j]    ])
        t2 = pd.to_datetime(time_pd[ i[j+1]-1])
        datemiddle = pd.Timestamp(t1) + (pd.Timestamp(t2) - pd.Timestamp(t1)) / 2

        time_avg.append(datemiddle)

        if roll_avg_count ==1:
            d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))
            roll_avg_count =0
            
        roll_avg_count+=1 
    d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))
    param_avg          = d_avg
    param_avg_rolling  = d_avg_rolling  
    return(time_avg, param_avg, param_avg_rolling )

def plot_density_orbit_avg(fig, obj_m1, plot_num ):
    

    ####  Get plot Parameters for this model
    model_m1 = obj_m1.__dict__['global_params']['den_model']
    col,x_annot,y_annot1,y_annot2,m_size = get_plot_params(plot_num, model_m1)
    
    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        
        vals  = np.arange(obj_m1.__dict__['Density'][arc].index[0],obj_m1.__dict__['Density'][arc].index[-1]+1)
        df = obj_m1.__dict__['Density'][arc].set_index('Date',drop=False ) 
        df['i_vals'] = vals
        index_date = df.loc[df.index.max()]['i_vals'].min()

        
        time_avg,d_avg, d_avg_rolling = orb_avg(obj_m1.Density, arc)
        
        
        fig.add_trace(go.Scattergl(x=time_avg,
                                 y=d_avg_rolling,
#                                  y=d_avg,
                                name= model_m1 ,
                                mode='markers',
                                marker=dict(
                                color=col,
                                size=7,),
                                showlegend=False,
                                   ),
                                   row=1, col=1,
                                   )
        

        fig.update_yaxes(type="log", exponentformat= 'power',row=1, col=1)
    fig.update_xaxes(title_text="Date", row=1, col=1)
    fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
#     fig.update_yaxes(title_text="nT", row=2, col=1)
#     fig.update_yaxes(title_text="sfu", row=3, col=1)
    fig.update_layout(legend= {'itemsizing': 'constant'})
#     fig.update_layout(
#         font=dict(          size=18, ),
#         autosize=False,
#         width=900,
#         height=1000,)
    
    return(fig)



def Plot_Densitycomparison(fig, obj_m1, plot_num):


    ####  Get plot Parameters for this model
    model_m1 = obj_m1.__dict__['global_params']['den_model']
    col,x_annot,y_annot1,y_annot2,m_size = get_plot_params(plot_num, model_m1)
    
    
    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input'][:]):
        
        
        #### INDEX THE DENSITY DF correctly
        vals  = np.arange(obj_m1.__dict__['Density'][arc].index[0],obj_m1.__dict__['Density'][arc].index[-1]+1)
        df = obj_m1.__dict__['Density'][arc].set_index('Date',drop=False ) 
        df['i_vals'] = vals
        index_date = df.loc[df.index.max()]['i_vals'].min()

        
        str_run_param = 'run_parameters'+ arc
        final_iter = obj_m1.__dict__[str_run_param]['str_iteration']
        i_arc = ii+1
        
        
        
        vals  = np.arange(obj_m1.__dict__['Density'][arc].index[0],obj_m1.__dict__['Density'][arc].index[-1]+1)
        df = obj_m1.__dict__['Density'][arc].set_index('Date',drop=False ) 
        df['i_vals'] = vals
        index_date = df.loc[df.index.max()]['i_vals'].min()       
        time_avg,d_avg, d_avg_rolling = orb_avg(obj_m1.Density, arc)
        
        
        
        print('----',model_m1,'----')
        print('     mean:    ',np.mean(obj_m1.Density[arc]['rho (kg/m**3)']),'----')
        print('     variance:',np.std(obj_m1.Density[arc]['rho (kg/m**3)']),'----')
        print()
#         if ii ==0:
#             fig.add_trace(go.Scattergl(  x=obj_m1.Density[arc]['Date'][:index_date][:],
#                                      y=obj_m1.Density[arc]['rho (kg/m**3)'][:index_date][:],
#                                      name= model_m1,
#                                      mode='markers',
#                                      opacity=1,
#                                      marker=dict(
#                                         color=col, 
#                                         size=m_size,),
#                                      showlegend=True,),
#                                       secondary_y=False,
#                                        row=1, col=1,)

#         else:
#             fig.add_trace(go.Scattergl(  x=obj_m1.Density[arc]['Date'][:index_date][:],
#                          y=obj_m1.Density[arc]['rho (kg/m**3)'][:index_date][:],
#                          name= model_m1,
#                          mode='markers',
#                          opacity=1,
#                          marker=dict(
#                             color=col, 
#                             size=m_size,),
#                          showlegend=False,),
#                           secondary_y=False,
#                            row=1, col=1,)
        fig.add_trace(go.Scattergl(x=time_avg,
                     y=d_avg_rolling,
                    name= model_m1 ,
                    mode='markers',
                    marker=dict(
                    color='black',
                    size=5,),
                    showlegend=False,),
                       row=1, col=1,)


#         (mean,rms,rms_about_zero) = STATS_residuals(data_resids['T'], 'in-track')

#         fig = add_stats_annotation(fig, model_m1+'<br>Mean='+ str(np.round(mean,4))+'<br>RMS='+ str(np.round(rms_about_zero,4)), col , x_annot, y_annot1)


    fig.update_yaxes( title=r"$\frac{kg}{m^3}$", type='log', exponentformat= 'power',row=1, col=1)
    fig.update_xaxes( title="Date", row=1, col=1)

    return(fig)


def legend_as_annotation(fig, den_model_string, color_it, x_annot, y_annot):
    fig.add_annotation(
            x=x_annot,
            y=y_annot,
            xref="paper",
            yref="paper",
            showarrow=False,
            text=den_model_string,
            font=dict(
                size=16,
                color="#ffffff"
                ),
            align="center",
            bordercolor="#c7c7c7",
            borderwidth=2,
            borderpad=4,
            bgcolor=color_it,
            opacity=0.9
            )

    return(fig)

def add_stats_annotation(fig, text_in, col, x_annot, y_annot):
    fig.add_annotation(
            x=x_annot,
            y=y_annot,
            xref="x domain",
            yref="y domain",
            showarrow=False,
            text=text_in,
            font=dict(
                size=13,
                color="#ffffff"
                ),
            align="center",
            bordercolor="#c7c7c7",
            borderwidth=2,
            borderpad=4,
            bgcolor=col,
            opacity=0.9
            )
    return(fig)

import pandas as pd

def STATS_residuals(residuals,measurement_type):
    import numpy as np
    n = np.size(residuals)
    mean = (1/n)*(np.sum(residuals))
    variance = (1/n)*(np.sum(np.square(residuals)))
    rms = np.sqrt(variance)
    rms_about_zero = np.sqrt((n/(n-1))*variance)
    
                
#     print('mean            ',measurement_type ,':',mean)
#     print('rms             ',measurement_type ,':',rms)
#     print('rms about zero  ',measurement_type ,':',rms_about_zero)
#     print()
    
    return(mean,rms,rms_about_zero)



def add_fancy_legend(fig, run_dict, rms_total_return):
    
    modelnames=[]
    modelcolors = []
    #### LEGEND ####
    for model in run_dict.keys():
        if model == 'msis2':
            modelnames.append("MSISe2 | RMS="+str(np.round(rms_total_return['msis2'],2)))
            modelcolors.append(col_msis2)

        elif model == 'dtm2020_o':
            modelnames.append("DTM2020")
            modelcolors.append(col_dtm2020)

        elif model == 'jb2008':
            modelnames.append("JB2008")
            modelcolors.append(col_jb2008)

        elif model == 'tiegcm_oc':
            modelnames.append("TIEGCM")
            modelcolors.append(col_tiegcm_oc)

        elif model == 'hasdm_oc':
            modelnames.append("HASDM")
            modelcolors.append(col_hasdm_oc)

        elif model == 'ctipe_oc':
            modelnames.append("CTIPe")
            modelcolors.append(col_ctipe_oc)

        elif model == 'gitm':
            modelnames.append("GITM")
            modelcolors.append(col_gitm)



    df = pd.DataFrame({"starts_colors": modelcolors})

    fig.update_traces(showlegend=False).add_traces(
        [   go.Scattergl(name=modelnames[i], 
                   x=[pd.to_datetime( "181110-000000", format='%y%m%d-%H%M%S')],
                   mode='lines',
                   line = dict(shape = 'hv',  width=10),
                   marker_color=c, 
                   showlegend=True)
            for i,c in enumerate((df.loc[:,["starts_colors"]].values.ravel()))])
    fig.update_layout(legend=dict(
        yanchor="middle",
        y=0.5,
        xanchor="center",
        x=1,
    #     x=1.015,
            font=dict(family='Arial',size=13,color='black')      ,
            bgcolor="white",
            bordercolor="darkgrey",
            borderwidth=0.8,
        )  )
    
    return fig

def add_fancy_legend_noRMS(fig, run_dict):
    
    modelnames=[]
    modelcolors = []
    #### LEGEND ####
    for model in run_dict.keys():
        if model == 'msis2':
            modelnames.append("MSISe2")# | RMS="+str(np.round(rms_total_return['msis2'],2)))
            modelcolors.append(col_msis2)

        elif model == 'dtm2020_o':
            modelnames.append("DTM2020")
            modelcolors.append(col_dtm2020)

        elif model == 'jb2008':
            modelnames.append("JB2008")
            modelcolors.append(col_jb2008)

        elif model == 'tiegcm_oc':
            modelnames.append("TIEGCM")
            modelcolors.append(col_tiegcm_oc)

        elif model == 'hasdm_oc':
            modelnames.append("HASDM")
            modelcolors.append(col_hasdm_oc)

        elif model == 'ctipe_oc':
            modelnames.append("CTIPe")
            modelcolors.append(col_ctipe_oc)

        elif model == 'gitm':
            modelnames.append("GITM")
            modelcolors.append(col_gitm)



    df = pd.DataFrame({"starts_colors": modelcolors})

    fig.update_traces(showlegend=False).add_traces(
        [   go.Scattergl(name=modelnames[i], 
                   x=[pd.to_datetime( "181110-000000", format='%y%m%d-%H%M%S')],
                   mode='lines',
                   line = dict(shape = 'hv',  width=10),
                   marker_color=c, 
                   showlegend=True)
            for i,c in enumerate((df.loc[:,["starts_colors"]].values.ravel()))])
    fig.update_layout(legend=dict(
        yanchor="middle",
        y=0.5,
        xanchor="center",
#         x=1,
        x=1.015,
            font=dict(family='Arial',size=13,color='black')      ,
            bgcolor="white",
            bordercolor="darkgrey",
            borderwidth=0.8,
        )  )
    
    return fig



def QuickLook_plots(self, plot_num, PLOTTYPE):
        import plotly.graph_objects as go
        from plotly.offline import plot, iplot
        from plotly.subplots import make_subplots
        import plotly.express as px
        import pandas as pd
        import numpy as np
        config = dict({
                        'displayModeBar': True,
                        'responsive': False,
                        'staticPlot': False,
                        'displaylogo': False,
                        'showTips': False,
                        })

        import os

        def get_color(colorscale_name, loc):
            from _plotly_utils.basevalidators import ColorscaleValidator
            # first parameter: Name of the property being validated
            # second parameter: a string, doesn't really matter in our use case
            cv = ColorscaleValidator("colorscale", "")
            # colorscale will be a list of lists: [[loc1, "rgb1"], [loc2, "rgb2"], ...] 
            colorscale = cv.validate_coerce(colorscale_name)

            if hasattr(loc, "__iter__"):
                return [get_continuous_color(colorscale, x) for x in loc]
            return get_continuous_color(colorscale, loc)


        import plotly.colors
        from PIL import ImageColor

        def get_continuous_color(colorscale, intermed):
            """
            Plotly continuous colorscales assign colors to the range [0, 1]. This function computes the intermediate
            color for any value in that range.

            Plotly doesn't make the colorscales directly accessible in a common format.
            Some are ready to use:

                colorscale = plotly.colors.PLOTLY_SCALES["Greens"]

            Others are just swatches that need to be constructed into a colorscale:

                viridis_colors, scale = plotly.colors.convert_colors_to_same_type(plotly.colors.sequential.Viridis)
                colorscale = plotly.colors.make_colorscale(viridis_colors, scale=scale)

            :param colorscale: A plotly continuous colorscale defined with RGB string colors.
            :param intermed: value in the range [0, 1]
            :return: color in rgb string format
            :rtype: str
            """
            if len(colorscale) < 1:
                raise ValueError("colorscale must have at least one color")

            hex_to_rgb = lambda c: "rgb" + str(ImageColor.getcolor(c, "RGB"))

            if intermed <= 0 or len(colorscale) == 1:
                c = colorscale[0][1]
                return c if c[0] != "#" else hex_to_rgb(c)
            if intermed >= 1:
                c = colorscale[-1][1]
                return c if c[0] != "#" else hex_to_rgb(c)

            for cutoff, color in colorscale:
                if intermed > cutoff:
                    low_cutoff, low_color = cutoff, color
                else:
                    high_cutoff, high_color = cutoff, color
                    break

            if (low_color[0] == "#") or (high_color[0] == "#"):
                # some color scale names (such as cividis) returns:
                # [[loc1, "hex1"], [loc2, "hex2"], ...]
                low_color = hex_to_rgb(low_color)
                high_color = hex_to_rgb(high_color)

            return plotly.colors.find_intermediate_color(
                lowcolor=low_color,
                highcolor=high_color,
                intermed=((intermed - low_cutoff) / (high_cutoff - low_cutoff)),
                colortype="rgb",
            )

        cols = get_color("Viridis", np.linspace(0, 1, 5))
        map_cols = np.linspace(0, 1, 5)
        colorscale=[]
        for i,val in enumerate(map_cols):
            colorscale.append([val, cols[i]])

        # Simplify Plotting Schemes:
        col1 =  px.colors.qualitative.Plotly[0]
        col2 =  px.colors.qualitative.Plotly[1]
        col3 =  px.colors.qualitative.Plotly[2]
        col4 =  px.colors.qualitative.Plotly[3]
        col5 =  px.colors.qualitative.Plotly[4]
        col6 =  px.colors.qualitative.Plotly[5]

        if plot_num == 0:
            col = col1
            m_size = 4.5
        elif plot_num == 1:
            col = col2
            m_size = 4
        elif plot_num == 2:
            col = col3
            m_size = 3.5
        elif plot_num == 3:
            col = col4
            m_size = 3
        elif plot_num == 4:
            col = col5
            m_size = 3
        elif plot_num == 5:
            col = col6
            m_size = 3

    ############################################################################
    
        if PLOTTYPE =='DEN':
            fig = make_subplots(
                rows=1, cols=1,
                subplot_titles=(['Sampled Orbit Densities']),
                vertical_spacing = 0.15)

            model_m1 = self.__dict__['global_params']['den_model']
            print(model_m1)
            for ii,arc in enumerate(self.__dict__['global_params']['arc_input'][:]):


                #### INDEX THE DENSITY DF correctly
                vals  = np.arange(self.__dict__['Density'][arc].index[0],self.__dict__['Density'][arc].index[-1]+1)
                df = self.__dict__['Density'][arc].set_index('Date',drop=False ) 
                df['i_vals'] = vals
                index_date = df.loc[df.index.max()]['i_vals'].min()


                str_run_param = 'run_parameters'+ arc
                final_iter = self.__dict__[str_run_param]['str_iteration']
                i_arc = ii+1

                print('----',model_m1,'----')
                print('     mean:    ',np.mean(self.Density[arc]['rho (kg/m**3)']),'----')
                print('     variance:',np.std(self.Density[arc]['rho (kg/m**3)']),'----')
                print()

                if ii==0:
                    legend_flag = True
                    name_str    = model_m1+arc
                else:
                    legend_flag = False
                    name_str    = model_m1+arc


                fig.add_trace(go.Scattergl(  x=self.Density[arc]['Date'][:index_date][:],
                                             y=self.Density[arc]['rho (kg/m**3)'][:index_date][:],
                                             name= name_str,
                                             mode='markers',
                                             opacity=1,
                                             marker=dict(
                                                color=col, 
                                                size=m_size,
                                                        ),
                                             showlegend=legend_flag,
                                          ),
                                              secondary_y=False,
                                               row=1, col=1,
                                          )

            fig.update_layout(legend= {'itemsizing': 'constant'})
            fig.update_yaxes( title="rho (kg/m**3)", type='log', exponentformat= 'power',row=1, col=1)
            fig.update_xaxes( title="Date", row=1, col=1)       
            
            return(fig.show(config=config) )        
    ############################################################################

        elif PLOTTYPE =='DEN_orbavg':

            def orb_avg(den_df, arc):
                #### Find the index for the correct date
                vals  = np.arange(den_df[arc].index[0],den_df[arc].index[-1]+1)
                df = den_df[arc].set_index('Date',drop=False ) 
                df['i_vals'] = vals
                index_date = df.loc[df.index.max()]['i_vals'].min()

                lat = np.asarray(den_df[arc]['Lat'][:index_date])
                time_pd = pd.to_datetime(den_df[arc]['Date'][:index_date])
                i = np.nonzero( lat[1:]*lat[0:-1]  <  np.logical_and(0 , lat[1:] > lat[0:-1] )  )
                i = i[0]

                d_avg = np.zeros(np.size(i))
                height_avg = np.zeros(np.size(i))

                time_avg = []
                d_avg_rolling = []

                roll_avg_count = 0
                for j in range(np.size(i)-1):
                    d_avg[j]      = np.mean(den_df[arc]['rho (kg/m**3)'  ][i[j] : i[j+1]-1  ]  )
                    height_avg[j] = np.mean(den_df[arc]['Height (meters)'][i[j] : i[j+1]-1  ]  )
                    t1 = pd.to_datetime(time_pd[ i[j]    ])
                    t2 = pd.to_datetime(time_pd[ i[j+1]-1])
                    datemiddle = pd.Timestamp(t1) + (pd.Timestamp(t2) - pd.Timestamp(t1)) / 2

                    time_avg.append(datemiddle)

                    if roll_avg_count ==1:
                        d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))
                        roll_avg_count =0

                    roll_avg_count+=1 
                d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))

                return(time_avg, d_avg, d_avg_rolling )
            fig = make_subplots(
                rows=1, cols=1,
                subplot_titles=(['Orbit Averaged Densities']),
                vertical_spacing = 0.15)
            for ii,arc in enumerate(self.__dict__['global_params']['arc_input']):

                vals  = np.arange(self.__dict__['Density'][arc].index[0],self.__dict__['Density'][arc].index[-1]+1)
                df = self.__dict__['Density'][arc].set_index('Date',drop=False ) 
                df['i_vals'] = vals
                index_date = df.loc[df.index.max()]['i_vals'].min()


                time_avg,d_avg, d_avg_rolling = orb_avg(self.Density, arc)


                fig.add_trace(go.Scattergl(x=time_avg,
                                         y=d_avg_rolling,
        #                                  y=d_avg,
        #                                 name= ' Arc ' +str(i_arc) ,
                                         mode='markers',
                                         marker=dict(
                                         color=col,
                                         size=7,),
                                         showlegend=False,
                                           ),
                                           row=1, col=1,
                                           )


                fig.update_yaxes(type="log", exponentformat= 'power',row=1, col=1)
            fig.update_xaxes(title_text="Date", row=1, col=1)
            fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
            fig.update_layout(legend= {'itemsizing': 'constant'})


            
            return(fig.show(config=config) )        
            
    ############################################################################

        elif PLOTTYPE =='NTW_residuals':
            
            coords = ['N','T','W']

            fig = make_subplots(
                rows=3, cols=1,
                subplot_titles=(['Normal (N)', 'In-track (T)','Cross-Track (W)']),
                vertical_spacing = 0.15)

            model_m1 = self.__dict__['global_params']['den_model']
            for ii,arc in enumerate(self.__dict__['global_params']['arc_input']):

                data_resids = self.__dict__['OrbitResids'][arc]['resids']


                fig.update_layout(title=''.join(coords)+' Orbit Residuals',
                                autosize=True,
                                font=dict(size=14),
                                legend= {'itemsizing': 'constant'})

                for i,val in enumerate(coords):
                    fig.add_trace(go.Scattergl(
                                             x=data_resids['Date'][::10],
                                             y=data_resids[val][::10],
                                         name=val+' '+model_m1,
                                         mode='markers',
                                         marker=dict(
                                             color=col,
                                             size=m_size,
                                                    ),
                                         showlegend=False),
                                         secondary_y=False,
                                         row=i+1, col=1)



            fig.update_yaxes( title="meters" ,type="linear" , exponentformat= 'power',row=1, col=1)
            fig.update_yaxes( title="meters" ,type="linear" , exponentformat= 'power',row=2, col=1)
            fig.update_yaxes( title="meters" ,type="linear" , exponentformat= 'power',row=3, col=1)
            fig.update_xaxes( title="Date",  row=3, col=1)

            
            
            return(fig.show(config=config) )        
