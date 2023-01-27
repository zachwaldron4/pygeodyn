def write_EXAT_binary(filename, params, quat_xyzw, quat_dates, writetxt=False):
    """Write the GEODYN external attitude file from satellite quaternions.

        filename
        params
        quat_xyzw
        quat_dates
    """
    
    from scipy.io import FortranFile
    import pandas as pd
    import numpy as np
    import sys
    
    f = FortranFile(filename, 'w')
    ### ----------------------------------------------------------------
    ###     1) GENERAL HEADER RECORD  
    ### ----------------------------------------------------------------
    ###     Read the first record, this is the header buffer
    ###     Use 64-bit float datatype. 
    ###     Each record contains 9, 64-bit words.
    ### ----------------------------------------------------------------
    record1_HeaderGeneral = [
               -6666666.0,  #   1--- Record Indicator #(-6666666.00)
        params['version'],  #   2--- Version Number
        params['num_sat'],  #   3--- Number of Satellites
                  0.0,      #   Not  used at  present  time
                  0.0,      #   Not  used at  present  time
                  0.0,      #   Not  used at  present  time
                  0.0,      #   Not  used at  present  time
                  0.0,      #   Not  used at  present  time
                  0.0,      #   Not  used at  present  time
                    ]

    ### ----------------------------------------------------------------
    ###     2) SATELLITE INFORMATION RECORD                
    ### ----------------------------------------------------------------
    ###     The number of these records equals the number of 
    ###     satellites from the General Header Record.  
    ###     All of these records must directly follow the 
    ###     above General Header Record.
    ### ----------------------------------------------------------------
    record2_HeaderSatInfo = [
            - 7777777.0    ,  # 1--- Record Indicator  (-7777777.00)
                   0.0     ,  #      Not used at present time
      params['SATID']      ,  # 3--- Satellite ID
      params['interval']   ,  # 4--- Interval          (SSSSS.SSSSS)
      params['startEpoch'] ,  # 5--- Start time        (YYMMDDHHMMSS.00)
      params['startFrac_S'],  # 6--- Start frac secs   (00.SS)
      params['stopEpoch']  ,  # 7--- Stop time         (YYMMDDHHMMSS.00) 
      params['stopFrac_S'] ,  # 8--- Stop frac secs    (00.SS)
      params['qqqpppaaa']  ,  # 9--- QQQPPPAAA 
                # QQQ is the total # of separate movable panels   
                #      + antenna quaternion sets for this satellite 
                #      (excludes SBF to J2000 quaternion set which is 
                #      mandatory for each satellite in the file).
                # PPP is the number of movable panels represented for 
                #      this satellite.
                # AAA is the number of moveable antenna represented for 
                #      this satellite.  One quaternion set may represent 
                #      the attitude for up to two movable panels and 
                #      one movable antenna. 
                    ]

    ### ----------------------------------------------------------------
    ###     3) QUATERNION SET HEADER RECORD  
    ### ----------------------------------------------------------------
    ###     This header record must precede the quaternion 
    ###     data records for a a particular set of quaternions.
    ### ----------------------------------------------------------------
    record3_HeaderQuatInfo = [
            -8888888.0      ,  # 1 --- Record Indicator (-8888888.00)
         params['SATID']    ,  # 2 --- Satellite ID
         params['panel_num'],  # 3 --- Panel # (MMMNNN)  
                                           # (0 if not moveable 
                                           # MMM is 1st panel 
                                           # NNN is 2nd panel number)
                    0.        , # 4 --- AntennaLink number
         params['startEpoch'] , # 5 --- Start time      (YYMMDDHHMMSS.00)
         params['startFrac_S'], # 6 --- Start frac secs (00.SS)
         params['stopEpoch']  , # 7 --- Stop time       (YYMMDDHHMMSS.00) 
         params['stopFrac_S'] , # 8 --- Stop frac secs  (00.SS)
         params['interval']   , # 9 --- Interval        (SSSSS.SSSSS) 
                    ]

    
    date_check_start=float(pd.to_datetime(
                            quat_dates[0],format='%Y-%m-%d %H:%M:%S'
                            ).strftime(format='%y%m%d%H%M%S'))
    date_check_stop=float(pd.to_datetime(
                            quat_dates[-1],format='%Y-%m-%d %H:%M:%S'
                            ).strftime(format='%y%m%d%H%M%S'))
    
    if date_check_start != params['startEpoch']:
        f.close
        print("Error while writing EXAT file.  Start dates don't match.")
        print('   Expecting:',date_check_start    )
        print('   Received :',params['startEpoch'])
        sys.exit(0)
    if date_check_stop != params['stopEpoch']:
        f.close
        print("Error while writing EXAT file.  Stop dates don't match.")
        print('   Expecting:',date_check_stop    )
        print('   Received :',params['stopEpoch'])
        sys.exit(0)
    
    
    #### Write the above records.
    f.write_record(np.array(record1_HeaderGeneral , dtype=float))
    f.write_record(np.array(record2_HeaderSatInfo , dtype=float))
    f.write_record(np.array(record3_HeaderQuatInfo, dtype=float))


    ### ----------------------------------------------------------------
    ###     4) DATA RECORD  
    ### ----------------------------------------------------------------
    ###     These records apply to a particular quaternion set
    ###     and must follow (in time ascending order) the 
    ###     quaternion set header record. 
    ### ----------------------------------------------------------------
    ###        Notes from Vol5 document:
    ###        1) All quaternion sets (SBFtoJ2000, or MVPtoSBF, 
    ###            or MVAtoSBF) for a particular satellite ID must have 
    ###            the same interval, start, and stop times.
    ###        2) Quaternion info  must include 10 Integration steps 
    ###           before & after the EPOCH of the satellite in the run.
    ###        3) q1=-9999999.000 indicates quaternion information is
    ###            missing for this time point. GEODYN will use the 
    ###            internal attitude module if selected.
    ###        4) A panel or antenna must not use designation # of 0. 
    ###        5) GEODYN's quaternion definition is Eulerian. Therefore, 
    ###           the scalar argument is defined as positive. Also, 
    ###           the quaternions should be provided without any 
    ###           discontinuities to provide for proper interpolation.
    for i, val in enumerate(quat_xyzw):
        record4_Data = [0. ,     #       Not used at present time
                        0. ,     #       Not used at present time
            quat_xyzw[i,0] ,     # 3 --- Q1      [  sin (/2)n1  ]
            quat_xyzw[i,1] ,     # 4 --- Q2      [  sin (/2)n2  ]
            quat_xyzw[i,2] ,     # 5 --- Q3      [  sin (/2)n3  ]
            quat_xyzw[i,3] ,     # 6 --- Q4      [  cos (/2)    ]
                        0. ,     #       Not used at present time
                        0. ,     #       Not used at present time
                        0. ,     #       Not used at present time
                        ]
        f.write_record(np.array(record4_Data, dtype=float))

    print('Reached end of attitude data.  Closing the File')
    f.close()

    if writetxt:
        #'/data/SatDragModelValidation/notebooks/O2R_spire/exat.txt'
        filetxt = filename+'_check.txt'
        print("Saving external attitude as a text file")
        print("    filetxt" ,filetxt )

        f = open(filetxt, "w")
        f.write("\n")
        f.close()

        with open(filetxt, 'r+') as file:
            file.write('# '+ ', '.join([str(i) for i in record1_HeaderGeneral])  + '\n'  )
            file.write('# '+ ', '.join([str(i) for i in record2_HeaderSatInfo])  + '\n'  )
            file.write('# '+ ', '.join([str(i) for i in record3_HeaderQuatInfo]) + '\n' )
            file.write('# ------------------------------------------------------'+'\n' )
       
            startDT = pd.to_datetime(params['startEpoch'], format='%y%m%d%H%M%S')#'%Y-%m-%d %H:%M:%S')
            stopDT  = pd.to_datetime(params['stopEpoch'],  format='%y%m%d%H%M%S')#'%Y-%m-%d %H:%M:%S')
            
            freq_str = str(int(params['interval']))+"S"
            times_linspace = pd.date_range(start=startDT, end=stopDT, freq=freq_str)

            print('startDT', startDT)
            print('stopDT', stopDT)
            print(times_linspace)

            print(len(times_linspace))
            print(len(quat_xyzw))
        
            for i, val in enumerate(quat_xyzw):
                record4_Data = [#0. ,     #       Not used at present time
                                #0. ,     #       Not used at present time
                    times_linspace[i],
                    quat_xyzw[i,0] ,     # 3 --- Q1      [  sin (/2)n1  ]
                    quat_xyzw[i,1] ,     # 4 --- Q2      [  sin (/2)n2  ]
                    quat_xyzw[i,2] ,     # 5 --- Q3      [  sin (/2)n3  ]
                    quat_xyzw[i,3] ,     # 6 --- Q4      [  cos (/2)    ]
                                #0. ,     #       Not used at present time
                                #0. ,     #       Not used at present time
                                #0. ,     #       Not used at present time
                                ]
                
                file.write(', '.join([str(i) for i in record4_Data]) + '\n' )


    return



















def read_EXTAT_binary():
    """
        OVERVIEW  
            External Attitude File is a binary file where all records contain 9, 64-bit real words
            +-----------------------------------------------------------
            |Nomenclature
            |    SBF   --> Spacecraft  Body  Fixed  Frame
            |    J2000 --> J2000  Earth  Equator  and  Equinox  Frame
            |    MVP   -->  Movable  Panel  Frame
            |    MVA   -->  Movable  Antenna  Frame
            |        ("Movable" is with  respect  to the SBF  frame)
            |        (All  times in this  file  are TDT or TDB)
            |              TDT (Terrestrial  Dynamic  Time)
            |              TDB (Barycenter   Dynamic  Time)
            |              GPS (Global  Positioning  System Time)
            +------------------------------------------------------------

        The file is segmented as follows:
            1) GENERAL HEADER RECORD
            2) SATELLITE INFORMATION HEADER RECORDS
            3) QUATERNION  SET  HEADER  RECORD
            4) DATA RECORDS



                1)  GENERAL HEADER RECORD
                    --------------------- 
                    There is only one of these records and it must be first in the file.
                -----------------------------------------------------------------------
                WORD         DESCRIPTION
                ----------------------------------------------------------------------- 
                1          Record Indicator                              -6666666.00
                2          Version Number
                3          Number of Satellites represented in this file.
                4          Not used at present time
                5          Not used at present time
                6          Not used at present time
                7          Not used at present time
                8          Not used at present time
                9          Not used at present time


                2)  SATELLITE INFORMATION HEADER RECORDS
                    ------------------------------------ 
                    The number of these records equals the number of satellites from the 
                    General Header Record. All of these records must directly follow the
                    above General Header Record.
                -----------------------------------------------------------------------
                WORD         DESCRIPTION
                ----------------------------------------------------------------------- 
                1           Record Indicator                      -7777777.00
                2           Not used at present time              
                3           Satellite ID*                         
                4           Interval                              SSSSS.SSSSS
                5           Start time                            YYMMDDHHMMSS .00
                6           Start (fractional seconds)            00.SS
                7           Stop time                             YYMMDDHHMMSS .00
                8           Stop (fractional seconds)             00.SS
                9           No. of: panels+antenna separate       QQQPPPAAA **
                                quaternion sets/movable panels
                                represented/movable antenna 
                                represented        

                3)  QUATERNION SET HEADER RECORD
                    ----------------------------
                    This header record must precede the quaternion data 
                    records for a particular set of quaternions.
                -----------------------------------------------------------------------
                WORD           DESCRIPTION
                -----------------------------------------------------------------------
                1           Record  Indicator                    -8888888.00
                2           Satellite  ID
                3           Panel  Number*                        MMMNNN
                4           Antenna -Link  Number ***
                5           Start  time**                         YYMMDDHHMMSS .00
                6           Start (fractional  seconds )**        00.SS
                7           Stop  time**                          YYMMDDHHMMSS .00
                8           Stop (fractional  seconds )**         00.SS
                9           Interval **                           SSSSS.SSSSS

    """
    from scipy.io import FortranFile


    AttitudeFile = '/data/zach_work/O2R_spire/EXAT01_icesat2.2018.313'
    f = FortranFile(AttitudeFile, 'r')

    sp = '    '
    header= {}

    ### ----------------------------------------------------
    ### ------------- 1) GENERAL HEADER RECORD -------------
    ###
    ###      Read the first record, this is the header buffer
    ####     Use 64-bit float datatype. Each record contains 9, 64-bit words
    a = f.read_record(float)  

    if a[1 -1] == -6666666.00:
        print('Reading GENERAL HEADER RECORD')

        header['NSATS']           = a[3 -1]  # Number of Satellites represented in this file.
        print(sp, 'NSATS =', header['NSATS'] )
    for sats in np.arange(1, int(header['NSATS'])+1):
        header['SATINFO_'+f"{int(sats):02}"] = {}
    #
    #
    ### ----------------------------------------------------
    ### ---------- 2) SATELLITE INFORMATION RECORD ---------
    ###  
    a = f.read_record(float)
    if a[1 -1] == -7777777.00:
        print('Reading SATELLITE INFORMATION HEADER RECORD')

        for i in np.arange(1, int(header['NSATS'])+1):
            iSat = 'SATINFO_'+f"{int(i):02}"

            header[iSat]['SATID']                             = a[3 -1]  
            header[iSat]['Interval (SSSSS.SSSSS)']            = a[4 -1]  
            header[iSat]['Start time (YYMMDDHHMMSS.00)']      = a[5 -1]  
            header[iSat]['Start (fractional seconds, 00.SS)'] = a[6 -1] 
            header[iSat]['Stop time (YYMMDDHHMMSS.00)']       = a[7 -1] 
            header[iSat]['Stop (fractional seconds, 00.SS)']  = a[8 -1]  
            header[iSat]['QQQPPPAAA']                         = a[9 -1]  

            datestart_string = str(header[iSat]['Start time (YYMMDDHHMMSS.00)']).split('.')[0]
            datestop_string  = str(header[iSat]['Stop time (YYMMDDHHMMSS.00)']).split('.')[0]
            date_start       = pd.to_datetime(datestart_string, format='%y%m%d%H%M%S')
            date_stop        = pd.to_datetime(datestop_string, format='%y%m%d%H%M%S')

            print(sp,'Start:',date_start)
            print(sp,'Stop:' ,date_stop)
            print(sp,'Interval:',header[iSat]['Interval (SSSSS.SSSSS)'])

            print(sp,'QQQPPPAAA', a[9 -1])
                # QQQ is the total # of separate movable panels + antenna quaternion  
                #     sets for this satellite (excludes SBF to J2000 quaternion set  
                #     which is mandatory for each satellite represented in the file).   
                # PPP is the number of movable panels represented for this satellite. 
                # AAA is the number of moveable antenna represented for this  satellite.
                #     One quaternion set may represent the attitude for up to two  
                #     movable panels and one movable antenna.                


    #
    #
    ### -------------------------------------------------------
    ### ---------- 3) QUATERNION SET HEADER RECORD  --------
    ###  
    ###       This header record must precede the 
    ###            quaternion data records for a 
    ###            particular set of quaternions.


    def read_QUAT_SET_HEADER(set_count, header, a):
        if a[1 -1] ==  -8888888.00:
    #         print(a)
            set_count+=1
            iQuat = "QuatSet"+str(set_count)
            print('Reading QUATERNION SET HEADER RECORD', str(set_count))

            header[iQuat] = {}

            header[iQuat]['SATID']                            = a[2 -1]  
            header[iQuat]['Panel # (MMMNNN)']                 = a[3 -1] 
                #    MMM is 1st panel  number  and 
                #    NNN is 2nd panel  number
            header[iQuat]['AntennaLink #']                    = a[4 -1]  
                #Antenna -Link  number  is used to  specify  both  the  antenna  and  linknumber(see  table  below ).
            ####
            ###  Qaternion  satellite  information  must  match  
            ###  that on thes satellite  information  header  record.
            header[iQuat]['Start time (YYMMDDHHMMSS.00)']      = a[5 -1]  
            header[iQuat]['Start (fractional seconds, 00.SS)'] = a[6 -1] 
            header[iQuat]['Stop time (YYMMDDHHMMSS.00)']       = a[7 -1] 
            header[iQuat]['Stop (fractional seconds, 00.SS)']  = a[8 -1]  
            header[iQuat]['Interval (SSSSS.SSSSS)']            = a[9 -1]  

        else:
            print(a)
            f.close()  #### be sure to close the file
            sys.exit(0)
        return(set_count, header)


    #### Read the first header record
    a = f.read_record(float)
    set_count = 0
    (set_count, header) = read_QUAT_SET_HEADER(set_count, header, a)



    def read_QUAT_DATA(a, data, set_count, header):
        print("Reading Data for set:", set_count)
        data[set_count] = {}
        data[set_count]['q1'] = []
        data[set_count]['q2'] = []
        data[set_count]['q3'] = []
        data[set_count]['q4'] = []

        while a[1 -1] ==  0:
            data[set_count]['q1'].append(a[3 -1])  ###  sin (/2)n1
            data[set_count]['q2'].append(a[4 -1])  ###  sin (/2)n2
            data[set_count]['q3'].append(a[5 -1])  ###  sin (/2)n3
            data[set_count]['q4'].append(a[6 -1])  ###  cos (/2)

            ### Move to next data record while in the loop
            a = f.read_record(float)
        else:
            if a[1 -1] !=  0:
    #             print('Back to a header?')
                (set_count, header) = read_QUAT_SET_HEADER(set_count, header, a)
                try:
                    a = f.read_record(float)
                    (a, data, set_count) = read_QUAT_DATA(a, data, set_count, header)

                except:
                    print('End of File')
                    f.close()  #### be sure to close the file

        return(a, data, set_count)



    ### Read the first Data record and then open the loop until the end of file
    a = f.read_record(float)
    data = {}
    (a, data, set_count) = read_QUAT_DATA(a, data, set_count, header)
    f.close()  #### be sure to close the file




    ## attempt to clear out some memory
    f         = 0
    a         = 0
    set_count = 0

    return()