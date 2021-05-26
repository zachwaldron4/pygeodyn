

def MJDS_to_YYMMDDHHMMSS(input_ModJulianDay_secs):
    '''
    This function takes modified julian day seconds (MJDS) as input 
    and returns a date_string in the format YYMMDDHHMMSS.
    '''

    ### Modified Julian Date conversion
    # *  MODIFIED JULIAN DAY = JULIAN DAY - GEODYN REFERENCE TIME IN JD

    #########################################
    # Define some constants
    SECDAY              = 86400
    geodyn_ref_time_mjd = 30000
    jd_0                = 2400000.5
    d36525              = 365.25
    d122                = 122.1
    d30600              = 30.6001
    half                = 0.5
    ib = -15
    d17209 = 1720996.5

    ######  CONVERT FROM MJDS TO MJD
    # Inputs:
    MJDS = input_ModJulianDay_secs
    #
    MJD = (MJDS/SECDAY) + geodyn_ref_time_mjd

    ######  CONVERT FROM MJD TO YMD
    # Note from zach-- I took this calculation from geodyn...
    # There is more going on here than I understand, 
    # but I want to stay on their level of accuracy
    #
    JD = MJD + jd_0                  # Convert to JulianDay
    c  = int( JD + half ) + 1537     # ??   sorry, i'm   ??
    nd = int( (c - d122) / d36525 )  # ??   not sure     ??
    e  = int( d36525 * nd )          # ??   what this    ??
    nf = int( ( c - e ) / d30600 )   # ??   all is       ??
    # ----
    frac = (JD + half) - int( JD + half )           # frac of day leftover
    iday = c - e - int( d30600 * nf ) + frac        # day
    imonth  = nf -  1   - 12 * int( nf / 14 )       # month
    iyyyy = nd - 4715 - int(  ( 7 + imonth ) / 10 ) # YYYY
    #
    ##### Use modular division to get 2 digit year
    iyear =  iyyyy % 100 
    #
    #### Return YYMMDD 
    yymmdd = int(iyear * 10000 + imonth * 100 + iday)



#     ##### Calculate Hours, Minutes, seconds
    
    
    ##### Calculate Hours, Minutes, seconds
    isec_mjd  =  MJDS % 86400

    ihour     = isec_mjd/3600
    iminutes = (ihour % 1)*60
    isec = (iminutes % 1)*60
    
    isec_str      = str(int(isec))
    ihour_str = str(int(ihour))
    iminutes_str  = str(int(iminutes))

    if len(ihour_str)==1:
        ihour_str = '0'+ihour_str
    if len(iminutes_str)==1:
        iminutes_str = '0'+iminutes_str
    if len(isec_str)==1:
        isec_str = '0'+isec_str

    #hhmmss  =  int((ihour*10000) + (iminutes*100) + isec)
    hhmmss  =  ihour_str + iminutes_str + isec_str

#     print('mjd',MJDS)
#     print('isec_mjd',isec_mjd)
#     print('--')
#     print('ihour', str(ihour))
#     print('ihourstr', ihour_str)
#     print()
#     print('iminutes', iminutes)
#     print('iminutes_str', iminutes_str)
#     print()
#     print('isec', isec)
#     print('isec_str', isec_str)
#     print('HHMMSS', str(hhmmss))
#     print()
#     print('------------------------')

    
    
    
    
    
    
    
    
    
    YYMMDDHHMMSS = str(yymmdd) + '-' + str(hhmmss)
#     print(MJDS)
#     print('YYMMDD', str(yymmdd))
#     print('HHMMSS', str(hhmmss))
#     print()
    return(YYMMDDHHMMSS)






###### Some extra stuff from that code that seems wrong...

#     if iyear < 1900:
#         if iyear > 50:
#             y = 1900 + iyear - 1
#         else:
#             y = 2000 + iyear - 1
#     if imonth > 2:
#         m = imonth
#         y = y + 1
#     else:
#         m = imonth + 12

#     xjd = int( d36525 * y ) + int( d30600 * (m + 1) ) + ib   + d17209 + iday
#     modjd = xjd - jd_0


