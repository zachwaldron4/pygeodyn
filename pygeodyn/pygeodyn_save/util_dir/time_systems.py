import pandas as pd


def mjds_to_ymdhms(input_ModJulianDay_secs):
    '''
    This function takes modified julian day seconds (MJDS) as input 
    and returns a date string in the format YYMMDDHHMMSS.
    '''

    #########################################
    # Define some constants
    SECDAY              = 86400
    geodyn_ref_time_mjd = 30000
    jd_0                = 2400000.5
    d36525              = 365.25
    d122                = 122.1
    d30600              = 30.6001
    half                = 0.5
    ib                  = -15
    d17209              = 1720996.5

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
    JD = MJD + jd_0                  #  Convert to JulianDay
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


    ##### Calculate Hours, Minutes, seconds
    isec_mjd  =  MJDS % 86400

    ihour    = isec_mjd/3600
    iminutes = (ihour % 1)*60
    isec     = (iminutes % 1)*60 

    ihour_str     = str(int((ihour)))
    iminutes_str  = str(int((iminutes)))
    isec_str      = str(int(round(isec)))

    if len(ihour_str)==1:
        ihour_str = '0'+ihour_str
    if len(iminutes_str)==1:
        iminutes_str = '0'+iminutes_str
    if len(isec_str)==1:
        isec_str = '0'+isec_str

    #hhmmss  =  int((ihour*10000) + (iminutes*100) + isec)
    hhmmss  =  ihour_str + iminutes_str + isec_str
    YYMMDDHHMMSS = str(yymmdd) + '-' + str(hhmmss)

    return(YYMMDDHHMMSS)






def time_tdt_to_utc(terrestrial_time_mjdsec, leap_seconds):
    '''
    ET is Ephemeris Time and has been numerically equivalent to 
    Terrestral Time (TT) or Terrestral Dynamic Time (TDT) since ~1975.

    TT is distinct from the time scale often used as a basis for civil
    purposes, Coordinated Universal Time (UTC). TT is indirectly the 
    basis of UTC, via International Atomic Time (TAI).
     
 
    Because of the historical difference between TAI and ET 
    when TT was introduced, TT is approximately 32.184 s
    ahead of TAI.

        ??ET - A1 = 32.1496183??

        TDT = TAI + 32.184  
        TAI = UTC + dAT  
            where dAT is the total algebraic sum of leap seconds 

            As of 1 January 2017,
            TAI is ahead of UTC   by 37 seconds.
            TAI is ahead of GPS   by 19 seconds.
            GPS is ahead of UTC   by 18 seconds.

    Convert ET to UTC:
        UTC  =  TT - dAT - 32.184 s  
     '''
    
    TT  = terrestrial_time_mjdsec
    dAT = leap_seconds

    UTC = TT - dAT - 32.184
    mjdsecs_UTC = UTC

    return(mjdsecs_UTC)






def time_gps_to_tdt(tim_gps, leap_sec):
    r"""Convert from GPS time to TDT
            TDT -> Terrestral Dynamic Time
            GPS -> GPS Time
    
    dAT = 37
    TDT = TAI + 32.184  
    TAI = UTC + dAT  
        where dAT is the total algebraic sum of leap seconds 
    As of 1 January 2017,
        TAI is ahead of UTC   by 37 seconds.
        TAI is ahead of GPS   by 19 seconds.
        GPS is ahead of UTC   by 18 seconds.
    Convert ET to UTC:
        UTC  =  TT - dAT - 32.184 s 
        
    Args:
        tim_gps (pandas datetime): pandas datetime GPS date.
        leap_sec (int): integer leap seconds as of 2017 is 37 sec.

    Returns:
        tim_tdt (pandas datetime): pandas datetime TDT date.
    """

#     dAT = 37  # total algebraic sum of leap seconds 
    leap_sec = pd.to_timedelta(leap_sec,'s')

    #### GPS to UTC---gps ahead of utc by 18 secs. 
    tim_utc = tim_gps + pd.to_timedelta(18,'s')

    ### UTC to TDT---utc ahead of tdt by 32.184
    tim_tdt = tim_utc+leap_sec+pd.to_timedelta(32.184,'s')

    return(tim_tdt)



# !     YMD TO MJD
# !
#   200 CONTINUE
#       IY=IYMD/10000
#       IM=(IYMD-IY*10000)/100
#       ID=IYMD-IY*10000-IM*100
# !
# !
#       if( iy .lt. 1900 ) then
#          if( iy .gt. 50 ) then
#             y = 1900 + iy - 1
# !     write (6,*)'in 200 block: y ', y
#          else
#             y = 2000 + iy - 1
# !     write (6,*)'in 200 block: y ', y
#          endif
#       endif
# !
#       if( im .gt. 2 ) then
#          m = im
# !   write (6,*)'in 200 block: m ', m
#          y = y + 1
#       else
#          m = im + 12
# !   write (6,*)'in 200 block: m ', m
#       endif
# !
#       xjd = INT( d36525 * y ) + INT( d30600 * (m + 1) ) + ib            &
#      &    + d17209 + id
# !     write(6,*) ' xjd ', xjd
# !
#       modjd = xjd - xjd0
# !     write(6,*) ' modjd ', modjd
# !
# !     ....below line not needed for iflag=2
# !cc      fsec = ih * d3600 + imin * d60 + sec
# !
# !     write(6,*) ' iy, im, id ', iy, im, id, '  mjd ', mjd
# !
#       RETURN



