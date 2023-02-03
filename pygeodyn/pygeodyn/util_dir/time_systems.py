import pandas as pd
import numpy as np


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




def ymdhms_to_mjds(year, month, day, hour, minute, second):
    '''convert calendar date to MJDS

    This function takes year, month, day, hour, minute, second as input 
    and returns modified julian day seconds (MJDS).
    '''

    from pygeodyn.util_dir.time_systems import jday

    SECDAY               = 86400   #seconds in a day
    geodyn_ref_time_mjd  = 30000   # ref time in MJD

    
    jd , jdfrac = jday(year, month, day, hour, minute, second)
    mjd = ((jd+jdfrac) - 2400000.5)  
    mjdsec = (mjd - geodyn_ref_time_mjd ) * SECDAY

    return(mjdsec)
    #print(f"")
    #print(f'mjd                :  {mjd}')
    #print(f'mjdsec             :  {mjdsec}')




def get_leapseconds(year, mon, day):
    """ Get leap seconds for given date (dAt = tai-utc)

    Arg:
        mjd (float): - Modified Julian Days

    Output:
        dAT (float)  - leap seconds (tai-utc)
    
    Comments:
        - Source: https://www.ietf.org/timezones/data/leap-seconds.list  
        - dict format: leapsecond_table[MJD: [leapsec, ntp_timestamp]]  

    Notes:
        #	The last column shows an epoch as a number of seconds
        #	since 1 January 1900, 00:00:00 (1900.0 is also used to
        #	indicate the same epoch.) Both of these time stamp formats
        #	ignore the complexities of the time scales that were
        #	used before the current definition of UTC at the start
        #	of 1972. (See note 3 below.)
        #	The first column shows the number of seconds that
        #	must be added to UTC to compute TAI for any timestamp
        #	at or after that epoch. The value on each line is
        #	valid from the indicated initial instant until the
        #	epoch given on the next one or indefinitely into the
        #	future if there is no next line.

        ts_unix_epoch =-2208988800 # unix timestamp 1 January 1900, 00:00:00
        print(datetime.datetime.fromtimestamp(\
                    ts_unix_epoch + ntp_timestamp\
                    ).strftime('%Y-%m-%d %H:%M:%S'))
    """

    jd, jdfrac = jday(year, mon, day,0,0,0)
    mjd =  jd+jdfrac-2400000.5
    
    # dat = get_leapseconds(jd+jdfrac-2400000.5)


    leapsecond_table={41317.0 : [10 ,'1972-01-01 00:00:00', 2272060800], 
                      41499.0 : [11 ,'1972-07-01 00:00:00', 2287785600], 
                      41683.0 : [12 ,'1973-01-01 00:00:00', 2303683200], 
                      42048.0 : [13 ,'1974-01-01 00:00:00', 2335219200], 
                      42413.0 : [14 ,'1975-01-01 00:00:00', 2366755200], 
                      42778.0 : [15 ,'1976-01-01 00:00:00', 2398291200], 
                      43144.0 : [16 ,'1977-01-01 00:00:00', 2429913600], 
                      43509.0 : [17 ,'1978-01-01 00:00:00', 2461449600], 
                      43874.0 : [18 ,'1979-01-01 00:00:00', 2492985600], 
                      44239.0 : [19 ,'1980-01-01 00:00:00', 2524521600], 
                      44786.0 : [20 ,'1981-07-01 00:00:00', 2571782400], 
                      45151.0 : [21 ,'1982-07-01 00:00:00', 2603318400], 
                      45516.0 : [22 ,'1983-07-01 00:00:00', 2634854400], 
                      46247.0 : [23 ,'1985-07-01 00:00:00', 2698012800], 
                      47161.0 : [24 ,'1988-01-01 00:00:00', 2776982400], 
                      47892.0 : [25 ,'1990-01-01 00:00:00', 2840140800], 
                      48257.0 : [26 ,'1991-01-01 00:00:00', 2871676800], 
                      48804.0 : [27 ,'1992-07-01 00:00:00', 2918937600], 
                      49169.0 : [28 ,'1993-07-01 00:00:00', 2950473600], 
                      49534.0 : [29 ,'1994-07-01 00:00:00', 2982009600], 
                      50083.0 : [30 ,'1996-01-01 00:00:00', 3029443200], 
                      50630.0 : [31 ,'1997-07-01 00:00:00', 3076704000], 
                      51179.0 : [32 ,'1999-01-01 00:00:00', 3124137600], 
                      53736.0 : [33 ,'2006-01-01 00:00:00', 3345062400], 
                      54832.0 : [34 ,'2009-01-01 00:00:00', 3439756800], 
                      56109.0 : [35 ,'2012-07-01 00:00:00', 3550089600], 
                      57204.0 : [36 ,'2015-07-01 00:00:00', 3644697600], 
                      57754.0 : [37 ,'2017-01-01 00:00:00', 3692217600], 
                     }
    keys = list(leapsecond_table.keys())
    for i,imjd in enumerate(keys):
        if imjd<mjd:
            pass
        if imjd>mjd:
            #print(leapsecond_table[keys[i-1]])
            dAT = leapsecond_table[keys[i-1]][0]
            break
        if mjd >= keys[-1]:
            leapsecond_table[keys[-1]]
            dAT = leapsecond_table[keys[-1]][0]
 
    return(float(dAT))



def time_tdt_to_utc(terrestrial_time_mjdsec, leap_seconds):
    """
    time_tdt_to_utc Converts time TDT (TT) to UTC

    Converts the time system from terrestrial dynamic time (TDT, TT) to
    universal coordinated time.  ET is Ephemeris Time and has been 
    numerically equivalent to Terrestral Time (TT) or Terrestral Dynamic
    Time (TDT) since ~1975. TT is distinct from the time scale often 
    used as a basis for civil purposes, Coordinated Universal Time (UTC).
    TT is indirectly the basis of UTC, via International Atomic Time (TAI).
    Because of the historical difference between TAI and ET when TT 
    was introduced, TT is approximately 32.184secs ahead of TAI.
        ??ET - A1 = 32.1496183??
        TDT = TAI + 32.184  
        TAI = UTC + dAT  
            where dAT is the total algebraic sum of leap seconds 
            As of 1 January 2017,
            TAI is ahead of UTC   by 37 seconds.
            TAI is ahead of GPS   by 19 seconds.
            GPS is ahead of UTC   by 18 seconds.
        Convert ET to UTC: UTC  =  TT - dAT - 32.184 s  
    
    Args:
        terrestrial_time_mjdsec (_type_): _description_
        leap_seconds (_type_): _description_
    """
    '''
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



def time_gps_to_utc(tim_gps, leap_sec):
    r"""Convert from GPS time to UTC
            GPS -> GPS Time
            UTC -> Universal Coordinated Time
            
    Args:
        tim_gps (pandas datetime): pandas datetime GPS date.
        leap_sec (int): integer leap seconds as of 2017 is 37 sec.

    Returns:
        tim_utc (pandas datetime): pandas datetime universal coord. time date.
    """

#     dAT = 37  # total algebraic sum of leap seconds 
    leap_sec = pd.to_timedelta(leap_sec,'s')

    #### GPS to UTC---gps ahead of utc by 18 secs. 
    tim_utc = tim_gps + pd.to_timedelta(18,'s')
    return(tim_utc)

def time_utc_to_gps(tim_utc, leap_sec):
    r"""Convert from UTC time to GPS time
            GPS -> GPS Time
            UTC -> Universal Coordinated Time
            
    Args:
        tim_utc (pandas datetime): pandas datetime universal coord. time date.
        leap_sec (int): integer leap seconds as of 2017 is 37 sec.

    Returns:
        tim_gps (pandas datetime): pandas datetime GPS date.
    """

    leap_sec = pd.to_timedelta(leap_sec,'s')

    #### utc to gps---gps ahead of utc by 18 secs. 
    tim_gps = tim_utc - pd.to_timedelta(18,'s')
    return(tim_gps)





def jday(yr, mon, day, hr, minute, sec):
    """find the julian date given the year, month, day, and time.

        author        : david vallado                27 may 2002
        revisions     : Zach Waldron, converted to python, January 2023

        inputs          description                    range / units
            year        - year                           1900 .. 2100
            mon         - month                          1 .. 12
            day         - day                            1 .. 28,29,30,31
            hr          - universal time hour            0 .. 23
            min         - universal time min             0 .. 59
            sec         - universal time sec             0.0 .. 59.999
            whichtype   - julian .or. gregorian calender   'j' .or. 'g'

        outputs    
            jd          - julian date                    days from 4713 bc
            jdfrac      - julian date fraction of a day   0.0 to 1.0

    """
    
    #### ------------------------  implementation   ------------------
    jd = 367.0 * yr \
         - np.floor( (7 * (yr + np.floor( (mon + 9) / 12.0) ) ) * 0.25 ) \
         + np.floor( 275 * mon / 9.0 ) \
         + day + 1721013.5   # % use - 678987.0 to go to mjd directly
    jdfrac = (sec + minute * 60.0 + hr *3600.0) / 86400.0

    ### check jdfrac
    if jdfrac > 1.0: 
        jd = jd + np.floor(jdfrac)
        jdfrac = jdfrac - np.floor(jdfrac)
        
    # %  - 0.5 * sign(100.0 * yr + mon - 190002.5) + 0.5;
    return(jd, jdfrac)



def hms2sec(hr, minute, sec):
    """converts hours, minutes, seconds into seconds from the beginning of the day
    
    inputs          description                    range / units
        hr          - hours                          0 .. 24
        min         - minutes                        0 .. 59
        sec         - seconds                        0.0 .. 59.99
        
    outputs      :
        utsec       - seconds                        0.0 .. 86400.0
    """
    utsec  = hr * 3600.0 + minute * 60.0 + sec

    return utsec




def sec2hms( utsec ):
    """converts seconds from the beginning of the day into hours, minutes and seconds.

    inputs          description                    range / units
        utsec       - seconds                        0.0 .. 86400.0
    
    outputs       :
        hr          - hours                          0 .. 24
        min         - minutes                        0 .. 59
        sec         - seconds                        0.0 .. 59.99

    """
    # np.fix() is equivalent to matlab's fix(),--> Rounds towards zero
    
    temp  = utsec / 3600.0
    hr    = np.fix( temp )
    minute   = np.fix( (temp - hr)* 60.0 )
    sec   = (temp - hr - minute/60.0 ) * 3600.0

    return(hr,minute,sec)







def convtime(year,mon,day,hr,minute,sec,
             timezone, dut1, dat): 
    """ finds the time parameters and julian century values for inputs
        of utc or ut1. numerous outputs are found as shown in the local variables.
        because calucations are in utc, you must include timezone if ( you enter a
        local time, otherwise it should be zero.
    
     
    
      author        : david vallado                  719-573-2600    4 jun 2002
    
      revisions
        vallado     - add tcg, tcb, etc                              6 oct 2005
        vallado     - fix documentation for dut1                     8 oct 2002
    
      inputs          description                    range / units
        year        - year                           1900 .. 2100
        mon         - month                          1 .. 12
        day         - day                            1 .. 28,29,30,31
        hr          - universal time hour            0 .. 23
        min         - universal time min             0 .. 59
        sec         - universal time sec (utc)            0.0  .. 59.999
        timezone    - offset to utc from local site  0 .. 23 hr
        dut1        - delta of ut1 - utc             sec
        dat         - delta of tai - utc             sec
    
      outputs       :
        ut1         - universal time                 sec
        tut1        - julian centuries of ut1
        jdut1       - julian date (days only)           days from 4713 bc
        jdut1Frac   - julian date (fraction of a day)   days from 0 hr of the day
        utc         - coordinated universal time     sec
        tai         - atomic time                    sec
        tdt         - terrestrial dynamical time     sec
        ttdt        - julian centuries of tdt
        jdtt        - julian date (days only)           days from 4713 bc
        jdttFrac    - julian date (fraction of a day)   days from 0 hr of the day
        tdb         - terrestrial barycentric time   sec
        ttdb        - julian centuries of tdb
        jdtdb       - julian date of tdb             days from 4713 bc
        tcb         - celestial barycentric time     sec
        tcg         - celestial geocentric time      sec
        jdtdb       - julian date (days only)           days from 4713 bc
        jdtdbFrac   - julian date (fraction of a day)   days from 0 hr of the day
    
      locals        :
        hrtemp      - temporary hours                hr
        mintemp     - temporary minutes              min
        sectemp     - temporary seconds              sec
        localhr     - difference to local time       hr
        jd          - julian date of request         days from 4713 bc
        me          - mean anomaly of the earth      rad
    
      coupling      :
        hms_2_sec   - conversion between hr-min-sec .and. seconds
        jday        - find the julian date
    
    """


    deg2rad = np.pi/180.0

    ### ------------------------  implementation   ------------------
    ### find the julian date given
    jd, jdfrac =  jday( year, mon, day, hr + timezone, minute, sec )
    ### Convert to modified julian date
    mjd  = jd+jdfrac - 2400000.5
    mfme = hr*60.0 + minute + sec/60.0


    ### ------------------ start if ( ut1 is known ------------------
    localhr = timezone + hr
    utc     = hms2sec( localhr, minute, sec )
#     print('CONVTIME--- UTC=',utc)

    ut1    = utc + dut1
    hrtemp,mintemp,sectemp = sec2hms(  ut1 )
    jdut1, jdut1frac       = jday( year,mon,day, hrtemp, mintemp, sectemp )
    tut1   = (jdut1+jdut1frac - 2451545.0  )/ 36525.0

    tai = utc + dat
    hrtemp, mintemp, sectemp = sec2hms(  tai )
    jdtai, jdtaifrac = jday( year,mon,day, hrtemp, mintemp, sectemp )

    tt  = tai + 32.184    # sec
    hrtemp,mintemp,sectemp = sec2hms( tt )
    jdtt, jdttfrac = jday( year,mon,day, hrtemp, mintemp, sectemp)
    ttt= (jdtt+jdttfrac - 2451545.0  )/ 36525.0

    ##### %%%%%%%%%%%%%%%%%%%%%% tdb
    # % vallado approach (extra digits)
    # %         me= 357.5277233  + 35999.05034 *ttt;
    # %         me= mod( me,360.0  );
    # %         me= me * deg2rad;
    # %         tdb= tt + 0.001658  * sin(me) + 0.00001385 *sin(2.0 *me);
    # %         [hrtemp,mintemp,sectemp] = sec2hms( tdb );
    # %         [jdtdb, jdtdbfrac] = jday( year,mon,day, hrtemp, mintemp, sectemp );
    # %         ttdb= (jdtdb + jdtdbfrac - 2451545.0  )/ 36525.0;
    # %         fprintf(1,'book tdb %8.6f ttdb  %16.12f jdtdb  %18.11f %18.11f \n',tdb,ttdb,jdtdb, jdtdbfrac );
    # % std approach (digits)
    # %         me= 357.53  + 0.9856003 * (jdtt - 2451545.0);   
    # %         me= mod( me,360.0  );
    # %         me= me * deg2rad;
    # %         tdb1= tt + 0.001658  * sin(me) + 0.000014 *sin(2.0 *me);
    # %         [hrtemp,mintemp,sectemp] = sec2hms( tdb1 );
    # %         [jdtdb1, jdtdb1frac] = jday( year,mon,day, hrtemp, mintemp, sectemp );
    # %         ttdb1= (jdtdb1 + jdtdb1frac - 2451545.0  )/ 36525.0;
    # %         fprintf(1,'std  tdb %8.6f ttdb  %16.12f jdtdb  %18.11f %18.11f \n',tdb1,ttdb1,jdtdb1, jdtdb1frac );
    # % ast alm approach (2012) bradley email 
    me = 357.53  + 0.98560028 * (jdtt - 2451545.0) 
    me = np.fmod( me, 360.0  )
    me = me * deg2rad
    dlje = 246.11 + 0.90251792*(jdtt - 2451545.0)
    tdb2= tt + 0.001657  * np.sin(me) + 0.000022 *np.sin(dlje)
    #
    hrtemp,mintemp,sectemp = sec2hms( tdb2 )
    jdtdb2, jdtdb2frac = jday( year,mon,day, hrtemp, mintemp, sectemp )
    ttdb2 = (jdtdb2 + jdtdb2frac - 2451545.0  )/ 36525.0
    # print(f'asta tdb {tdb2:8.6f} ttdb  {ttdb2:16.12f} jdtdb  {jdtdb2:18.11f} {jdtdb2frac:18.11f} ')
    #### usno circular approach 
    tdb = tt + 0.001657*np.sin(628.3076*ttt+6.2401)   \
             + 0.000022*np.sin(575.3385*ttt+4.2970)   \
             + 0.000014*np.sin(1256.6152*ttt+6.1969)  \
             + 0.000005*np.sin(606.9777*ttt+4.0212)   \
             + 0.000005*np.sin(52.9691*ttt+0.4444)    \
             + 0.000002*np.sin(21.3299*ttt+5.5431)    \
             + 0.000010*ttt*np.sin(628.3076*ttt+4.2490)  # USNO circ (14)
    hrtemp,mintemp,sectemp = sec2hms( tdb )
    jdtdb, jdtdbfrac = jday( year,mon,day, hrtemp, mintemp, sectemp )
    ttdb = (jdtdb + jdtdbfrac - 2451545.0  )/ 36525.0

    # fprintf(1,'usno tdb %8.6f ttdb  %16.12f jdtdb  %18.11f %18.11f \n',tdb,ttdb,jdtdb, jdtdbfrac );
    h,m,s = sec2hms( tdb )
    # %        fprintf(1,'hms %3i %3i %8.6f \n',h,m,s);

    #         % 
    # %%%%%%%%%%%%%%%%%%%%%% tcg
    # % approx with tai
    tcg = tt + 6.969290134e-10*(jdtai - 2443144.5003725)*86400.0  # AAS 05-352 (10) and IERS TN (104)
    hrtemp,mintemp,sectemp = sec2hms( tcg )
    jdtcg, jdtcgfrac = jday( year,mon,day, hrtemp, mintemp, sectemp )
    tt2 = tcg-6.969290134e-10*(jdtcg+jdtcgfrac-2443144.5003725)*86400.0
    #
    #         fprintf(1,'tcg %8.6f jdtcg  %18.11f ',tcg,jdtcg );
    h,m,s = sec2hms( tcg )
    # %        fprintf(1,'hms %3i %3i %8.6f \n',h,m,s);        

    # % binomial approach with days
    # %        lg=6.969290134e-10*86400.0;
    # %        tcg1 = tt + (jdtt - 2443144.5003725)*(lg + lg*lg + lg*lg*lg);
    # % days from 77
    # %        jdttx = jday( year,mon,day, 0, 0, 0.0); 
    # %        ttx = tt/86400.0 + jdttx-2443144.5003725  % days from the 1977 epoch
    # %        tcg2 = (jdttx - 6.969290134e-10*2443144.5003725) / (1.0 - 6.969290134e-10) % days
    # %        tcg2 = (tcg2 - jdttx)*86400*86400;
    # % sec from 77
    # %        ttx = tt + (jdttx-2443144.5003725)*86400.0;  % s from the 1977 epoch
    # %        tcg3 = ttx / (1.0 - 6.969290134e-10); % s
    # %        tcg3 = tcg3 -(jdttx-2443144.5003725)*86400.0;
    # % check with tcg
    # %        tcg4 = tt + 6.969290134e-10*(jdtcg - 2443144.5003725)*86400.0;  % AAS 05-352 (10) and IERS TN (104)
    # %        [hrtemp,mintemp,sectemp] = sec2hms( tcg4 );
    # %        jdtcg4 = jday( year,mon,day, hrtemp, mintemp, sectemp );
    # %        tt2 = tcg4-6.969290134e-10*(jdtcg4-2443144.5003725)*86400.0;
    # %        difchk = tt2-tt


    tcbmtdb = -1.55051976772e-8*(jdtai+jdtaifrac - 2443144.5003725)*86400.0 - 6.55e-5  # sec, value for de405 AAS 05-352 (10) and IERS TN (104)?
    tcb = tdb + tcbmtdb
    hrtemp,mintemp,sectemp = sec2hms( tcb )
    jdtcb, jdtcbfrac       = jday( year,mon,day, hrtemp, mintemp, sectemp )
    ttcb = (jdtcb + jdtcbfrac - 2451545.0  )/ 36525.0
    # fprintf(1,'     tcb %8.6f ttcb  %16.12f jdtcb  %18.11f %18.11f \n',tcb,ttcb,jdtcb, jdtcbfrac );

    return(ut1, tut1, jdut1, jdut1frac, utc, tai, tt, ttt, jdtt, jdttfrac, tdb, ttdb, jdtdb, jdtdbfrac)
        













