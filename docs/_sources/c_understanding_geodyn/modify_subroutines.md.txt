# Modifying Subroutines:

Modifying subroutines is necessary to output parameters that are not necessarily included in the normal output of GEODYN.  

MOD directory Location:  
 `/data/geodyn_code/IIE/MODS/`   
- Here, one will find a `xxx` script, `COMMON_DECL.INC`, an executable `giie1810_gfortran`, some (hopefully empty) error outputs, and any modified subroutines.
- To modify a subroutine, it must be placed in this directory, you then have to run the `xxx` script to repopulate the MODS directory with a new GEODYN executable. The new executable is `giie1810_gfortran`.  You then have to tell GEODYN to run from THIS executable when you run the `/RUNS/zzz` script: 

Change zzz script from:
```
# RUN IIE program
../IIE/ORIG/giie1810_gfortran>iieout 2>iieerr
```

To:
```
# RUN IIE program
#../IIE/ORIG/giie1810_gfortran>iieout 2>iieerr
../IIE/MODS/giie1810_gfortran>iieout 2>iieerr
```


## Including Density Output:
To output density along the orbit of the spacecraft, we will modify the `DRAG.f90` subroutine as follows:
```
(line 430)
!     ....JACCHIA 71 MODEL
      RHO=D71(RASAT)
      DRHODZ=0.D0
  150 CONTINUE
      IF(LSTINR.AND.FSSTRT.GT.200.D0) THEN
        WRITE(99,7000) FSSTRT,IYMD,IHMS,XLATD,XLOND,ALTI,RHO,DRHODZ,    &
     &          X,Y,Z,XDOT,YDOT,ZDOT
 7000   FORMAT(F12.1,2I8,2F12.4,F12.3,2D20.11,6(3X,F12.4))
      ENDIF
!
```
**Description:** LSTINR is last inner iteration, FSSTRT is seconds since initial epoch.
If on the last iteration, it prints the parameters to a file `fort.99` using the format stored at 7000 (arbitrary value).  The format is such that there are 14 parameters being stored and 14 data-types being identified for each parameter.  

**Format description:**  
  `F12.1` -- reserves 12 spaces (including '.' and '-') for a real floating-point number with 1 decimal place.  
  `2I8` -- reserves 8 spaces for an integer.  The 2 at the front indicates to do this twice.  
 `2F12.4` -- reserves 12 spaces (including '.' and '-') for a real floating-point number with 4 decimal place.  The 2 indicates to do this twice.  
  `2D20.11` -- the D specifier is for the exponential form of decimal double-precision items.  
`6(3X,F12.4)` -- `3x` places 3 spaces before the `F12.4`.  The 6 and parenthesis indicates to repeat the formation 6 times.

**Parameter Descriptions:**  
   FSSTRT -- Elapsed seconds since initial epoch  
   IYMD -- YYMMDD of current epoch  
   IHMS -- HHMMSS of current epoch  
   XLATD -- Lat of Space station (degrees)  
   XLOND -- Lon of Space station (degrees)  
   ALTI -- Hieght of Space station above elipsoid (M)  
   RHO -- Density of atmosphere (KG/M^3)  
   DRHODZ -- Change in density of atmosphere with height (KG/M^3/M)  
   X -- X component of position vector  
   Y -- Y component of position vector  
   Z -- Z component of position vector  
   XDOT -- X component of velocity vector  
   YDOT -- Y component of velocity vector  
   ZDOT -- Z component of velocity vector  


## Other Modifications:
 - There was an error where the IIEOUT printout showed the that atmospheric model being used was "JAACHIA9999" when MSIS86 was selected.  The `SUMMRY.f90` subroutine was edited to fix this.  
```
(Line 365)
!!!!  900 WRITE(IOUT6,10500) IYATDN(JATDEN)
!!!!      WRITE(IUNT88,10500) IYATDN(JATDEN)
      IF(IATDN.EQ.2) WRITE(6,10551)
      IF(IATDN.EQ.4) WRITE(6,10552)
      IF(IATDN.EQ.5) WRITE(6,10553)
  950 CONTINUE

...

(Line 880)
10551 FORMAT(' ATMOSPHERIC DENSITY MODEL  USED: JAACHIA 71')
10552 FORMAT(' ATMOSPHERIC DENSITY MODEL  USED: FRENCH DTM')
10553 FORMAT(' ATMOSPHERIC DENSITY MODEL  USED: MSIS')
```

