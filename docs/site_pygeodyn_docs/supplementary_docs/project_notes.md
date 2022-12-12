# Project Notes

[comment]: <> (## Atmospheric Density and Drag:)

[comment]: <> (DRAG EQUATION)

[comment]: <> (Where,  )

[comment]: <> (AD  acceleration due to atmospheric drag force  )

[comment]: <> (CD is the satellite drag coefficient  )

[comment]: <> (As is the cross-sectional area of the satellite  )

[comment]: <> (ms is the mass of the satellite  )

[comment]: <> (D is the-density of the atmosphere at the satellite position  )

[comment]: <> (vr  is the velocity vector of the satellite relative to the atmosphere, and  )

[comment]: <> (vr  is the magnitude of the velocity vector, vr .  )

[comment]: <> (CD is treated as a constant in GEODYN, unless the parameter is being estimated in a data reduction run. The factor CD varies slightly with satellite shape and atmospheric composition. However, for any geodetically useful satellite, it may be treated as a satellite dependent constant.)

[comment]: <> (The DRAG.f90 subroutine modifies drag application and /or requests estimation of drag coefficients.)


## Density Models in GEODYN

As of December 2020, the current default thermospheric implementation in GEODYN is the Jacchia 1971 model, with options existing to use the Thermosphere Drag Model (DTM87), or the MSIS-86 Empirical Drag Model (MSIS86). These outdated models represent a source of error in the calculation of satellite drag during an orbit determination run.  The deficiency of the density model will be compensated by overfitting to other estimated parameters, such as drag coefficient, estimated accelerations, state vector, etc. This overfitting can lead to problems if one needs to maintain realistic values for the estimated parameters in order to predict a future orbit.


**On Overfitting: [Sutton 2020, Private Communication]**  
When using orbital determination (OD) and estimating lots of parameters, residuals may not always show an obvious improvement even when a much better density model is used. In such a case, the deficiency of the original model is compensated by other estimated parameters, maybe CD, maybe other terms. This overfitting can lead to problems if you care about more than just the residuals, i.e., if you need to maintain a realistic CD in order to predict a future orbit. For instance, if you estimate CD = 100 with a horrible thermosphere model, that value may not persist so well into the future.

**Items to track when changing density models:**
 - Density at the satellite orbit
 - Residuals 
 - Trajectory (ephemeris) of the satellite
 - Cd
 - Other parameters that might are being estimated. 
    - What are the parameters that could be “soaking up” the thermosphere errors. To assess the thermosphere model, you’d want to keep these as few as possible to avoid overfitting. 
    - i.e. if you’re also estimating a solar radiation pressure coefficient, you’d want to know how it varies over time. 
   



