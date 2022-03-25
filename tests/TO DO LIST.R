
# TO DO -------------------------------------------------------------------

## Add custom input for size of shelter site (ie distance where rest behaviour
# becomes near stationary)

## Expand range of rasters

## Ensure that each raster is connected to correct behaviour. Movement vs
# decisions

## FIX angle, as it is currently aiming always for North.
# Need double to store last chosen angle
# Need a vector length nopt to store angle options movement
# double of last angle will be used in calculating next destination and next movement:
# line 283, 342
# think we just need to add the new drawn angles to a previous angle
# make sure to update currAngle each time


# NOTES -------------------------------------------------------------------

## AVOIDANCE notes
# mO = moveoptions interator
#
# x_avoidPoints = vec of points
# y_avoidPoints = vec of points
# navp = number of points to avoid, comes from x_avoidPoints
#
# cumulative_dist = sum of distances for each moveOption, used once per loop
#
# distance_toAvoid = vec of the cum. distances for each moveOption
#
# haven't added anything about the avoidance only being in certain behaviours
