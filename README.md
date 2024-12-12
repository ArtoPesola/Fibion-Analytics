# FibionFlow
Fibion Flow is an R markdown code to process data collected from the Fibion Research device. It includes a validated algorithm to detect sleep and non-wear periods and to analyse sitting and physical activity durations, accumulation patterns and energy expenditures from the valid waking hours. Additional properties include visual representation of the algorithm performance, and data segmentation based on custom timestamps, e.g. for analysing sitting and physical activity metrics for working and non-working hours. 

## Features of FibionFlow
- Automated valid waking wear isolation from time in bed and prolonged non-wear
- Adaptable parameters to customize algorithm thresholds
- Adaptable wear time criteria
- Visualisation of valid waking wear vs. sleep and non-wear with activiyt intensity and off periods (can be exported as pdf files)
- Data segmentation by customized timestamps (can be imported as Microsoft Excel files)

## Outputs of FibionFlow

### Sitting and activity duration and energy expenditure:
 - Waking wear time
 - Sitting time
 - Standing time
 - Walking time
 - Cycling time
 - High-intensity/running time
 - Sitting MET-minutes
 - Standing MET-minutes
 - Walking MET-minutes
 - Cycling MET-minutes
 - High-intensity/running MET-minutes
 - Sitting mean METs
 - Standing mean METs
 - Walking mean METs
 - Cycling mean METs
 - High-intensity/running mean METs
 - Light-intensity activity duration
 - Moderate-intensity activity duration
 - Vigorous-intensity activity duration
 - Moderate-to-vigorous-intensity activity duration
 - Below-walking-intensity activity duration (relative-to-walking METs intensity)
 - Above-walking-intensity activity duration (relative-to-walking METs intensity)

### Sitting and activity accumulation patterns:
 - Number of sitting bouts
 - Number of activity bouts
 - Number of <10 min, 10-30 min and >30 min sitting bouts
 - Number of <10 min, 10-30 min and >30 min activity bouts
 - Sitting bout mean duration
 - Activity bout mean duration
 - Duration at <10 min, 10-30 min and >30 min sitting bouts
 - Duration at <10 min, 10-30 min and >30 min activity bouts
 - Usual sitting bout duration
 - Usual activity bout duration
 - Active sitting (>1.5 METs) duration
 - Active sitting (>1.5 METs) proportion of total sitting
 - Usual sitting bout duration (weighted median of sitting bout lengths)

The variables are calculated as daily means, as well as non-weighted and weighted (weekdays 5/7, weekend days 2/7) overall means. 

### Example of automated waking wear detection visualisation
![FibionFlow algorithm output figure](https://github.com/ArtoPesola/FibionFlow/assets/51989005/b624839e-5b2b-4596-a51a-c59974b70b90)

### Example of custom events visualisation
![FibionFlow custom events output figure](https://github.com/ArtoPesola/FibionFlow/assets/51989005/df851d9f-4f85-49f9-af0f-1ed34a081720)

FibionFlow has been developed by Arto Pesola (director of Active Life Lab, South-Eastern Finland University of Applied Sciences and co-founder of Fibion Inc). More information and resources can be found at www.fibion.com.
