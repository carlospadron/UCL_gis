Tool for the Definition and Analysis of Functional Economic Market Areas (DAFEMA).
written by Carlos Padron carlos.florez.16@ucl.ac.uk;padron.ca@gmail.com.

INSTRUCTIONS

Please note that the script expects the data to be stored in the same folder as
the script. 
After sourcing the script, the user only needs to run the following command to 
start:

dafemaStart(region, readData) 

example 

dafemaStart("north east", readData = TRUE)
dafemaStart("north east")

Please take in consideration the amount of msoa per region as it affects the
computing time

340, "North East", dafemaStart("north east")
573, "East Midlands", dafemaStart("East Midlands")
692, "Yorkshire and The Humber", dafemaStart("Yorkshire and The Humber")
700, "South West", dafemaStart("South West")
735, "West Midlands", dafemaStart("West Midlands")
736, "East of England", dafemaStart("East of England")
924, "North West", dafemaStart("North West")
983, "London", dafemaStart("London")
1108, "South East", dafemaStart("South East")

The region value can be one or many regions using a string, a string vector or 
a defined variable i.e. 'london', c('london', 'south east'), definedVariable etc.

This tool was tested with all the regions in england. They should work except
South West which doesn't to work for the moment.

The tool works for the region of London but this region is so intrinsically
connected that shows only one big fema even for small values.

The read data argument is optional and by default FALSE. In order to save time,
dafema reads the data only once so if it is used again it will skip this step. 
Anyhow, the user might modify or replace the data. In this case the user can 
force dafema to read the data with the argument readData = TRUE.

Once it is finished, the following commands can also be used:

dafemaShowMap(closureTarget)
View(dafemaFemaResults)

dafemaShowMap shows a map where the user can visualise the msoa forming
the fema per closure target.
example:

dafemaShowMap(.40)

View(dafemaFemaResults) shows a table with all closure values for every FEMA.
