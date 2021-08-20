

### Alpha version .29 updates
1. Scratch_modeling_table and scratch_modeling_agg_table now have source geometries 
2. User data can now have capital letters in road name and crash year attributes

### Beta version .1 updates 

#### Application 
1. Maps default to US boundary
2. Model start alert notifies user once 
3. Move_windows_short_comp attribute on accounts table is not longer used or maintained
4. Bug fixed when the user uploads new data when opting to delete previous results
5. Source id added to model output roads (user selects during upload)
6. Updated language on popup of model output segments. Removed term 'Predicted,' replaced with 'Estimated'
7. Adjusted colors of pedestrian crash scores on sliding windows visualization from orange to green for better visualization
8. Added additional information to popups of sliding windows segment and adjusted language
9. Disaggregated crash density analysis from modeling component 
   1.  Crash density analysis now creates a new table: automated.sw_sliding_windows_{user_run_id}.
   2.  The downloader only searches for the new table mentioned in 9.1, and does not search for the same table created during the modeling component. 
   3.  The new crash density function does not save scratch tables
   4.  Functions exist in /R/fct_sliding_windows_tool.R
   5.  If a user tries to upload new source data and crash density results exist, the tool will ask the user if they'd like to delete the results (the same applies to model results)
   6.  safer_streets_priority_finder/R/fct_hin_tool.R is depreciated  
11. User data can now have capital letters in road name and crash year attributes
    1.  updates 10 and 11 occurred on the alpha .29 version update, but because these steps overlapped, the code was updated in the alpha and beta branches separately 
12. Tool throws the correct error if the user tries to load the study area and no study area has been uploaded 
13. Map no longer zooms to the outer edge when the user uploads local crash data

### Beta version .2 updates 
1. password no longer required for login or account/scenario setup. We did this mainly to remove the veneer of security. Although data are stored in a secure PostgreSQL database with limited access (only the EC2 instance hosting the app and developers have inbound access), we don't any sort of authentication or security on the log in credetials. In the future we may want to incorporate Auth0 authentication.
   - [Auth0](https://auth0.com/security)
   - [Adding Authentication to Shiny Server in 4 Simple Steps](https://auth0.com/blog/adding-authentication-to-shiny-server/)
2. Local user input maps now fly to bounding box of user's data. 
3. New RDMS has been built and partially incorporated. 
   1. Major improvements include additional GiB of memory and better schema structure for the Tool. 
   2. [Refer to this document for more information](https://github.com/tooledesign/a0137_vulnerable_user_risk_network_tool/blob/beta_phase_updates/99x_main_tool_dev/safer_streets_priority_finder/notes/database_configuration.md)
4. App now tracks the number of times a user logs in, as well as the uesr's last log in time. Will be useful if we ever need to track bad users. 
5. App automatically jumps to next step in the Load Data navigation when a user submits study area, crash, and roads data. 

### Beta version .21
1. added quotes to delimited column names where no columns are specified in generalized downloader. This enables the tool to retrieve tables that container attributes that start with capital letters or numbers etc.
2. Two pie charts added to dashboard to summarize crashes by mode and severity  

### Beta version .22
1. Fixed issue associated with 'Map Data' button if user has no data. Now flags the problem rather than crashing. 
2. Switched out TIGER data for OSM data 
   - Users can now select nationally available OSM data using their local study area or county data. 
   - Generally includes roads data. Pedestrian and bicycle facilities are excluded. 
3. Users can select FARS data with their local roads data. 
4. Added boolean flag to crash data that indicates if a crash is within the study area or not. This flag is used to filter for desc stats on data dashboard. 
5. Model fit statistics are available on data dashboard, observed vs estimated by severity and functional classification 
6. Crashes are described by severity and functional classification 
7. Presents of summary statistics are dynamic on the available data. For example, if no model results are present, fit statistics won't be either. 
8. Sliding windows analysis now calculates Other Crashes sliding windows.
   - All three results are mapped. 
9. Omit from Analysis is not an option for mapping crash mode. 
10. Crash Density Analysis and High Risk Streets Model tools are on panels. Each tool has a button to jump to the proceeding screen. 
11. Map Visualization tab created. 
   - User can select to load crash, study area, and one roads dataset. Roads dataset options are data dependent. User can also upload a local dataset of any datatype (point, line, polygon).
12. General GIS disclaimer added to Welcome Page. 
13. Reversed order of crash and roads data upload. Crash data now requires user to upload a study area and roads dataset. Study area is required for the SRID and the roads data is required to join functional classification to crashes (used in summary statistics).
14. Crash data now requires roads to be loaded first to assign a functional class attribute to each crash. 
16. Data dashboard includes lots of new metrics to review. 
17. New login page - user now logs in with UN and password alone. There's a second screen for selecting login scenarios. 
18. Color symbolization is no longer randomized. 
19. Reorganized the analysis production tools, added several control buttons to manage the two
20. Fixed bug in attaching crash costs when defaults are not adjusted
21. Creation of OSM and FARS data no longer requires user confirmation. 

### Model Processor Beta version .1
1. Comprehensive costs are now taken from user's crash data rather than a default table. This means that any adjusted values the user applies will be accounted for in the final comprehensive costs. Default crash costs are offered to the user with the option to update them with local values. If crash costs are updated, a five-year discount rate is applied. 
2. The model processor will not flag model estimation complete if the processor fails 
3. ~~excess ddl files are deleted after each model run~~
   - stress testing confirmed this caused other downstream issues that we don't want.
4. Improved DB connection validity check every minute
5. hin_run_sliding_windows in model_processor/R/fct_hin_tool.R has been removed
6. scratch_modeling_table and scratch_modeling_agg_table now have source geometries

### Beta version .23
1. Added visualization for ped/bike risk estimates in map visualization
2. Added legend for schools 
3. Other crashes are now summarized in the dashboard
4. Tool not checks for attributes created by the tool. If exists, the user is notified and asked to remove/change names before proceeding. 
5. Moved extraneous steps from crash and roads data upload 
6. Applied formatting to welcome page 
7. Incorporated requested text edits 
8. Applied a new function to create a consistent color palette for maps
9. Added legend to model results 
10. Updated buttons on crash density analysis results map 
11. Fixed annoying click through bug on study selection during log in 
12. Cleaned up the processing of schools on map visualization (user can now select schools alone, also reduced number of renderings by unioning data where exists)
13. Removed crash Property Damage Only O's from maps to reduce browser load 
14. Added legends to model estimate maps 
15. Added catch for new value for model_status while processing

### Model Processor Beta version .2
1. Added estimated crashes, (rate * length / 1609) * 5
2. Calculated final cost off new estimated crash counts
3. Updated model_status while running to allow model processor to fail better and continue where it left off 
4. Added 9th function to model processor that creates final output roads. This allows the processor to fail without breaking user's UI. 

### Beta version .24
1. New language 
2. Bug fix on attribute check during local data load   
3. Updated model estimate visualization

### Beta version .25
1. Updated language 
2. New model processing icon on login
3. Additional "Loading..." spinners 

### Beta version .26
1. Added css loading ideas to plots and maps 
2. Added instructions modals 
3. Bike and ped crashes now pulls from user's mapped data  
4. Schools can now be toggled in the Map Visualization page

### Beta version .27
1. Extended time for many alerts 
2. Added check for data on crash density visualization button 
3. Removed .csv from downloader function 
4. Converted downloaded data format from shapefile to geopackage to better handle column name length
5. Added new start page 
6. Cleaned up the formatting on the login module 
7. Renamed most dangerous corridors to highest crash corridors
8. User data can now be toggled on map visualization 
9. Added descriptive text to download card 
10. Customizable themes!! 
11. Text updates
12. Additional summary tables and charts  
13. New options to filter data on Dashboard 
14. Tables on summary stats are downloadable 
15. Confirm data and map visualization pages no longer map crashes outside the study area 
16. Renamed Glossary tab
17. Updated Model Status notifications on landing page 
18. New favicon.ico from https://gauger.io/fonticon/ (thanks!)
19. Updated title of application in browser tab
20. Dashboard summarizes omitted crashes
21. Decoupled some processing in the dashboard for better performance 
22. Model estimates are visualized on estimated crash costs averages per mile (map visualization)
23. Added social media email buttons

### Beta version .28
1. On initial upload, the user no longer needs to refresh the dashboard twice to load all charts and tables
2. Multiply percents that feed stacked bar chart of crashes by severity to avoid rounding issue 
3. Removed text pointing to an unavailable switch

### Beta version .29 
1. Added hidden button that silently clicks when the user clicked the Dashboard tab item 
2. Tool checks that tables exist in dashboard before download

### Beta version .30
1. Model estimates are visualized on estimated crash costs averages per mile (analysis page)
2. Updated text

### Beta version .31
1. Addressed hyperlink color
2. Removed a column from stacked bar chart for needs by mode to avoid confusion 
3. Dashboard updates 
4. Fixed bug on model visualization button on the analysis page 
5. Updated name of model estimation selectors on map visualization 
6. Updates text 
7. App no longer hides card after user uploads load roads 
8. Updated tooltip on stacked bar charts 
9. Expressways omitted from dashboard 
10. Added instructions 

### Beta version .32 
1. Added option to download report of crashes 
2. Filtered out Omit From Analysis and Expressways from the process that binds functional classification to crashes. 

### Beta version .33 
1. Updated text 

### Beta version .34 
1. Added data dictionary to each downloadable dataset  
2. Updated column header names 
3. Updated text
4. Enabled various basemap tile options 

### Beta version .35
1. Fixed popups 
2. Added study area boundaries to sliding windows and model results 
3. Fixed issue with reading server logic on new account creation 
### Beta version .36 
1. Allowed study name to have underscores 
2. Updated default background color 
3. Added default messages to indicate where no information is available of mapped variables in confirm data inputs tab 
4. Improved handling of missing data handling from mapped variables in confirm data inputs tab 
5. Fixed bug on server logic associated with the creation of new account
6. Updated 'Bike' to 'Bicycle' on maps 
7. Added mode to crash legend and sliding window legend as part of the map visualization tab 
8. Changed dropdown text to be 'Top Ten by [Pedestrian/Bicycle] Sliding Windows Score
9. Updated text/popups in sliding windows analysis tab 
10. Updated messages displayed while uploading data
11. Removed confirmation modal from crash upload because the confirmation is auto navigation to the next tab. This confirmation was interfering with the loading screen when navigating to the confirm inputs tab
12. Enabled custom discount rate 
13. Updated report visualizations and save name

### Beta version .37 
1. Updated the backend to store pooled connection as a local instead of global object. 
2. Implemented logic that will disconnect the shiny session if the psql connection is not valid (checks every 5 seconds). 
3. Confirm inputs tab on Load Data page not pulls spatial data to map on first navigation 
4. Fixed bug in mapped tables on Cinform Inputs page 
5. Total allowed number of crashes is 150,000
6. Total length of allowed roadways is 26708.43 or ~ miles of roadways in LA county by OSM roads on file 
7. Cleaned up checks as a result of clicking, 'Perform Checks' button 
8. Added logic to confirm/deleted downstream local user data when user wants to upload new study area or roads data. In other words, if the user wants to upload a new study area, they need to delete their crash and roads data to continue. The user is prompted with a confirmation button before deleting the data. This catch is required to prevent downstream errors, particularly on the Dashboard. The reason is that several post processes occur on roads data and crash data that involve upstream data. 
9. Fixed connection issues resulting from updates from v.36 by replacing instances of dbSendQuery with dbGetQuery 
10. improved logic to warn users of bad variable selecting during mapping
11. Added correct method to clear leafgl features in 'Confirm Inputs' tab
12. Added trigger to keep maps centered when updating other maps 
13. Updated text from 20210729-img-and-text-edits branch


### Beta version .38
1. Better handling of model results visualization where no segments meet the identified threshold 
2. Fixed broken links to table of crash data 
3. Solved issue with model estimate charts where no historical ped or bike crashes exists
4. Updated psql schema name

### Beta version .39
1. Dashboard now filters out segments with no score from maps
2. Improved top ten map
3. Adjusted all alerts to size small 
4. Add catch when no FARS data exist in study area

### Beta version .40
1. Updated text

### Beta version .41
1. Updated text
2. Sliding windows, upload crashes, upload roads are now executed as background process 
3. Updated modal button when deleting model/sliding window results 
4. Force close any modals when disconnecting
5. Send notification email to user when model is finished 
