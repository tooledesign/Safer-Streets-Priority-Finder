# module for confirming inputs 
source(file.path(getwd(), 'R', 'mod_confirm_inputs.R'), local = TRUE)

# load data page
source(file.path(getwd(), 'R', 'mod_load_data.R'), local = TRUE)

# manage study area selction
source(file.path(getwd(), 'R', 'mod_manage_study_area_upload.R'), local = TRUE)

# manage crash data upload
source(file.path(getwd(), 'R', 'mod_manage_crash_upload.R'), local = TRUE)

# manage roads data upload
source(file.path(getwd(), 'R', 'mod_manage_roads_upload.R'), local = TRUE)

# build model modules
source(file.path(getwd(), 'R', 'mod_build_model.R'), local = TRUE)

# exporter/reporter page
source(file.path(getwd(), 'R', 'mod_reporter.R'), local = TRUE)

# build sliding windows 
source(file.path(getwd(), 'R', 'mod_build_sliding_windows.R'), local = TRUE)

# visualize model results
source(file.path(getwd(), 'R', 'mod_visualize_model_results.R'), local = TRUE)

# login configuration 
source(file.path(getwd(), 'R', 'mod_login_config.R'), local = TRUE)

# global variables
source(file.path(getwd(), 'R', 'global.R'), local = TRUE)

# get visualization sliding windows 
source(file.path(getwd(), 'R', 'mod_visualize_sliding_windows.R'), local = TRUE)

# get helper functions 
source(file.path(getwd(), 'R', 'fct_helpr_funs.R'), local = TRUE)

# get map viz
source(file.path(getwd(), 'R', 'mod_visualize_data_reporter.R'), local = TRUE)

# get use cases
source(file.path(getwd(), 'R', 'mod_use_case.R'), local = TRUE)