
# global variables 

# create local list of states and counties with fips
counties <- read.csv(file.path(getwd(), 'county_state_fips.csv'))
counties$statefp <- sprintf("%02d", counties$statefp)
counties$countyfp <- sprintf("%03d", counties$countyfp)

# default kabco costs 
default_kabco <- read.csv(file.path(getwd(), 'kabco_default_costs.csv'))
names(default_kabco)[1] <-  "KABCO Value"