library(usmap)
library(dplyr)


states     <- as.data.frame(subset(statepop, select = c(abbr, full, fips))) 
states     <- states %>% 
              rename (
                state_name     = full,
                state_fips     = fips 
              )

counties   <- as.data.frame(subset(countypop, select = c(abbr, county, fips)))
counties   <- counties %>% 
              rename (
                county_name     = county,
                county_fips     = fips 
              )

comb <- merge(counties, states, by= 'abbr', all.x = TRUE)




write.csv(comb, paste0(getwd(), '//gen_dev//state_county_fips.csv'))

counties <- read.csv('gen_dev//state_county_fips.csv')

get_county_fip <- function(state, county){
  county_fip   <- subset(counties, counties$county_name == county & counties$state_name == state, select = c(counties$county_fips))
  return(county_fip)
}

get_state_fip <- function(state){
  county_fip   <- toString(subset(counties, counties$state_name == state, select = c(counties$state_fips)))
  return(county_fip)
}
get_county_fip('New York', 'Albany')
