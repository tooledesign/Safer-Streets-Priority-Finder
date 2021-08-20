
#############################3
### This script holds severeal general functions used throughout the applications.

list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(lapply(list, tags$li))
  } else {
    res <- lapply(list, tags$li)
    res <- lapply(res, function(x) tagAppendAttributes(x, class = class))
    tagList(res)
  }
}

col_12 <- function(...){
  shiny::column(12, ...)
}

col_6 <- function(...){
  shiny::column(6, ...)
}

col_9 <- function(...){
  shiny::column(9, ...)
}

col_8 <- function(...){
  shiny::column(8, ...)
}

col_4 <- function(...){
  shiny::column(4, ...)
}

col_3 <- function(...){
  shiny::column(3, ...)
}

col_2 <- function(...){
  shiny::column(2, ...)
}

# Creates simple modal
Add_Modal <- function(title='None', body='None'){
  showModal(modalDialog(
    title = title,
    paste0(body),
    easyClose = TRUE,
    footer = NULL
  ))
}

# gets county fips code from counties table
get_county_fip <- function(state, county){
  county_fip   <- as.character(subset(counties, county_namelsad == county & state_name == state, select = c(countyfp)))
  return(county_fip)
}

# gets state fips code from counties table
get_state_fip <- function(state){
  state_fip   <- as.character(unique(subset(counties, state_name == state, select = c(statefp))))
  return(state_fip)
}

# unzips data 
process_zipped_shapefile <- function(file, type=NULL, promote_to_multi=TRUE, group_or_split=FALSE, do_split=FALSE){

    tryCatch(
      {
        # get directory name
        tempdirname <- dirname(file$datapath[1])

        for (i in 1:nrow(file)) {
          file.rename( file$datapath[i],
                       paste0(tempdirname, "/",
                              file$name[i]
                       )
          )}
        temp1 <- unzip(paste0(tempdirname, "/",
                              file$name[1]
        ))
        geodata <- read_sf(temp1[grep(pattern = "*.shp$", temp1)][1], promote_to_multi=promote_to_multi)

        if (!is.null(type)) {
          geodata <- sf::st_cast(geodata, type, group_or_split=group_or_split, do_split=do_split)
        }

        unlink(temp1, recursive = TRUE)

        #return spatial data frame
        return(geodata)

        }, error = function(cond){
        c <- toString(cond)
        Add_Modal(title = 'Something Went Wrong.', body=c)
        }
    )
  }


# csv to dataframe 
csv_to_dataframe <- function(data=NULL){
    tryCatch(
      {
        csv_df <- read.csv(
          file = data$datapath,
          header = T,
          sep = ','
        )
        return(csv_df)
      }, error = function(cond){
        c <- toString(cond)
        Add_Modal(title = 'Something Went Wrong.', body=c)
      }
    )
  }


# create spatial table from dataframe (NOT TESTED)
# process_csv_spatial_data <- function(data=NULL, lat=NULL, long=NULL){
#   # process sptial data
#   # file = R SpatialDataFrame
#   # lat = latitude column
#   # long - longitude column
#  tryCatch(
#    {
#      geodata <- st_as_sf(data, coords = c(lat, lat), crs = utm18nCRS)
#      return(geodata)
#    }, error = function(cond){
#      c <- toString(cond)
#      Add_Modal(title = 'Something Went Wrong.', body=c)
#    }
#  )
# }

# base function that renders 
render_map <- function(output, map_name, bounds=c(-125.0, 25.0, -66.96, 49.5), position="topright"){
  tryCatch({
  output[[map_name]] <- leaflet::renderLeaflet({
    leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
      leaflet::addProviderTiles(providers$CartoDB.DarkMatter, group = 'Negative') %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addProviderTiles(providers$CartoDB.Positron, group = 'Grey', options = providerTileOptions(noWrap = TRUE)) %>%
      leaflet::addLayersControl(baseGroups = c("Grey", "Negative",  "OpenStreetMap"), position = position) %>%
      leaflet::fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      htmlwidgets::onRender("function(el, x) {``
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }")
  })
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

 
# check for integers 
check_for_integer <- function(N) {
  N <- as.character(N)
  sapply(N, function(x) all(unlist(strsplit(x, ""))%in% 0:9))
}

# confirm if all are integers 
are_all_integers <- function(N){
  test <- check_for_integer(N)
  if (all(test)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# are years formatted correctly
are_year_formatted_sortof <- function(N){
  tryCatch({
      if (any((floor (log10 (abs (N))) + 1) != 4)){
        return(FALSE)
      } else {
        return(TRUE)
      }
    }, error = function(cond){
        print(cond)
        return(FALSE)
      }
    )
  }

# update user account info 
update_account_info <- function (
  connection,
  user_id=NULL,
  run_id=NULL,
  column=NULL,
  new_value=NULL
){
  tryCatch({
  q <- glue::glue("
                    UPDATE gen_management.accounts
                    SET {column} = {new_value}
                    WHERE user_id = {user_id}
                    AND run_id = {run_id};
                    "
  )

  DBI::dbGetQuery(connection, q)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# check for five years of crash data 
five_year_crash_data_test <- function (
  connection,
  user_id,
  run_id
){
tryCatch({
  crash_data_max <- get_max_year(connection=connection, user_id=user_id, run_id=run_id)
  crash_data_min <- get_max_year(connection=connection, user_id=user_id, run_id=run_id, method = 'MIN')

  if ( crash_data_max - crash_data_min >= 4 ) {
    return (TRUE)
  } else {
    return (FALSE)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns vector of crash year on crash table 
get_unique_crash_years <- function(
  connection,
  user_id,
  run_id,
  crash_o_year_col
){
  tryCatch({
  crash_o_year_col <- DBI::dbQuoteIdentifier(connection, crash_o_year_col)
  query <- glue::glue(" SELECT DISTINCT {crash_o_year_col}
                                FROM local_user_data.crashes_{user_id}_{run_id}
                               ;")

  crash_year <- DBI::dbGetQuery(connection, query)
  if ( !is.null(crash_year) ) {
    return(as.vector(crash_year))
  } else {
    return(FALSE)
  }
}, 
error = function(cond){
  c <- toString(cond)
  Add_Modal(title = 'Something Went Wrong.', body=c)
})
}

# returns the name of crash year column on crash table 
get_crash_year <- function(
  connection,
  user_id,
  run_id
) {
tryCatch({
    crash_year <- NULL
    run_id <- DBI::dbQuoteLiteral(connection, run_id)
    query <- glue::glue(" SELECT crash_o_year_col
                                FROM gen_management.accounts
                               WHERE user_id = {user_id}
                                 AND run_id = {run_id}
                               ;")

    crash_year <- DBI::dbGetQuery(connection, query)

    if ( !is.null(crash_year) ) {
      return(crash_year)
    } else {
      return(FALSE)
    }
  }, error = function(cond){
    return(FALSE)
  })
}

# returns latest year of crash data 
get_max_year <- function(
  connection,
  user_id,
  run_id,
  method='MAX' # use 'MAX' or 'MIN'
) {
  tryCatch({
  crash_year_col <- DBI::dbQuoteIdentifier(connection, as.character(get_crash_year(connection, user_id, run_id)))
  if (crash_year_col != FALSE  ){
    crash_table <- DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id))
    run_id <- DBI::dbQuoteLiteral(connection, run_id)
    q <- glue::glue("
                    SELECT {method}({crash_year_col})
                    FROM local_user_data.{crash_table}
                    WHERE {crash_year_col} IS NOT NULL;
                    "
    )

    max_year <- DBI::dbGetQuery(connection, q)
    valid <- are_year_formatted_sortof(as.integer(max_year))
    if ( valid ) {
      return(as.integer(max_year))
    } else {
      return(FALSE)
    }
  } else {
      return (FALSE)
  } 
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns table of user data found for confirm crash table 
get_user_data <- function(
  connection,
  user_id,
  run_id
) {
  tryCatch({
  q <- glue::glue('
                    SELECT
                      o_username as "Username",
                      o_run_id as "Scenario ID",
                      sa_o_source as "Study Area Source",
                      crash_o_source as "Crash Data Source",
                      road_o_source as "Roads Data Source",
                      crs as "Study Area UTM SRID",
                      crash_o_year_col as "Selected Crash Year Column",
                      crash_o_serv_col as "Selected Crash Severity Column",
                      crash_o_rep_id_col as "Selected Crash Report ID Column",
                      crash_o_mode_col as "Selected Crash Mode Column",
                      roads_fun_c_col as "Selected Road Functional Classification",
                      road_o_name as "Selected Road Name Attribute",
                      sa_storage_opt_out    as "Save Study Area For Research Opt Out",
                      crash_storage_opt_out as "Save Crashes For Research Opt Out",
                      roads_storage_opt_out as "Save Roads For Research Opt Out"
                      FROM gen_management.accounts
                      WHERE user_id = {user_id}
                        AND run_id  = {run_id}
                      ;
                    ')
  table <- DBI::dbGetQuery(connection, q)
  return (as.data.frame(table))
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# records bounding box on user account table  
account_bbox_udpates <- function (
  connection,
  user_id,
  run_id,
  shape
) {
  tryCatch({
  shape_bbox <- sf::st_bbox(shape)
  query <- glue::glue("UPDATE gen_management.accounts
                       SET    sa_bbox_south_4326 = {shape_bbox[2]},
                              sa_bbox_north_4326 = {shape_bbox[4]},
                              sa_bbox_east_4326  = {shape_bbox[3]},
                              sa_bbox_west_4326  = {shape_bbox[1]}
                       WHERE  user_id = {user_id}
                         AND  run_id  = {run_id}
                       ;")
  DBI::dbGetQuery(connection, query)
  print("Done updating account bounding box notes.")
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns model information
get_any_needed_model_info <- function(
  connection
) {
  tryCatch({
  models_needed <- DBI::dbGetQuery(connection, glue::glue('SELECT user_id, run_id, time_since_model_desired FROM gen_management.accounts WHERE model_status = \'model_needed\' and model_status IS NOT NULL;'))
  models_needed <- as.data.frame(models_needed)

  if ( length(models_needed) == 0 ) {
    return(NULL)
  } else {
    models_needed$time_since_int <- as.integer(models_needed$time_since_model_desired)
    return(models_needed)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns table with information on where a given user is in relation to other users in line for a model
get_model_timeline_information <- function(
  connection,
  user_id,
  run_id
) {
  tryCatch({
  run_id <- DBI::dbQuoteLiteral(connection, run_id)
  table <- DBI::dbGetQuery(connection, glue::glue('SELECT username, run_id, model_status, time_since_model_desired FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = {run_id};'))
  table <- as.data.frame(table)
  table$model_status <- ifelse(table$model_status == 'model_needed', 'Model Production Requested and in Queue, Not Complete',
         ifelse(table$model_status == 'no_model_desired', 'No Model Requested',
                ifelse(table$model_status == 'model_estimation_completed', 'Model Complete, Ready for Review and Download',
                       ifelse(table$model_status == 'model_currently_running' || table$model_status == 'model_needed_running', 'Model Currently in Production, Not Complete',
                              'should not be here'
                              ))))
  colnames(table)<- c("User Name (lower case)","Scenario ID","Model Production Status", "Time Model Last Requested")

  if ( length(table) == 0 ) {
    return(NULL)
  } else {
    if (table$`Model Production Status` == 'Model Production Requested and in Queue, Not Complete') {
      table$time_since_int <- as.integer(table$`Time Model Last Requested`)
      q <- get_any_needed_model_info(connection)
      queue <- which(table$time_since_int %in% sort(q$time_since_int, decreasing = TRUE))
      queue <- data.frame(n = queue)
      table <-cbind(table, queue)
      total <- data.frame(t = nrow(q))
      table <- cbind(table, total)
      table <- within(table, rm(time_since_int))
      names(table)[names(table)=="t"] <- "Total Number in Model Production Queue"
      names(table)[names(table)=="n"] <- "Number in Model Production Queue"
    }

    return(table)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns status of model 
model_production_status <- function (
  connection,
  user_id,
  run_id
  ){
  tryCatch({
  run_id <- DBI::dbQuoteLiteral(connection, run_id)
  status <- DBI::dbGetQuery(connection, glue::glue('SELECT model_status FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = {run_id};'))
  return(status)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}


# returns true is attributes are in dataframe 
attribute_check <- function(
  data,
  attributes
) {
  tryCatch({
  if( attributes %in% names(data) ) {
    TRUE
  } else {
    FALSE
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns max bounding box from list of spatial tables
get_max_bounds <- function(list_of_objects){
  tryCatch({
  bbox <- c(xmin=NULL, xmin=NULL, ymax=NULL, ymax=NULL)
  for (i in list_of_objects){
    
    for (k in 1:4){
      if ( is.null(bbox[k]) || is.na(bbox[k]) ) {
        bbox[k] <- i[k]
      }
      else {
        if ( k == 1 || k == 2){
          if (bbox[k] > i[k]){
            bbox[k] <- i[k]
          }
        } else {
          if (bbox[k] < i[k]){
            bbox[k] <- i[k]
          }
        }
      }
    }
  }
  return(bbox)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

