long_job_crashes_to_postgresql <- function(
  connection,
  user_id,
  run_id,
  crashes_utm,
  table, 
  schema, 
  geodata, 
  promote_to_multi, 
  geom_type
) { 
  
  # sending crashes to database 
  sf_to_postgres_as_wkt(connection=connection, table=table, schema=schema, geodata=geodata, promote_to_multi=promote_to_multi, geom_type=geom_type)
  
  # creating geom from WKT
  create_geom_from_wkt(connection=connection, schema=schema, table=table, srid=4326, geom_type=geom_type)
  
  # creating primary key 
  psql_create_pkey(connection=connection, table=table, pkey_name = paste0("tdg_id_", toString(user_id), '_', toString(run_id)))
  
  # creating spatial index 
  psql_add_s_index(connection=connection, table = table, index_name = paste0('index_', toString(user_id), '_', toString(run_id), '_'))
  
  # update crs
  psql_update_epsg(connection=connection, table=table, new_epsg=crashes_utm)
  
  #  flag crashes in study area and bind functional classification 
  q <- glue::glue('
        
                         ALTER TABLE local_user_data.crashes_{user_id}_{run_id} 
                         ADD COLUMN in_sa_{user_id}_{run_id} BOOLEAN DEFAULT FALSE;
                         
                         UPDATE local_user_data.crashes_{user_id}_{run_id}  c
                         SET in_sa_{user_id}_{run_id} = TRUE 
                         FROM local_user_data.study_area_{user_id}_{run_id} s
                         WHERE ST_INTERSECTS(c.geom, s.geom);
                         
                         ALTER TABLE local_user_data.crashes_{user_id}_{run_id} 
                         ADD COLUMN fclass_mapped TEXT DEFAULT \'Unknown Functional Classification\';
    
                         UPDATE local_user_data.crashes_{user_id}_{run_id} t
                         SET fclass_mapped = usdot_fun_class_mapped
                         FROM
                             (SELECT 
                              DISTINCT ON (c.tdg_id_{user_id}_{run_id}) c.tdg_id_{user_id}_{run_id}, 
                              i.usdot_fun_class_mapped
                             FROM 
                              local_user_data.crashes_{user_id}_{run_id} c, local_user_data.roads_{user_id}_{run_id} i
                             WHERE 
                              ST_DWithin(c.geom, i.geom, 250) 
                              
                             ORDER BY 
                              c.tdg_id_{user_id}_{run_id}, 
                              ST_Distance(c.geom, i.geom)
                              ) z
                         WHERE t.tdg_id_{user_id}_{run_id} = z.tdg_id_{user_id}_{run_id};

                         
                        ')
  
  DBI::dbExecute(connection, q)
}


long_job_roads_to_postgresql <- function(
  connection,
  user_id, 
  run_id,
  roads_utm,
  table, 
  schema, 
  geodata, 
  promote_to_multi, 
  geom_type
) { 
  
  # sending to db 
  sf_to_postgres_as_wkt(connection=connection, table=table, schema=schema, geodata=geodata, promote_to_multi=promote_to_multi, geom_type=geom_type)
  
  # rebuild geoms 
  create_geom_from_wkt(connection=connection, schema=schema, table=table, srid=4326, geom_type=geom_type)
  
  # create primary key 
  psql_create_pkey(connection=connection, table=table, pkey_name = paste0("tdg_id_", toString(user_id), '_', toString(run_id)))
  
  # creating spatial index 
  psql_add_s_index(connection=connection, table = table, index_name = paste0('index_', toString(user_id), '_', toString(run_id)))
  
  # Updated SRID spatial index 
  psql_update_epsg(connection=connection, table=table, new_epsg=roads_utm)
  
}
