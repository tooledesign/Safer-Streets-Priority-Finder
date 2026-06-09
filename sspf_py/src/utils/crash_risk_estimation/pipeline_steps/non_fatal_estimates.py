from ..datastore import DataStore
from ..config import FatalModes
from dataclasses import replace
import pandas as pd


def compute_non_fatal_estimates(
    datastore: DataStore,
) -> DataStore:
    """
    Compute non-fatal estimates based on the fatal estimates.
    """
    # pull input data from datastore
    fatal_estimates_gdf = datastore.fatal_estimates.copy()

    # pull severity ratio from datastore
    severity_ratio_df = pd.read_sql(
        f"SELECT * FROM {datastore.config.static_schema}.{datastore.config.severity_ratio_table}",
        con=datastore.config.db_engine,
    )

    all_severities = ['k', 'a', 'b', 'c', 'o']

    # ensure ratio columns are numeric (guard against TEXT columns in DB)
    for col in severity_ratio_df.columns:
        if col != 'state':
            severity_ratio_df[col] = pd.to_numeric(severity_ratio_df[col], errors='coerce')

    study_area_ratio = severity_ratio_df[
        severity_ratio_df['state'] == datastore.config.state_name
    ]

    if study_area_ratio.empty:
        raise ValueError(f"No severity ratios found for state: {datastore.config.state_name}")
    
    # Calculate the ratio for each severity
    
    all_severity_estimates_gdf = fatal_estimates_gdf.copy()

    for mode in FatalModes:
        
        # this part of the code take k crashes as a Nx1 array
        # and take the kabco ratios as a 1x5 array from the ratio table
        # then multiply them to get the all severity estimates (Nx5 array)

        # Check if the fatal count column exists
        k_column = f"est_{mode.value}_k"  # Fatal count column
        if k_column not in all_severity_estimates_gdf.columns:
            print(f"Warning: {k_column} not found in fatal estimates")
            continue

        # get k crashes array
        k_estimates = all_severity_estimates_gdf[[k_column]].to_numpy()
        
        # get ratio array
        all_severity_ratios = study_area_ratio[
            [col for col in study_area_ratio.columns if col.startswith(f"{mode.value}_")]
            ].to_numpy()
        
        # get non-fatal estimates
        all_severity_estimates = k_estimates * all_severity_ratios
        all_severity_estimates_gdf[
            [f"est_{mode.value}_{severity}" for severity in all_severities]
            ] = all_severity_estimates

    # Reorder columns to have group estimates for the same mode together
    other_columns = [
        col for col in all_severity_estimates_gdf.columns
        if not col.startswith('est_')
    ]
    ordered_columns  = other_columns + \
        [f"est_{mode.value}_{severity}" for mode in FatalModes for severity in all_severities]
    
    all_severity_estimates_gdf = all_severity_estimates_gdf[ordered_columns]

    # update datastore
    datastore = replace(
        datastore,
        all_severity_estimates=all_severity_estimates_gdf,
    )
    return datastore

def upload_all_severity_estimates(
    datastore: DataStore,
) -> DataStore:
    """
    Upload crash estimates to the database.
    """
    with datastore.config.db_engine.connect() as conn:
        datastore.all_severity_estimates.to_postgis(
            name=datastore.config.all_severity_estimates_table,
            con=conn,
            if_exists="replace",
            index=False,
            schema=datastore.config.debug_schema,
            chunksize=5000,
        )
    return replace(datastore, all_severity_estimates=None)
