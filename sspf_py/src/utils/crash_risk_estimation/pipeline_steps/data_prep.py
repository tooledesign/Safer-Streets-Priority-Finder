import geopandas as gpd
import numpy as np
from dataclasses import replace
from sqlalchemy import text
from ..datastore import DataStore

def get_study_data(datastore: DataStore) -> DataStore:
    """
    Get study data for the crash risk estimation pipeline.
    """

    roadway_sql = f"SELECT * FROM {datastore.config.input_roadway_table} WHERE functional_class != 'Omit From Analysis'"
    crashes_sql = f"SELECT * FROM {datastore.config.input_crash_table}"
    bg_sql = f"SELECT * FROM {datastore.config.input_bg_fatal_table}"

    with datastore.config.db_engine.connect() as conn:
        roadway_network = gpd.read_postgis(roadway_sql, con=conn, geom_col="geom")
        local_crash_data = gpd.read_postgis(crashes_sql, con=conn, geom_col="geom")
        estimated_fatal_by_bg = gpd.read_postgis(bg_sql, con=conn, geom_col="geom")

        # Load 0.05mi segments for crash distribution
        segments_0p05mi = None
        if datastore.config.input_segments_0p05mi_table:
            segments_sql = f"SELECT * FROM {datastore.config.input_segments_0p05mi_table}"
            segments_0p05mi = gpd.read_postgis(segments_sql, con=conn, geom_col="geom")

    datastore = replace(
        datastore,
        roadway_network=roadway_network,
        estimated_fatal_by_bg=estimated_fatal_by_bg,
        local_crash_data=local_crash_data,
        segments_0p05mi=segments_0p05mi,
    )

    return datastore
