-- roads.lua — Flex config: loads OSM roadway centerlines
-- Target: static.osm_centerlines  (change SRID below if needed)

local srid = 4326  -- use 4326 (lon/lat)

-- Highway values to keep (vehicular roads)
local ROAD_TAGS = {
  motorway=true, motorway_link=true, trunk=true, trunk_link=true,
  primary=true, primary_link=true, secondary=true, secondary_link=true,
  tertiary=true, tertiary_link=true, unclassified=true, residential=true,
  living_street=true, service=true, road=true
}

-- Define destination table (schema + table name)
local roads = osm2pgsql.define_table({
  schema  = 'static',
  name    = 'osm_centerlines',
  ids    = { type = 'way', id_column = 'osm_id' },
  columns = {
    { column = 'highway',  type = 'text' },
    { column = 'name',     type = 'text' },
    { column = 'ref',      type = 'text' },
    { column = 'oneway',   type = 'text' },
    { column = 'maxspeed', type = 'text' },
    { column = 'surface',  type = 'text' },
    { column = 'bridge',   type = 'text' },
    { column = 'tunnel',   type = 'text' },
    { column = 'access',   type = 'text' },
    { column = 'lanes',    type = 'int' },
    { column = 'tags',     type = 'jsonb' },  -- keep all tags for later use
    { column = 'geom',     type = 'linestring', projection = srid, not_null = true },
  },
  indexes = {
    { column = 'geom', method = 'gist' }
  }
})

local function is_area(tags) return tags.area == 'yes' end

function osm2pgsql.process_way(obj)
  local h = obj.tags.highway
  if not h or not ROAD_TAGS[h] then return end
  if is_area(obj.tags) then return end

  local g = obj:as_linestring()
  if not g then return end

  roads:insert{
    highway  = h,
    name     = obj.tags.name,
    ref      = obj.tags.ref,
    oneway   = obj.tags.oneway,
    maxspeed = obj.tags.maxspeed,
    surface  = obj.tags.surface,
    bridge   = obj.tags.bridge,
    tunnel   = obj.tags.tunnel,
    access   = obj.tags.access,
    lanes    = tonumber(obj.tags.lanes),
    tags     = obj.tags,
    geom     = g
  }
end
