### Follow these steps to repeat the analysis from Thesis

To get the trip counts for entire dataset run the following:

```time python get_trip_counts.py -c nyc -d '/Users/colinbroderick/repos/bike-correlations/data/nyc/citi_bike_usage_stats/' -s '/Users/colinbroderick/repos/temp/bike-correlations/data/station_latlons_nyc.txt' -p total```

To get the correlation coefficients run the following:

```time python reg1.py -c nyc -day total``` 

Open the R script `../python/nyc1.R` and set the period you wish to analyze (total, day of the week)

```
time R CMD BATCH nyc1.R

```

## get the weekday totals
time python get_trip_counts.py -c nyc -d '/Users/colinbroderick/repos/bike-correlations/data/nyc/citi_bike_usage_stats/' -s '/Users/colinbroderick/repos/temp/bike-correlations/data/station_latlons_nyc.txt' -p weekday

## get the correlation coefficients for each weekday
time python reg1.py -c nyc -day mon


# getting the pluto datasets
ogr2ogr -f 'GeoJSON' -s_srs EPSG:2263 -t_srs EPSG:4326 mn_mappluto.json MNMapPLUTO.shp 
ogr2ogr -f "PostgreSQL" PG:"dbname=osm_ie user=colinbroderick" mn_mappluto.json -nln mn_pluto

-- add a column for landuse descriptions
ALTER TABLE mn_pluto ADD COLUMN lu_descript TEXT;

-- add values based on landuse codes
UPDATE mn_pluto SET lu_descript = 
    CASE WHEN landuse = '01' THEN 'One and Two Family Buildings'
         WHEN landuse = '02' THEN 'Multi-Family Walkup'
         WHEN landuse = '03' THEN 'Multi-Family with Elevator'
         WHEN landuse = '04' THEN 'Mixed Residential & Commerical'
         WHEN landuse = '05' THEN 'Commerical & Office'
         WHEN landuse = '06' THEN 'Industrial & Manufacturing'
         WHEN landuse = '07' THEN 'Transport & utility'
         WHEN landuse = '08' THEN 'Public Facilities & Insitutions'
         WHEN landuse = '09' THEN 'Open Space & Recreation'
         WHEN landuse = '10' THEN 'Parking Facilities'
         WHEN landuse = '11' THEN 'Vacant Land'
         WHEN landuse IS NULL THEN 'N/A'
         END;

CREATE TABLE nyc_test AS
SELECT ROW_NUMBER() OVER(ORDER BY a.ogc_fid) AS row_id, a.* FROM mn_pluto AS a, ny_sa_n AS b WHERE ST_Intersects(a.wkb_geometry, b.wkb_geometry)

UPDATE nyc_test SET lu_descript = 
    CASE WHEN landuse = '01' THEN 'Residential'
         WHEN landuse = '02' THEN 'Residential'
         WHEN landuse = '03' THEN 'Residential'
         WHEN landuse = '04' THEN 'Mixed Residential & Commerical'
         WHEN landuse = '05' THEN 'Commerical'
         WHEN landuse = '06' THEN 'Commerical'
         WHEN landuse = '07' THEN 'Transport & utility'
         WHEN landuse = '08' THEN 'Public Facilities & Insitutions'
         WHEN landuse = '09' THEN 'Open Space & Recreation'
         WHEN landuse = '10' THEN 'Parking Facilities'
         WHEN landuse = '11' THEN 'Vacant Land'
         WHEN landuse IS NULL THEN 'N/A'
         END;

ogr2ogr -f "PostgreSQL" PG:"dbname=osm_ie user=colinbroderick" sa_neighbourhoods.geojson -nln ny_sa_n