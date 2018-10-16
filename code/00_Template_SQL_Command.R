pm_mass_cmd<-"SELECT \"Monitor_ID\",avg(\"pm_mass\") AS \"mass\",12*date_part('year',age(date_trunc('month',\"Date Local\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Date Local\"),'2001-01-01'))AS \"m_month\"
FROM \"PM_MASS_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" AS \"city\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<RADIUS)AS tbl
WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2001' AND '12/31/2018') AND \"city\"='CITYNAME'
GROUP BY \"Monitor_ID\",\"m_month\""

total_gas_cmd<-"
SELECT sum(\"Monthly Gas\") AS \"Prod\",COUNT(\"Monthly Gas\") As \"Num\",\"Type\",12*date_part('year',age(date_trunc('month',\"Monthly Production Date\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Monthly Production Date\"),'2001-01-01'))AS \"m_month\"
FROM \"production_table\",(SELECT \"API/UWI\" AS \"ID\",\"city_state\" AS \"city\" ,\"Drill Type\" AS \"Type\" FROM  \"Gas_Well_Headers\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"gas_well_geom\",\"radnet_geom\")<RADIUS)AS tbl
WHERE \"API/UWI\" = \"ID\" AND (\"Monthly Production Date\" BETWEEN '01/01/2001' AND '12/31/2018') AND \"city\"='CITYNAME'
GROUP BY \"m_month\",\"Type\"
ORDER BY \"m_month\""

pm_spec_cmd<-"
SELECT \"Monitor_ID\",avg(\"pm_spec\") AS \"spec\",\"Parameter Name\",12*date_part('year',age(date_trunc('month',\"Date Local\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Date Local\"),'2001-01-01'))AS \"m_month\"
FROM \"PM_Spec_Measurement\",(SELECT \"Uni_ID\" AS \"ID\",\"city_state\" AS \"city\" FROM  \"PM_Monitors\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"pm_monitor_geom\",\"radnet_geom\")<RADIUS)AS tbl
WHERE \"Monitor_ID\" = \"ID\" AND (\"Date Local\" BETWEEN '01/01/2001' AND '12/31/2018') AND city='CITYNAME'
GROUP BY \"Monitor_ID\",\"m_month\",\"Parameter Name\"
"

radnet_raster_cmd<-"
SELECT \"city_state\",(K_stats).\"mean\" AS \"Kmeans\",(K_stats).\"stddev\" AS \"Ksd\",(Th_stats).\"mean\" AS \"Thmeans\",(Th_stats).\"stddev\" AS \"Thsd\",(U_stats).\"mean\" AS \"Umeans\",(U_stats).\"stddev\" AS \"Usd\"
FROM(SELECT ST_SummaryStatsAgg(ST_Clip(\"rast\",ST_Buffer(ST_Transform(radnet_geom,ST_SRID(\"rast\")),50000)),1,TRUE) AS K_stats,
ST_SummaryStatsAgg(ST_Clip(\"rast\",ST_Buffer(ST_Transform(radnet_geom,ST_SRID(\"rast\")),50000)),2,TRUE) AS Th_stats,
ST_SummaryStatsAgg(ST_Clip(\"rast\",ST_Buffer(ST_Transform(radnet_geom,ST_SRID(\"rast\")),50000)),3,TRUE) AS U_stats,
\"city_state\"
FROM \"RadNet_Sp\",\"USGS_RadioRaster\"
GROUP BY \"city_state\") AS foo;
"

radnet_basin_cmd<-"
SELECT \"city_state\",\"name\" AS \"basin_name\"
FROM \"RadNet_Sp\", \"us_basin\"
WHERE ST_Intersects(\"radnet_geom\",\"basin_geom\")
ORDER BY \"city_state\"
"

gas_construction_cmd<-"
SELECT \"API/UWI\",\"Spud Date\",\"Completion Date\",\"city_state\",\"Last Prod Date\",\"Drill Type\"
FROM \"Gas_Well_Headers\",\"RadNet_Sp\" 
WHERE ST_DistanceSphere(\"gas_well_geom\",\"radnet_geom\")<RADIUS
ORDER BY \"Spud Date\"
"

oil_construction_cmd<-"
SELECT \"API/UWI\",\"Spud Date\",\"Completion Date\",\"city_state\",\"Last Prod Date\",\"Drill Type\"
FROM \"Oil_Well_Headers\",\"RadNet_Sp\" 
WHERE ST_DistanceSphere(\"oil_well_geom\",\"radnet_geom\")<RADIUS
ORDER BY \"Spud Date\"
"