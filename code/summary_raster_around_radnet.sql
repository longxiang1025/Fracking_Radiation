SELECT "city_state",(K_stats)."mean" AS "Kmeans",(K_stats)."stddev" AS "Ksd",(Th_stats)."mean" AS "Thmeans",(Th_stats)."stddev" AS "Thsd",(U_stats)."mean" AS "Umeans",(U_stats)."stddev" AS "Usd"
FROM(SELECT ST_SummaryStatsAgg(ST_Clip("rast",ST_Buffer(ST_Transform(radnet_geom,ST_SRID("rast")),50000)),1,TRUE) AS K_stats,
ST_SummaryStatsAgg(ST_Clip("rast",ST_Buffer(ST_Transform(radnet_geom,ST_SRID("rast")),50000)),2,TRUE) AS Th_stats,
ST_SummaryStatsAgg(ST_Clip("rast",ST_Buffer(ST_Transform(radnet_geom,ST_SRID("rast")),50000)),3,TRUE) AS U_stats,
 "city_state"
FROM "RadNet_Sp","USGS_RadioRaster"
GROUP BY "city_state") AS foo;
