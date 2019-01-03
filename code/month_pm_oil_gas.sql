SELECT "Uni_ID", 
sum("Monthly_Oil") As "Prod_Oil",sum(("Monthly_Oil">0)::int) As "Oil_Num",
sum("Monthly_Gas") As "Prod_Gas",sum(("Monthly_Gas">0)::int) As "Gas_Num",
sum("Monthly_Water") AS "Prod_Water",sum(("Monthly_Water">0)::int) As "Both_Num",
"Prod_Year" AS "YEAR", "Prod_Month" AS "MONTH",
avg(ST_DistanceSphere("pm_monitor_geom","well_geom")) AS "Dist",
"Drill Type" AS "Type"
FROM "PM_Monitors","Well_Headers","All_Production_Table"
WHERE "Uni_ID"=9 AND ST_DistanceSphere("pm_monitor_geom","well_geom")<10000 AND "API"="API/UWI"
GROUP BY "Uni_ID","YEAR","MONTH","Type"
ORDER BY "Uni_ID","YEAR","MONTH","Type";
