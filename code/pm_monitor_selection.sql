SELECT "Monitor_ID",avg("pm_spec") AS "spec","Parameter Name","city",12*date_part('year',age(date_trunc('month',"Date Local"),'2001-01-01'))+date_part('month',age(date_trunc('month',"Date Local"),'2001-01-01'))AS "m_month"
FROM "PM_Spec_Measurement",(SELECT "Uni_ID" AS "ID","city_state" AS "city" FROM  "PM_Monitors","RadNet_Sp" WHERE ST_DistanceSphere("pm_monitor_geom","radnet_geom")<50000)AS tbl
WHERE "Monitor_ID" = "ID" AND ("Date Local" BETWEEN '01/01/2001' AND '12/31/2018') AND "Parameter Name"= 'Sulfate PM2.5 LC'
GROUP BY "Monitor_ID","m_month","Parameter Name","city"