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