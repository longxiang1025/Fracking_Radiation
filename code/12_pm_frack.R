for(r in c(5000,15000,20000)){
  SQL<-"
  SELECT \"Uni_ID\", 
  sum(\"Monthly_Oil\") As \"Prod_Oil\",sum((\"Monthly_Oil\">0)::int) As \"Oil_Num\",
  sum(\"Monthly_Gas\") As \"Prod_Gas\",sum((\"Monthly_Gas\">0)::int) As \"Gas_Num\",
  sum(\"Monthly_Water\") AS \"Prod_Water\",sum((\"Monthly_Water\">0)::int) As \"Water_Num\",
  \"Prod_Year\" AS \"YEAR\", \"Prod_Month\" AS \"MONTH\",
  avg(ST_DistanceSphere(\"pm_monitor_geom\",\"well_geom\")) AS \"Dist\",
  \"Drill Type\" AS \"Type\"
  FROM (\"PM_Monitors\" LEFT JOIN \"Well_Headers\" ON ST_DistanceSphere(\"pm_monitor_geom\",\"well_geom\")<RADIUS) LEFT JOIN \"Well_Production_Table\" ON \"API\"=\"API/UWI\"
  GROUP BY \"Uni_ID\",\"YEAR\",\"MONTH\",\"Type\"
  ORDER BY \"Uni_ID\",\"YEAR\",\"MONTH\",\"Type\"
  "
  SQL<-gsub(pattern="RADIUS",replacement=as.character(r),SQL)

  month_prod<-dbGetQuery(con,SQL)
  save(month_prod,file=here::here("data",paste0("Mon_PM_Gas_Oil_",r/1000,".RData")))
}

