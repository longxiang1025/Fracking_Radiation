#' ---
#' title: "Quick check of the difference between "
#' author: "Longxiang Li"
#' date: "`r format(Sys.Date())`"
#' output: 
#'      html_document:
#'       toc: true
#'       fig_caption: yes
#' ---
#' 
#' #Background
#' 
pw <- {
"koutrakis"
}
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "Fracking_Data",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password
source(here::here("code","00_Template_SQL_Command.R"))
RadNet_City_List<-read_csv(here::here("data","Processed-RadNet-Beta-citylist.csv"))
r=25000
for(r in c(25000,50000,75000,100000)){
  total_gas_cmd<-"
SELECT sum(\"Monthly Gas\") AS \"Prod\",COUNT(\"Monthly Gas\") As \"Num\",\"city\",12*date_part('year',age(date_trunc('month',\"Monthly Production Date\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"Monthly Production Date\"),'2001-01-01'))AS \"m_month\"
  FROM \"production_table\",(SELECT \"API/UWI\" AS \"ID\",\"city_state\" AS \"city\" ,\"Drill Type\" AS \"Type\" FROM  \"Gas_Well_Headers\",\"RadNet_Sp\" WHERE ST_DistanceSphere(\"gas_well_geom\",\"radnet_geom\")<RADIUS)AS tbl
  WHERE \"API/UWI\" = \"ID\" AND (\"Monthly Production Date\" BETWEEN '01/01/2001' AND '12/31/2018')
  GROUP BY \"m_month\",\"city\"
  ORDER BY \"city\""
  cmd<-gsub(pattern="RADIUS",replacement=as.character(r),total_gas_cmd)
  rad_gas<-dbGetQuery(con,cmd)
  names(rad_gas)[3]<-"city_state"
  
  
  radon_cmd<-"
  SELECT \"citystate\",\"radon\"
  FROM \"Radon_Zones\",(SELECT \"city_state\" AS \"citystate\",\"STUSPS\" AS \"state_sn\",\"COUNTY_NAME\" AS \"county_name\" FROM \"RadNet_Sp\",\"us_state\",\"us_county\" WHERE ST_INTERSECTS(\"radnet_geom\",\"state_geom\") AND ST_INTERSECTS(\"radnet_geom\",\"county_geom\"))AS tb1
  WHERE \"state\"=\"state_sn\" AND \"county\"=\"county_name\"
  "
  rad_radon<-dbGetQuery(con,radon_cmd)
  names(rad_radon)[1]<-"city_state"
  
  total_beta_cmd<-"
  SELECT avg(result_amount) AS beta,12*date_part('year',age(date_trunc('month',\"collect_end\"),'2001-01-01'))+date_part('month',age(date_trunc('month',\"collect_end\"),'2001-01-01'))AS \"m_month\",EXTRACT(YEAR FROM \"collect_end\" ) AS \"YEAR\",\"city_state\"
  FROM \"radnet_measurement_table\"
GROUP BY \"m_month\",\"city_state\",\"YEAR\"
"
  nuclide_cmd<-"
SELECT avg(\"Result\"),EXTRACT(YEAR FROM to_date(\"Date\",'DD-MON-YY')) AS \"YEAR\",\"Location\",\"Nuclides\"
FROM \"Nuclide_Measurement\"
GROUP BY \"YEAR\",\"Location\",\"Nuclides\"
ORDER BY \"Location\",\"Nuclides\",\"YEAR\"
"
  rad_beta<-dbGetQuery(con,total_beta_cmd)
  rad_beta<-rad_beta[rad_beta$m_month>0,]
  rad_qs<-left_join(rad_beta,rad_gas,by=c("m_month","city_state"))
  rad_qs<-left_join(rad_qs,rad_radon)
  rad_qs$radon<-as.factor(rad_qs$radon)
  rad_qs[is.na(rad_qs$Prod),]$Prod<-0
  rad_qs[is.na(rad_qs$Num),]$Num<-0
  address<-strsplit(rad_qs$city_state,",")
  address<-do.call(rbind,address)
  state<-address[,2]
  rad_qs$state<-state
  rad_qs$state<-as.factor(rad_qs$state)
  rad_qs$city_state<-as.factor(rad_qs$city_state)
  
  nuc<-dbGetQuery(con,nuclide_cmd)
  names(nuc)[2]<-"year"
  nuc<-spread(data=nuc,key=Nuclides,value = avg)
  rad_qs<-left_join(rad_qs,nuc,by=c("YEAR"="year","city_state"="Location"))
  write.csv(rad_qs,file=here::here("data",paste0("beta_gas_nuc_",r/1000,".csv")),na="")
}
rad_qs$city_state<-as.factor(rad_qs$city_state)
g<-gam(beta~radon+Prod+s(m_month),data=rad_qs)


