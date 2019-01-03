﻿SELECT "API/UWI","Monthly Gas","city",12*date_part('year',age(date_trunc('month',"Monthly Production Date"),'2001-01-01'))+date_part('month',age(date_trunc('month',"Monthly Production Date"),'2001-01-01'))AS "m_month"
FROM "production_table",(SELECT "API/UWI" AS "ID","city_state" AS "city" FROM  "Gas_Well_Headers","RadNet_Sp" WHERE ST_DistanceSphere("gas_well_geom","radnet_geom")<50000)AS tbl
WHERE "API/UWI" = "ID" AND ("Monthly Production Date" BETWEEN '01/01/2001' AND '12/31/2018') AND "city"='DALLAS,TX'