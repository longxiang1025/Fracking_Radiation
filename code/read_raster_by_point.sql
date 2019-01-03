SELECT avg(ST_Value("vwnd_rast",(ST_Transform("radnet_geom",880002)),TRUE)),"city_state"
FROM "VWind","RadNet_Sp"
GROUP BY "city_state"