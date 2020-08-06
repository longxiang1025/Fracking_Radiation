library(SplitR)
library(magrittr)
library(ggplot2)
trajectory_model <-
  create_traj_model() %>%
  add_grid(
    lat = 42.0,
    lon = -72.0) %>%
  add_params(
    height = 0,
    duration = 24,
    run_period = "2015-07-01",
    daily_hours = 0,
    direction = "backward",
    met_type = "narr") %>%
  run_model()
trajectory_df <-
  trajectory_model %>% get_output_df()
trajectory_plot(trajectory_model$traj_df)
trajectory_df<-trajectory_df[!is.na(trajectory_df$month),]

trajectory_model %>% trajectory_plot()
ggplot2::ggplot(data=trajectory_df[trajectory_df$receptor==2,])+
  geom_path(aes(x=lon,y=lat,color=receptor))+
  geom_label(aes(x=lon,y=lat,label=hour))
