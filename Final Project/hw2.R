
hw2 <- theme_update()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),

  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
    colour=gray(.5), size=.2),

  panel.border=element_rect(fill=F,colour=gray(.50)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.08,"cm"),
  panel.spacing.y = unit(0.03,"cm"),

  axis.ticks=element_blank(),
  axis.text=element_text(colour="grey13"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0), angle = 60, hjust = 1)
)


layout = rbind(c(NA,3,NA),
               c(1,2))