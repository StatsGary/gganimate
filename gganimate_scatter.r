install_or_load_pack <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
  #I know I should be using purr here, but this is before the Tidyverse is loaded. I know you Tidyverse trend setters will have me here.
}

packages <- c("ggplot2", "tidyverse", "gganimate", "dplyr", "gifski", "png", "lubridate")
install_or_load_pack(packages)

#Animation of new and follow up ratios over months

theme_set(theme_minimal())

set.seed(123)

new <- ifelse(rnorm(10000) <=0, 1, rnorm(10000)^2)
fup <- ifelse(rnorm(10000) <=0, 1, rnorm(10000)^2)
ratio <- fup/new
date_seq1 <- c(seq(as.Date("2015/1/1"), by = "day", length.out = 2500),
               seq(as.Date("2015/1/1"), by = "day", length.out = 2500),
               seq(as.Date("2015/1/1"), by = "day", length.out = 2500),
               seq(as.Date("2015/1/1"), by = "day", length.out = 2500))
area <- c(rep("Cancer",2500), rep("Medicine",2500),rep("Surgery",2500), rep("Family Health",2500))

op_df <- tibble(date=date_seq1,
                new=round(new, digits = 2)*2, 
                fup=round(fup, digits = 2)*5, 
                fup_new_ratio=round(ratio, digits=2),
                division=area,
                year=round(year(date_seq1), digits = 0),
                month=round(month(date_seq1),digits = 0))

str(op_df)

#Uses random sampling to arrive at 500 random observations and then I raise them to the 
#power of 2 to arrive at something that looks more like new and follow up numbers in
#outpatient clinics


#Here I also createa random sequence of dates from 1/1/2017 by day and terminate at length 500


plot <- ggplot(data=op_df,
               aes(x=op_df$new,
                   y=op_df$fup,
                   size=op_df$fup_new_ratio,
                   color=factor(op_df$division))) + geom_point(show.legend = F,
                                                               alpha= 0.7) 
plot <- plot +  scale_fill_viridis_d() + scale_size(range=c(3,12)) + 
  labs(title= "New and Follow up ratio",
       x="New appointments",
       y="FUP appointments",
       caption = "Produced by Gary Hutson")


plot2 <- plot + transition_time(op_df$year) + labs(title="Year: {frame_time}",
                                                   x="FUP",
                                                   y="New")

image <- animate(plot2)


anim_save("op_by_year_scatter.gif", image)

plot + facet_wrap(~op_df$division) +
  transition_time(op_df$year) +
  labs(title="Year: {frame_time}",
       x="FUP",
       y="New")
image2 <- animate(plot)
image2
anim_save("op_by_year_by_division.gif")