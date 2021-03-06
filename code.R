library(ggplot2)
library(zoo)
library(lubridate)
library(reshape2)
library(scales)
library(ggthemes)

setwd("~/Developer/2020-in-booleans")

data = read.csv("raw.csv")
data$date = as.Date(data$X.1, format = "%m/%d/%y")
data$week = week(data$date)
data$month = format(data$date,"%B")
data$month = factor(data$month, list(
  "January", "February", "March", 
  "April", "May", "June", 
  "July", "August", "September", 
  "October", "November", "December"
))
data$yearmonth = as.yearmon(data$date)
data$yearmonthf = factor(data$yearmonth)
data$day_of_week = factor(data$X, list(
  "Sunday", 
  "Monday", 
  "Tuesday", 
  "Wednesday", 
  "Thursday", 
  "Friday", 
  "Saturday"
))

data[data==0] = "False"
data[data==1] = "True"

# calculate week of month, where sunday is new week
weekofmonth = rep(0, length(data$date))
weekNum = 1
currentMonth = 1
for (i in (1:length(data$date))) {
  date = data$date[i]
  if (weekdays(date) == "Sunday") {
    weekNum = weekNum + 1
  }
  if (month(date) > currentMonth) {
    currentMonth = currentMonth + 1
    weekNum = 1
  }
  weekofmonth[i] = weekNum
}
data$monthweek = weekofmonth
data$monthweek = factor(data$monthweek, list(6, 5, 4, 3, 2, 1))

#==========================================================
# Variable labels
#==========================================================

variable_labels = c(
  `felt.enough.sleep` = "Enough\nSleep",
  `slept.at.home` = "Slept at Home",
  `shower` = "Shower",
  `toothbrush.morning` = "Brush Teeth Morning",
  `toothbrush.night` = "Brush Teeth Night",
  `poop` = "Poop",
  `breakfast` = "Break-\nfast",
  `lunch` = "Lunch",
  `dinner` = "Dinner",
  `period` = "Period",
  `birth.control` = "Birth Control",
  `anti.depressant` = "Anti-Depressant",
  `vitamins` = "Vitamins",
  `advil` = "Advil",
  `excederin` = "Excederin",
  `adderall` = "Adderall",
  `caffeine` = "Caffeine",
  `ambien` = "Ambien",
  `alcohol` = "Alcohol",
  `weed` = "Weed",
  `chores` = "Chores",
  `personal.coding` = "Personal\nCoding",
  `feel.sad.about.leaving.C1` = "Sad about Leaving C1",
  `feel.happy.about.leaving.C1` = "Happy about Leaving C1",
  `went.to.work` = "Went to\nWork",
  `happy.about.work` = "Happy about Work",
  `productive.at.work` = "Productive at Work",
  `social.with.my.friends..non.work` = "",
  `interaction.with.my.family` = "",
  `happy` = "Happy",
  `angry` = "Angry",
  `Annoyed` = "Annoyed",
  `cried` = "Cried",
  `sad.about.relationship` = "Sad about Relationship",
  `abandoned.by.sean` = "Abandoned",
  `upset.at.sean` = "Upset at Sean",
  `sean.drunk` = "Sean Drunk",
  `worked.remotely` = "WFH",
  `sad.about.work` = "Sad about Work",
  `social.with.my.friends..work` = "",
  `social.with.sean.s.friends` = "",
  `interaction.with.sean.s.family` = "",
  `sad` = "Sad",
  `stressed.or.anxious` = "Felt\nStressed",
  `Journaled` = "Journaled",
  `happy.about.relationship` = "Happy about Relationship",
  `sex` = "Sex",
  `had.own.activities..other.than.work` = "",
  `sean.alcohol` = "Sean Alcohol",

  `January` = "Jan", 
  `February` = "Feb", 
  `March` = "Mar", 
  `April` = "Apr", 
  `May` = "May", 
  `June` = "Jun", 
  `July` = "Jul", 
  `August` = "Aug", 
  `September` = "Sep", 
  `October` = "Oct", 
  `November` = "Nov", 
  `December` = "Dec"
)

#==========================================================
# Helper function to melt and plot data
#==========================================================

melt_and_plot = function(data, subtitle) {
  melt = melt(
    data = data, 
    id = names(data)[1:4])
  
  ggplot(
    melt, 
    aes(
      day_of_week, 
      monthweek, 
      fill = as.factor(value)
    )
  ) + 
    coord_equal(ratio = 1) + 
  geom_tile(color = "white") + 
  facet_grid(
    variable ~ month, 
    switch = "y", 
    space = "free", 
    labeller = as_labeller(variable_labels)
  ) +
  labs(
    y = "",
    x = "",
    title = "2020* in Booleans",
    subtitle = subtitle,
    fill = "Legend",
    caption = "* Forgot to record data for a couple days in mid-November, which made me too sad to continue."
  ) + 
  theme(
    text = element_text(family = "mono", color = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(),
    plot.title = element_text(size = 60, face = "bold", margin = margin(b = 40)),
    # plot.subtitle = element_text(size = 40, margin = margin(t = 20, b = 40)),
    plot.subtitle = element_blank(),
    plot.caption = element_text(size = 30, margin = margin(t = 30, b = 20), hjust = 0),
    strip.text.x = element_text(size = 25, face = "bold"),
    strip.text.y = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 30),
    legend.box.margin = margin(l = 40),
    legend.background = element_rect(fill = "black"),
    plot.margin = margin(t = 50, r = 80, b = 20, l = 80),
    plot.background = element_rect(fill = "black")
  )
}

#==========================================================
# Plots with data columns subset
#==========================================================

#------------------
# Main
#------------------

data.main = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "felt.enough.sleep" = data$felt.enough.sleep,
  "shower" = data$shower,
  "poop" = data$poop,
  "breakfast" = data$breakfast,
  "lunch" = data$lunch,
  "dinner" = data$dinner,
  "caffeine" = data$caffeine,
  "alcohol"= data$alcohol,
  "weed" = data$weed,
  "chores" = data$chores,
  "personal.coding" = data$personal.coding,
  "went.to.work" = data$went.to.work,
  "worked.remotely" = data$worked.remotely,
  "stressed.or.anxious" = data$stressed.or.anxious,
  "sex" = data$sex
)

melt_and_plot(data.main, "Daily life attributes of a 26 year-old Asian female, living in the US")

#------------------
# SFW columns
#------------------

data.sfw = data.frame(
  "date" = data$date,
  "month" = data$month,
  "day_of_week" = data$day_of_week,
  "monthweek" = data$monthweek,
  
  "felt.enough.sleep" = data$felt.enough.sleep,
  "shower" = data$shower,
  "breakfast" = data$breakfast,
  "lunch" = data$lunch,
  "dinner" = data$dinner,
  "caffeine" = data$caffeine,
  "vitamins"= data$vitamins,
  "chores" = data$chores,
  "personal.coding" = data$personal.coding,
  "went.to.work" = data$went.to.work,
  "worked.remotely" = data$worked.remotely,
  "happy" = data$happy,
  "sad" = data$sad,
  "cried" = data$cried,
  "Journaled" = data$Journaled
)

melt_and_plot(data.sfw, "")

#------------------
# Work
#------------------

data.work = data.frame(
  "date" = data$date,
  "month" = data$month,
  "day_of_week" = data$day_of_week,
  "monthweek" = data$monthweek,

  "went.to.work" = data$went.to.work,
  "happy.about.work" = data$happy.about.work,
  "sad.about.work" = data$sad.about.work,
  "productive.at.work" = data$productive.at.work,
  "worked.remotely" = data$worked.remotely
)

melt_and_plot(data.work, "Work")

#==========================================================
# Wake and get up time
#==========================================================

wake_up_data = data.frame(
  "date" = as.POSIXct(data$date),
  "wake_up_time" = strptime(data$wake.up.time, "%I:%M %p"),
  "get_up_time" = strptime(data$get.up.time, "%I:%M %p")
)

wake_up_data$date = wake_up_data$date + 6 * 60 * 60
wake_up_data$time_diff = difftime(wake_up_data$get_up_time, wake_up_data$wake_up_time)
wake_up_data$wake_up_time[which(is.na(wake_up_data$wake_up_time))] = strptime("12:00 AM", "%I:%M %p")
wake_up_data$get_up_time[which(is.na(wake_up_data$get_up_time))] = strptime("12:00 AM", "%I:%M %p")

wake_up_melted = melt(
  wake_up_data,
  id.vars = c("date"), 
  measure.vars = c("get_up_time", "wake_up_time")
)

wake_up_melted$label = ifelse(
  wake_up_melted$variable == "get_up_time",
  "Get-Up Time",
  "Wake-Up Time"
)

colors = c("yellow", "purple")

ggplot(
  wake_up_melted, 
  aes(
    x = date,
    y = value,
    group = label,
    fill = label
  )
) + 
  geom_polygon(
    alpha = 0.8
  ) +
  scale_color_manual(
    values = colors
  ) +
  scale_fill_manual(
    values = colors
  ) +
  coord_polar(
  ) +
  scale_x_datetime(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  scale_y_datetime(
    date_labels = "%I:%M %p"
  ) +
  labs(
    y = "",
    x = "",
    title = "Wake-Up and Get-Up Times for (most of) 2020",
    subtitle = "",
    caption = "",
    fill = ""
  ) +
  theme_solarized_2(light = FALSE) +
  theme(
    text = element_text(family = "mono", color = "white"),
    plot.title = element_text(size = 70, color = "white", face = "bold"),
    plot.margin = margin(t = 60, b = 20, l = 45, r = 55),
    panel.grid.major = element_line(color = "white"),
    axis.text.x = element_text(size = 40, color = "white", face = "bold"),
    axis.text.y = element_text(size = 40, color = "white", margin = margin(r = 20)),
    legend.position = "top",
    legend.text = element_text(size = 40, color = "white"),
    legend.title = element_blank(),
    legend.margin = margin(t = 30, b = 60),
    legend.spacing.x = unit(2, 'cm')
  )







