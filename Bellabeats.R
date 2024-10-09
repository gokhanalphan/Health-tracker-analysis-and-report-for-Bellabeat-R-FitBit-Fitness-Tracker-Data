## BELLABEAT...
## librarys need to loaded..

library(tidyverse)
library(scales)
library(janitor)
library(paletteer)

# DAILY ACTIVITIES

# Manual filtered for outliers. 
c_daily_act <- bind_rows(read_csv("dailyActivity_merged_1.csv", show_col_types = FALSE), 
                         read_csv("dailyActivity_merged_2.csv", show_col_types = FALSE)) %>%
  drop_na() %>% distinct() %>% 
  filter(TotalSteps > 0 & FairlyActiveMinutes < 150 & LightlyActiveMinutes < 580 & SedentaryMinutes < 1080) %>% 
  mutate(ActivityDate = mdy(ActivityDate)) %>% 
  mutate(totalActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes) %>% 
  arrange(Id, ActivityDate)


ggplot(c_daily_act, aes(x = TotalSteps, y = Calories)) + geom_jitter() + 
  geom_smooth(method = "loess", formula = "y ~ x", alpha = .5, color = "purple", fill = "lightgreen") + 
  labs(title = "Calories & Total Steps Correlation", x = "Total Steps") +
  annotate("rect", xmin = 1500, xmax = 10000, ymin = 800, ymax = 2200, alpha = 0.2, color = "#892523")

# In the chart shown there are 5 to 10 km walkers who burned 2k or below calories in a day.
# Average healthy woman needs to burn 1.8k - 2.4k cal. And man needs to 2.2k - 3k so under 10km and 2k calories
# might have some health issues in the future or having because lack of activities. 
ggplot(c_daily_act, aes(x = TotalDistance, y = Calories)) + geom_jitter() + 
  geom_smooth(method = "loess", formula = "y ~ x", alpha = .5, color = "purple", fill = "lightgreen") +
  labs(title = "Calorie & Total Distance Correlation", x = "Total Distance") +
  annotate("rect", xmin = 1, xmax = 8, ymin = 800, ymax = 2200, alpha = 0.2, color = "#892523")

# Total active minutes. 
ggplot(c_daily_act, aes(x = totalActiveMinutes, y = Calories)) + geom_point(aes(colour = totalActiveMinutes)) + 
  geom_smooth(size = .7, fill = "lightblue") + 
  scale_color_gradient(low = "green", high = "purple") +
  labs(title = "Calories & Active Minutes Comparing", x = "Active Minutes") +
  theme(legend.position = "none")

# 1440 min a day. Mean 6 hours sleep 360 min. Mean day 1080 min or below. Very right part of chart shows 
# deskjob workers and their colorie burned. Woman need to burn 1800-2400 kal, man 2200-3000 kal in a day!
#Chart very right down part show that users who have non active life don't burn enough kal in a day. 
# IN THE END all of non users need to stay healthy, so need to take their attention with devices, or
# notifications or ads. 
ggplot(c_daily_act, aes(x = SedentaryMinutes, y = Calories)) + geom_point(aes(colour = SedentaryMinutes)) + 
  geom_smooth(size = .7, fill = "lightblue", method = "loess", formula = "y ~ x") + 
  scale_color_gradient(low = "green", high = "purple") +
  labs(title = "Calories & Sedentary Minutes Comparing", x = "Sedentary Minutes") +
  theme(legend.position = "none")


# JUST WALK, ON COUCH, CROSSFIT, UPHILL OR UPSTAIR SPORTS AND RUNNERS.

ggplot(c_daily_act, aes(x = VeryActiveMinutes, y = VeryActiveDistance)) + geom_jitter()

ggplot(c_daily_act, aes(x = FairlyActiveMinutes, y = ModeratelyActiveDistance)) + geom_jitter() + geom_smooth()

ggplot(c_daily_act, aes(x = LightlyActiveMinutes, y = LightActiveDistance)) + geom_jitter(size = 1) + geom_smooth()


c_daily_longer <- c_daily_act %>% mutate(daysof = wday(ActivityDate, label = TRUE, abbr = FALSE)) %>% 
  select(Id, daysof, TotalSteps, TotalDistance, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, Calories) %>% 
  filter(TotalSteps > 0 & FairlyActiveMinutes < 150 & LightlyActiveMinutes < 580) %>% 
  pivot_longer(cols = c(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes), names_to = "ActivityTypes", values_to = "Minutes") %>% 
  mutate(ActivityTypes = fct_reorder(ActivityTypes, -Minutes))

ggplot(c_daily_longer, aes(x = Minutes, y = Calories)) + geom_jitter(aes(colour = ActivityTypes, shape = ActivityTypes)) +
  scale_color_manual(values=c("#892523", "#69b3a2", "purple")) +
  labs(title = "Calories & Active Minutes Correlation")

avg_act <- c_daily_act %>% 
  group_by(Id) %>% 
  summarize(across(everything(), ~ round(mean(.x, na.rm = TRUE), 2)),
    totalActiveHours = round(sum(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes, na.rm = TRUE) / 60, 1))

avg_act2 <- c_daily_longer %>% 
  group_by(daysof) %>% summarize(mean_steps = round(mean(TotalSteps)), mean_dist = round(mean(TotalDistance), 2))

# The chart show that non Bella users tend to walk or run especially Saturdays, which means of they like to travel or
# create time for themselves even its weekend. On the other hand weekdays are similar each other the could point that they are 
# focusing on their business. The brand focuses on fasionable devices for health and sport concerns people. 
ggplot(avg_act2) + geom_col(aes(x = daysof, y = mean_dist, fill = daysof)) +
  scale_fill_paletteer_d("ggprism::floral") +
  labs(title = "Daily Average Distance", y = "Distance in km") +
  theme(legend.position = "none", axis.title.x = element_blank())
  
  

# HEARTRATE IN SECONDS

c_heartrate <- bind_rows(read_csv("heartrate_seconds_merged_1.csv", show_col_types = FALSE), 
                         read_csv("heartrate_seconds_merged_2.csv", show_col_types = FALSE)) %>% 
  drop_na() %>% distinct() %>% mutate(Time = mdy_hms(Time)) %>% arrange(Id, Time)

smp_heartrate <- c_heartrate %>% group_by(Id) %>% 
  summarize(min_rate = min(Value), median_rate = round(median(Value), 0), max_rate = max(Value), std_deviation = round(sd(Value))) %>% 
  pivot_longer(cols = c(min_rate, median_rate, max_rate, std_deviation), names_to = "rate_types", values_to = "bpm_")

print(smp_heartrate %>% filter(rate_types == "std_deviation"))

# There is a outlier in the chart, I prefer to keep in that bc emphasize the how could be important hardware or 
# software in our devices.
ggplot(smp_heartrate %>% filter(rate_types != "std_deviation"), aes(x = reorder(rate_types, bpm_), y = bpm_, fill = rate_types)) + geom_boxplot() +
  labs(title = "BPM Range For Individuals", y = "BPM", fill = "BPM Ranges") +
  scale_y_continuous(breaks = seq(0, 220, by = 20)) + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  scale_fill_manual(values=c("#892523", "purple", "#69b3a2"))

# There are some seconds inputes, I round them to whole integers
round_min_hr <- c_heartrate %>% mutate(Time = floor_date(Time, "minute")) %>% group_by(Id, Time) %>% 
  summarize(avg_rate = round(mean(Value)), .groups = "drop")

# SLEEP IN MINUTES

c_sleep_min <- bind_rows(read_csv("minuteSleep_merged_1.csv", show_col_types = FALSE),
                         read_csv("minuteSleep_merged_2.csv", show_col_types = FALSE)) %>% 
  drop_na() %>% distinct() %>% mutate(date = mdy_hms(date), date = floor_date(date, "minute")) %>% 
  rename(Time = date) %>% arrange(Id, Time)

sleep_hr_join <- inner_join(c_sleep_min, round_min_hr, by = c("Id","Time")) %>% 
  filter(avg_rate > 90)

# Filtered users who have higher than 90 BPM heart rate in their sleeps, and phases are shown. Especially 
# when human in resting state (sleeping), heart rate should be stay between 60-80 BPM if sport background has that
# rate could be 40-60 BPM but lower that or higher 90 BPM point out there is a problem. In the chart below 
# There are some participants who has heart issue, anxiety, stress or, sleep apnea or measurement technical problem.
ggplot(sleep_hr_join, aes(x = value, y = avg_rate, colour = avg_rate)) + geom_jitter() + 
  labs(title = "Heartrates over 90 BPM in Sleep", x = "Sleep Phases", y = "BPM", colour = "Over-BPM Gauge") +
  scale_color_gradient(low = "orange", high = "red")

# WEIGHT LOG

c_weight <- bind_rows(read_csv("weightLogInfo_merged_1.csv", show_col_types = FALSE),
                      read_csv("weightLogInfo_merged_2.csv", show_col_types = FALSE)) %>% 
  mutate(height = round(sqrt(WeightKg / BMI), 2)) %>% 
  group_by(Id) %>% summarize(mean_weight = round(mean(WeightKg), 1), height = mean(height), BMI_ = round(mean(BMI))) %>% 
  arrange(-BMI_)

print(c_weight)


##########################COMBINED HOUR#######################

# Total 35 person's mean values by hourly activites. 
combined_hour <- left_join(c_hourly_cal, c_hourly_intensity, by = c("Id", "ActivityHour")) %>% 
  mutate(ActivityHour = format(ActivityHour, "%H:%M")) %>%
  mutate(ActivityHour = factor(ActivityHour, levels = unique(ActivityHour))) %>% 
  group_by(ActivityHour) %>%
  summarize(mean_calories = round(mean(Calories, na.rm = TRUE)), 
                                mean_intensity = round(mean(TotalIntensity, na.rm = TRUE))) %>% 
  pivot_longer(cols = c(mean_calories, mean_intensity), names_to = "Type", values_to = "Value")

ggplot(combined_hour) + geom_line(aes(x = ActivityHour, y = Value, group = Type, colour = Type)) +
  scale_colour_manual(values = c("purple", "#69b3a2")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank()) +
  labs(title = "Hourly Average Calories and Intensity Levels")

# HOURLY CALORIES

c_hourly_cal <- bind_rows(read_csv("hourlyCalories_merged_1.csv", show_col_types = FALSE), 
                          read_csv("hourlyCalories_merged_2.csv", show_col_types = FALSE)) %>% 
  drop_na() %>% distinct() %>% mutate(ActivityHour = mdy_hms(ActivityHour)) %>% 
  arrange(Id, ActivityHour)

# HOURLY INTENSITIES

c_hourly_intensity <- bind_rows(read_csv("hourlyIntensities_merged_1.csv", show_col_types = FALSE),
                                read_csv("hourlyIntensities_merged_2.csv", show_col_types = FALSE)) %>% 
  drop_na() %>% distinct() %>% mutate(ActivityHour = mdy_hms(ActivityHour)) %>% 
  arrange(Id, ActivityHour)