library(Rmisc)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(tableHTML)

setwd("../data_folder")

# create function to bind results

results_bind <- function(extenction) {
  
  file_names <- list.files(pattern = paste0("*.", extenction))
  if (extenction == "csv") {
    frames <- lapply(file_names, read.csv, na.strings = c("NA", ""))
  } else if (extenction == "tsv") {
    frames <- lapply(file_names, read.table, sep = '\t', 
                     header = TRUE, na.strings = c("NA", ""))
  }
  frames_binded <- do.call("rbind", frames)
  final_frame <- as_data_frame(frames_binded)
  return(final_frame)
}

raw_frame <-  results_bind("csv")

# Check the frame

typeof(raw_frame)
str(raw_frame)
View(raw_frame)

# Work with NAs and types (Итого я сделал факторами пол и сезон)

raw_frame$Medal[is.na(raw_frame$Medal)] <- "participant"

filt_frame <- filter(raw_frame, !is.na(Name),
                     !is.na(Age),
                     !is.na(Sex),
                     Sex != "G", 
                     !is.na(Team), 
                     !is.na(NOC), 
                     !is.na(Games), 
                     !is.na(Year), 
                     !is.na(Season),
                     !is.na(City),
                     !is.na(Sport), 
                     !is.na(Event)) %>% 
  mutate( Sex = as.factor(Sex),
          Season = as.factor(Season), 
          Medal = as.factor(Medal)) 

HW_filt_frame <- filter(filt_frame, !is.na(Height), !is.na(Weight))

# Задание 3

age_uvenile_by_sex_1992 <- filter(filt_frame, Games %in% c("1992 Summer", "1992 Winter"))  %>% 
  group_by(Sex) %>% summarise(min(Age))
View(age_uvenile_by_sex_1992)

# Задание 4

Height_stat_by_sex <- group_by(HW_filt_frame, Sex) %>% summarise(mean(Height), sd(Height))
View(Height_stat_by_sex)

# Задание 5

Height_stat_F_Tennis_2000 <- filter(HW_filt_frame, Sex == "F",
                      Games %in% c("2000 Summer", "2000 Winter"),
                      Sport == "Tennis") %>% 
  summarise(Mean = round(mean(Height), digits = 1), SD = round(sd(Height), digits = 1)) 
View(Height_stat_F_Tennis_2000)

# Задание 6

Sport_most_weight_2006_M <- filter(HW_filt_frame, Games %in% c("2006 Summer", "2006 Winter"),
                                   Sex == "M") %>% filter(Weight == max(Weight))
print(Sport_most_weight_2006_M$Sport)

# Задание 7

F_Gold_Medal_1980.2010_count <- nrow(filter(filt_frame, Sex == "F", Year %in% c(1980:2010), 
                                       Medal == "Gold"))
print(F_Gold_Medal_1980.2010_count)

# Задание 8

John_Aalberg_count <- nrow(filter(filt_frame, Name  == "John Aalberg"))
print(John_Aalberg_count)

# Задание 9

number_AgeGroups_2008 <- c(nrow(filter(filt_frame, Age %in% 15:24, Year == 2008)),
                           nrow(filter(filt_frame, Age %in% 25:34, Year == 2008)),
                           nrow(filter(filt_frame, Age %in% 35:44, Year == 2008)),
                           nrow(filter(filt_frame, Age %in% 45:55, Year == 2008)))
names(number_AgeGroups_2008) <- c("[15-25)", "[25-35)", "[35-45)", "[45-55]")
print(number_AgeGroups_2008)
print(min(number_AgeGroups_2008))
print(max(number_AgeGroups_2008))

# Задание 10

sport_variety_1994 <- filter(filt_frame, Year == 1994) %>% 
  summarise(sport_variety_1994 = unique(Sport))
sport_variety_2002 <- filter(filt_frame, Year == 2002) %>% 
  summarise(sport_variety_2002 = unique(Sport))

Diff_sport_variety_1994_2002 <- nrow(sport_variety_2002) - nrow(sport_variety_1994)
print(Diff_sport_variety_1994_2002)

# Задание 11

Summer_Gold <- filter(filt_frame, Season == "Summer", Medal == "Gold") %>% group_by(NOC) %>% 
  summarise(Gold_Summer_nubmer = length(Medal)) %>% arrange(desc(Gold_Summer_nubmer)) %>% slice(1:3)

Summer_Silver <- filter(filt_frame, Season == "Summer", Medal == "Silver") %>% group_by(NOC) %>% 
  summarise(Silver_Summer_nubmer = length(Medal)) %>% arrange(desc(Silver_Summer_nubmer)) %>% slice(1:3)

Summer_Broze <- filter(filt_frame, Season == "Summer", Medal == "Bronze") %>% group_by(NOC) %>% 
  summarise(Bronse_Summer_nubmer = length(Medal)) %>% arrange(desc(Bronse_Summer_nubmer)) %>% slice(1:3)

Winter_Gold <- filter(filt_frame, Season == "Winter", Medal == "Gold") %>% group_by(NOC) %>% 
  summarise(Gold_Winter_nubmer = length(Medal)) %>% arrange(desc(Gold_Winter_nubmer)) %>% slice(1:3)

Winter_Silver <- filter(filt_frame, Season == "Winter", Medal == "Silver") %>% group_by(NOC) %>% 
  summarise(Silver_Winter_nubmer = length(Medal)) %>% arrange(desc(Silver_Winter_nubmer)) %>% slice(1:3)

Winter_Bronse <- filter(filt_frame, Season == "Winter", Medal == "Bronze") %>% group_by(NOC) %>% 
  summarise(Broze_Winter_nubmer = length(Medal)) %>% arrange(desc(Broze_Winter_nubmer)) %>% slice(1:3)

Top3_MedalDiff_SeasonDiff = list(Summer_Gold,
                                 Summer_Silver, 
                                 Summer_Broze,
                                 Winter_Gold, 
                                 Winter_Silver,
                                 Winter_Bronse)
print(Top3_MedalDiff_SeasonDiff)

# Задание 12

HW_filt_Zst_frame <- mutate(HW_filt_frame,
                           Height_z_scores = (Height - mean(Height))/sd(Height))
View(HW_filt_Zst_frame)

# Задание 13

HW_filt_MMst_frame <- mutate(HW_filt_frame,
    Height_min_max_scaled = (Height - min(Height))/(max(Height) - min(Height)))
View(HW_filt_MMst_frame)

# Задание 14

Winter_HW <- filter(HW_filt_frame, Season == "Winter" )
Winter <-  filter(filt_frame, Season == "Winter")

# Работаю с весом (ниже)

wilcox.test(Winter_HW$Weight[Winter_HW$Sex == "M"],
            Winter_HW$Weight[Winter_HW$Sex == "F"])

ggplot(Winter_HW, aes(x = Sex, y = Weight))+
  geom_boxplot()+
  stat_compare_means(method = "wilcox.test", comparisons = list(c("F", "M")))+
  labs(title = "Competitors Weight on Winter Olympic Games",
       caption = "wilcoxon test performed")

# Работаю с ростом (ниже)

wilcox.test(Winter_HW$Height[Winter_HW$Sex == "M"],
            Winter_HW$Height[Winter_HW$Sex == "F"])

ggplot(Winter_HW, aes(x = Sex, y = Height))+
  geom_boxplot()+
  stat_compare_means(method = "wilcox.test", comparisons = list(c("F", "M")))+
  labs(title = "Competitors Height on Winter Olympic Games",
       caption = "wilcoxon test performed")

# Работаю с возрастом (ниже)

wilcox.test(Winter$Age[Winter$Sex == "M"],
            Winter$Age[Winter$Sex == "F"])

ggplot(Winter, aes(x = Sex, y = Age))+
  geom_boxplot()+
  stat_compare_means(method = "wilcox.test", comparisons = list(c("F", "M")))+
  labs(title = "Competitors Age on Winter Olympic Games", 
       caption = "wilcoxon test performed" )


# Задание 15

Medals <- filter(filt_frame, Medal != "participant")
Medals$Medal <- 1
Tab_Medals <- table(Medals$Team, Medals$Medal)
chisq.test(Tab_Medals)

# Задание 16

sport_with_min_max_mean_age <- group_by(filt_frame, Sport) %>% 
  summarise(mean_Age = mean(Age)) %>%
  filter(mean_Age %in% c(max(mean_Age), min(mean_Age)))

global_mean_age <- mean(filt_frame$Age)

Roque <- filter(filt_frame, Sport == "Roque")
nrow(Roque)

Rhythmic_Gymnastics <- filter(filt_frame, Sport == "Rhythmic Gymnastics")
nrow(Rhythmic_Gymnastics)

Rhythmic_Gymnastics_unique <- distinct(Rhythmic_Gymnastics, Name, .keep_all = T)

t.test(Rhythmic_Gymnastics_unique$Age, mu = global_mean_age, 
       alternative = "two.sided")




