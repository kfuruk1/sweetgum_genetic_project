library(tidyverse)
library(emmeans)
library(nlme)
library(multcomp)

# Step 1 get the data loaded
data_raw <- read.csv("https://raw.githubusercontent.com/azd169/sweetgum_genetic_project/main/phys_data.csv",
                     header = T)

data <- data_raw %>%
  rename(Location = loc,
         Family = geno,
         Time = period,
         Row = row,
         Tree = tree,
         ID = tree_id,
         GLD = gld,
         Height = ht,
         PSI = psi,
         Moisture = moisture,
         Obs = obs,
         Photo = photo,
         LiCOR = li.cor,
         Cond = cond,
         Trans = trans) %>%
  mutate(Location = recode(Location,
                           "1" = "LA Tech",
                           "2" = "Hill Farm"),
         Family = recode(Family,
                         "1" = "AGHS3",
                         "2" = "AGHS4",
                         "3" = "AGH2",
                         "4" = "AGH25",
                         "5" = "AGHS2",
                         "6" = "AGHS1"),
         Time = recode(Time,
                       "1" = "Early Summer",
                       "2" = "Mid Summer",
                       "3" = "Late Summer"),
         Time = fct_relevel(Time,
                            "Early Summer", "Mid Summer", "Late Summer"),
         IDD = as.factor(paste(Time, ID, sep = "_"))) %>%
  dplyr::select(-c("l.r.t"))

data_wp <- data %>%
filter(Time != "Early Summer")

mod_wp <- lme(PSI ~ Time * Location * Family, 
           random = ~ 1| IDD, 
           correlation = corAR1(form = ~ 1| IDD),
           data = data_wp, 
           method = "REML") 

summary(mod_wp)
anova(mod_wp)

mod_wp_time <- emmeans(mod_wp, list(pairwise ~ Time), adjust = "tukey")
contrast_wp_time <- contrast(mod_wp_time, adjust = "tukey")
cld_wp_time <- cld(contrast_wp_time, Letters = letters, reversed = T, adjust = "tukey") 
print(cld_wp_time)

means_wp_time <- data_wp %>%
  group_by(Time) %>%
  summarize(`Mean PSI` = mean(PSI),
            `SE PSI` = sd(PSI) / sqrt(n()),
            N = n())
