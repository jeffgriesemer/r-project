library(tidyverse)
library(dplyr)
library(lubridate)

# visual
library(ggplot2)
library(ggrepel)
library(scales)
library(themes)
#install.packages("googlesheets")
library(googlesheets)

rm(list=ls())

df_stats <- gs_title("golf_stats")

df_2018_drive <- df_stats %>% gs_read(ws = "2018 driving accur")
df_2018 <- df_2018_drive %>% select(RANK=RANK_2018,PLAYER_NAME,ROUNDS,FAIRWAY_PCT) %>% filter(PLAYER_NAME == 'Tiger Woods') %>% 
  mutate(YEARMO='2018-12', CATEGORY='Driving Accuracy')

df_tiger_2019 <- df_stats %>% gs_read(ws = "2019 driving accr")
df_tiger_2019_03 <- df_tiger_2019 %>% filter(PLAYER_NAME == 'Tiger Woods') %>% select(RANK=RANK_LAST_WEEK_2019,PLAYER_NAME,ROUNDS,FAIRWAY_PCT) %>% 
  mutate(YEARMO='2019-03', CATEGORY='Driving Accuracy')
df_tiger_2019_04 <- df_tiger_2019 %>% filter(PLAYER_NAME == 'Tiger Woods') %>% select(RANK=RANK_NOW,PLAYER_NAME,ROUNDS,FAIRWAY_PCT) %>% 
  mutate(YEARMO='2019-04', CATEGORY='Driving Accuracy')

df_tiger_drives <- rbind(df_2018,df_tiger_2019_03)
df_tiger_drives <- rbind(df_tiger_drives, df_tiger_2019_04)

df_tiger_drives

#####  gir
df_2018_gir <- df_stats %>% gs_read(ws = "2018 gir")
df_tiger_2018_gir <- df_2018_gir %>% filter(PLAYER_NAME == 'Tiger Woods') %>% 
  select(RANK=RANK_LAST_WEEK,PLAYER_NAME,ROUNDS,PCT_HIT) %>% 
  mutate(YEARMO='2018-12', CATEGORY='GIR')

df_2019_gir <- df_stats %>% gs_read(ws = "2019 gir")

df_tiger_2019_03_gir <- df_2019_gir %>% filter(PLAYER_NAME == 'Tiger Woods') %>% 
  select(RANK=RANK_LAST_WEEK,PLAYER_NAME,ROUNDS,PCT_HIT) %>% 
  mutate(YEARMO='2019-03', CATEGORY='GIR')

df_tiger_2019_04_gir <- df_2019_gir %>% filter(PLAYER_NAME == 'Tiger Woods') %>% 
  select(RANK=RANK_THIS_WEEK,PLAYER_NAME,ROUNDS,PCT_HIT) %>% 
  mutate(YEARMO='2019-04', CATEGORY='GIR')

df_tiger_gir <- rbind(df_tiger_2018_gir, df_tiger_2019_03_gir)
df_tiger_gir <- rbind(df_tiger_gir, df_tiger_2019_04_gir)

df_tiger_gir

### putting ###

df_2018_gir <- df_stats %>% gs_read(ws = "2018 gir")
df_tiger_2018_gir <- df_2018_gir %>% filter(PLAYER_NAME == 'Tiger Woods') %>% 
  select(RANK=RANK_LAST_WEEK,PLAYER_NAME,ROUNDS,PCT_HIT) %>% 
  mutate(YEARMO='2018-12', CATEGORY='GIR')

###############

df_2019 <- df_2019_drive %>% filter(RANK_NOW <= 130) %>% mutate(tiger_flag= ifelse(PLAYER_NAME == 'Tiger Woods', T, F)) %>% arrange(RANK_NOW)
df_2019$PLAYER_NAME <- as.factor(df_2019$PLAYER_NAME)
fct_reorder()

ggplot() + geom_point(data=df_2019, aes(x=reorder(PLAYER_NAME,RANK_NOW), y=RANK_NOW, color=tiger_flag)) +
  geom_point(data=df_2018, aes(x=PLAYER_NAME, y=RANK_2018, color=tiger_flag)) +coord_flip()


df_2018 <- read.table("clipboard", header = T, sep = "\t",comment="&", fill = T)
names(df$X.) <- "HIT PCT"
df_mrg <- merge(df,df_2018, by="PLAYER.NAME", all.x=T)
df_mrg_final <- df_mrg %>% select(PLAYER.NAME, RANK_NOW = RANK.THIS.WEEK.x, PCT_NOW = X..x)
df_2019 <- df %>% select(1:7)


ggplot( df_mrg, aes(x = PLAYER.NAME, y = X., color=)) +
  geom_point(size = 2, alpha = .6) +
  geom_smooth(size = 1.5, color = "darkgrey") +
  scale_y_continuous(label = scales::dollar, limits = c(50000, 250000)) +
  scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 60)) +
  labs(x = "x", y = "y", title = "title", subtitle = "subtitle") +
  #theme_minimal() +
  geom_text_repel(aes(label= ))





### espn stats

library(ggrepel)
library(ggplot2)
library(dplyr)
library(viridis)

rm(list=ls())

library(tidyverse)
library(googlesheets)
df_stats <- gs_title("golf_stats_espn")
df_2019 <- df_stats %>% gs_read(ws = "2019_stats")
df_2019$DRVE_TOTAL <- NULL


# data wrangling - cleanup

str(df_2019)

describe(df_2019$AGE)

df_2019_filtered <- df_2019 %>% 
  mutate(AGE_numeric = !(is.na(as.numeric(AGE)))) %>% 
  filter(AGE_numeric == TRUE) %>% 
  mutate(AGE = as.numeric(AGE))

df_2019_filtered <- rename(df_2019_filtered, "RANK_DRV_ACC" = "RK")


# EDA

library(DataExplorer)
#introduce(df_2019_filtered)
plot_intro(df_2019_filtered)
plot_missing(df_2019)
plot_bar(df_2019)

plot_histogram(df_2019_filtered)
plot_boxplot(df_2019_filtered, by = "YDS_DRIVE")
plot_boxplot(df_2019_filtered %>% select(-1), by = "AGE")
#plot_boxplot(df_2019, by = "PLAYER")

plot_correlation(na.omit(df_2019), maxcat = 5L)
plot_correlation(na.omit(df_2019), type = "c")
plot_correlation(na.omit(df_2019), type = "d")

library(funModeling) 
library(Hmisc)

eda_func <- function(data)
{
  glimpse(df_2019)
  df_status(df_2019)
  freq(df_2019)
  profiling_num(df_2019)
  plot_num(data)
  describe(df_2019$AGE)
}

eda_func(df)

sink("file")
describe(df)
sink()


# cor(df_2019)
# error - must be numeric

library(ggcorrplot)

corr <- cor(df_2019_filtered %>% select(-1,-2))

ggcorrplot(corr, type = "lower", outline.col = "black",
           lab=TRUE,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

# visualize as a circle indicating significance (circle size) and coorelation

ggcorrplot(corr, type = "lower", outline.col = "black",
           method="circle",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


######## create ntile

df_2019_filtered <- df_2019_filtered %>%  mutate(RNK_GIR = ntile(GREENS_REG, 10),
                                                 RNK_PUT = ntile(-PUTT_AVG,10),
                                                 RNK_SAVE = ntile(SAVE_PCT,10),
                                                 RNK_AGE = ntile(AGE,10),
                                                 RNK_YDS = ntile(YDS_DRIVE,10),
                                                 RNK_ACC = ntile(DRIVING_ACC,10))

#df_2019_filtered <- df_2019_filtered %>% mutate(RNK_INDEX = RNK_GIR + RNK_PUT + RNK_SAVE + RNK_AGE + RNK_YDS + RNK_ACC)
#df_2019_filtered <- df_2019_filtered %>% mutate(RNK_INDEX_GRP = ntile(RNK_INDEX, 10))

library(ggrepel)

#ggplot(df_2019_filtered %>% filter(RNK_INDEX_GRP > 5), aes(x=PLAYER, y=RNK_INDEX_GRP,color=as.factor(RNK_INDEX_GRP))) + 
#  geom_point() + 
#  coord_flip() +
#  geom_label_repel(aes(label=PLAYER))


df_rnk <- gather(df_2019_filtered, "VARIABLE", "RANK", 10:15)
df_rnk_tiger <- df_rnk %>% filter(grepl("Tiger",PLAYER))

ggplot(df_rnk_tiger, aes(x=VARIABLE, y=RANK)) + geom_point()

##### outliers

df_2019_filtered %>% filter(AGE > 50)
df_2019_filtered %>% filter(GREENS_REG > 73)

### regression line - age vs driving distance

ggplot(df_2019_filtered, aes(x=AGE, y=YDS_DRIVE)) + 
  geom_point(size=3, color="forest green")


ggplot(df_2019_filtered, aes(x=AGE, y=YDS_DRIVE)) + 
  geom_point(size=3, color="forest green") +
  geom_smooth(method = "lm") 

ggplot(df_2019_filtered, aes(x=AGE, y=YDS_DRIVE)) + 
  geom_point(size=3, color="forest green") +
  geom_smooth(method = "loess") 


ggplot(df_2019_filtered, aes(x=DRIVING_ACC, y=YDS_DRIVE, color=AGE)) + 
  geom_point(size=3) +
  geom_smooth(method = "lm") 

library(viridis)

ggplot(df_2019_filtered, aes(x=DRIVING_ACC, y=YDS_DRIVE, color=AGE)) + 
  geom_point(size=3) +
  geom_smooth(method = "lm") +
  scale_color_viridis()


library(ggrepel)

ggplot(df_2019_filtered, aes(x=DRIVING_ACC, y=YDS_DRIVE, color=AGE)) + 
  geom_point(size=4, alpha=.8) +
  geom_smooth(method = "lm") +
  scale_color_viridis() +
  scale_y_continuous(limits=c(250,325)) +
  geom_label_repel(data=df_2019_filtered %>% filter((YDS_DRIVE >= 305 | YDS_DRIVE < 275) | DRIVING_ACC > 70),
                   aes(label=PLAYER), size=3.5) +
  labs(title="PGA statistics - 2019", subtitle = "Yards Per Drive vs. Driving Accuracy by Age",
       caption="Stats from http://www.espn.com/golf/statistics",
       x="Driving Accuracy",y="Yards Per Drive")


################################################################

df_2019$YDS_RND <- round(df_2019$YDS_DRIVE, -1)
df_2019$YDS_DECILE <- cut(df_2019$YDS_RND, 10, labels=c('10','20','30','40','50','60','70','80','90','100'))
df_2019$RNK_YDS <- rank(-df_2019$YDS_DRIVE)
df_2019$RNK_ACC <- rank(df_2019$DRIVING_ACC)
df_2019$RNK_GIR <- rank(-df_2019$GREENS_REG)
df_2019$RNK_PUT <- rank(df_2019$PUTT_AVG)

ggplot(df_2019, aes(x=YDS_DRIVE, y=PLAYER, fill=YDS_RND)) + 
  geom_tile(size=3) +
  geom_tile(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(color="tiger")) +
  geom_text_repel(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(label=paste(PLAYER, ' / Rank #', RNK_YDS, sep=''), color="tiger"),size=4) +
  labs(title="Tiger Woods - 2019", subtitle = "Driving Distance", ylab="") +
  scale_fill_viridis(discrete = F, option="D", guide=guide_legend(title="Driving Yards"))

ggplot(df_2019, aes(x=DRIVING_ACC, y=PLAYER, fill=DRIVING_ACC)) + 
  geom_tile(size=3) +
  geom_tile(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(color="tiger")) +
  geom_text_repel(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(label=paste(PLAYER, ' / Rank #',RNK_ACC,sep=''), color="tiger"),size=4) +
  labs(title="Tiger Woods - 2019", subtitle = "Driving Accuracy", ylab="") +
  scale_fill_viridis(discrete = F, option="D", guide=guide_legend(title="Driving Accuracy"))

ggplot(df_2019, aes(x=GREENS_REG, y=PLAYER, fill=GREENS_REG)) + 
  geom_tile(size=3) +
  geom_tile(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(color="tiger")) +
  geom_text_repel(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(label=PLAYER, color="tiger"),size=4) +
  labs(title="Tiger Woods - 2019", subtitle = "Greens in Regulation", ylab="") +
  scale_fill_viridis(discrete = F, option="D", guide=guide_legend(title="Greens in Reg"))

ggplot(df_2019, aes(x=PUTT_AVG, y=PLAYER, fill=PUTT_AVG)) + 
  geom_tile(size=3) +
  geom_tile(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'), aes(color="tiger")) +
  #geom_text_repel(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(label=paste(PLAYER, ' / Rank #',RNK_PUT,sep=''), color="tiger"),size=4) +
  labs(title="Tiger Woods - 2019", subtitle = "Putting Average", ylab="") +
  scale_fill_viridis(discrete = F, option="D", guide=guide_legend(title="Putting"))



ggplot(df_2019, aes(x=reorder(PLAYER,DRIVING_ACC), y=DRIVING_ACC, fill=YDS_DRIVE)) + 
  geom_bar(stat="identity",size=3) +
  geom_bar(stat="identity",data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(color="tiger")) + 
  geom_text_repel(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(label=paste(PLAYER, ' / Rank #',RNK_ACC,sep=''), color="tiger"),size=4) +
  labs(title="Tiger Woods - 2019", subtitle = "Driving Accuracy", ylab="") +
  scale_fill_viridis(discrete = F, option="D", guide=guide_legend(title="Driving Distance"))


ggplot(df_2019, aes(x=reorder(PLAYER,DRIVING_ACC), y=DRIVING_ACC, color=YDS_DRIVE)) + 
  geom_point(size=4) + 
  geom_point(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(color="tiger")) + coord_flip() +
  geom_text_repel(data=df_2019 %>% filter(PLAYER == 'Tiger Woods'),aes(label=paste(PLAYER, ' / Rank #', RNK_ACC,sep=''), color="tiger"),size=4) +
  labs(title="Tiger Woods - 2019", subtitle = "Driving Accuracy", ylab="") +
  scale_color_viridis(discrete = F, option="D", guide=guide_legend(title="Driving Distance"))

df_2019

ggplot(df_2019, aes(x=YDS_DRIVE, y=PLAYER, fill=RNK_YDS, label=PLAYER)) + geom_point() +
  #  gghighlight(PLAYER == 'Tiger Woods',  label_key = PLAYER) 
  geom_text_repel(data=subset(df_2019, RNK_YDS = 107), aes(x=YDS_DRIVE, y=PLAYER))
