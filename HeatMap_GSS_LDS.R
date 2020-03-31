library(foreign)
library(ggplot2)
library(survey)

setwd("/Users/stephencranney/Desktop/hold/GSS_spss")

GSS<-read.spss('GSS7218_R2.sav', to.data.frame = TRUE)

GSS$weekattend<- ifelse(GSS$ATTEND=="EVERY WEEK" | GSS$ATTEND=="NRLY EVERY WEEK" | GSS$ATTEND=="MORE THN ONCE WK", 1, 0)

GSS$AGE_1<- GSS$AGE
GSS$AGE_1<- as.character(GSS$AGE_1)
GSS$AGE_1[GSS$AGE_1== '89 OR OLDER']<- '89'
GSS$AGE_1=as.numeric(GSS$AGE_1)
GSS$AGE_2<-cut(GSS$AGE_1, c(0,24,29,34,39,44,49,54,59,64,69,74,200))
GSS$AGE_3<-cut(GSS$AGE_1, c(0,24,34,44,54,64,74,200))

GSS$COHORT1<-as.numeric(as.character(GSS$COHORT))

GSS$COHORT_2<-  cut(GSS$COHORT1, c(1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970,
                                  1980, 1990))

GSS$COHORT_N<-  cut(GSS$COHORT1, c(1900, 1928, 1946, 1965,
                                   1980, 1990))

cohortlabels<-c("1881-1890","1891-1900","1901-1910","1911-1920", "1921-1930", "1931-1940", "1941-1950",
                "1951-1960","1961-1970","1971-1980", "1981-1990")

cohortlabels2<-c("Greatest Generation", "Silent Generation", "Baby Boomers", "Generation X", "Millenials")

GSS$YEAR_2<-cut(GSS$YEAR, c(1971,1976,1983,1988,1994,2004,2014, 2019))

yearlabels<-c("1972-1976","1977-1983","1984-1988","1989-1994","1995-2004","2005-2014","2015-2018")

agelabels<-c("<25","25-29","30-34","35-39","40-44","45-49","50-54",
             "55-59","60-64","65-69","70-74","75+")

agelabels2<-c("18-25","25-34","35-44","45-54",
              "55-64","65-74","75+")

GSS$WEIGHT<- as.character(GSS$WTSSALL)
GSS$WEIGHT<- as.numeric(GSS$WEIGHT)

GSS$active_LDS<- ifelse(GSS$OTHER=="Mormon" & (GSS$ATTEND=="EVERY WEEK" | GSS$ATTEND=="NRLY EVERY WEEK" | GSS$ATTEND=="MORE THN ONCE WK"), 1, 0)
GSS$inactive_LDS<- ifelse(GSS$OTHER=="Mormon" & (GSS$ATTEND=="2-3X A MONTH" | GSS$ATTEND=="ONCE A MONTH" | GSS$ATTEND=="SEVRL TIMES A YR" | GSS$ATTEND=="ONCE A YEAR"| GSS$ATTEND=="LT ONCE A YEAR" | GSS$ATTEND=="NEVER"), 1, 0)

LDS.design<-svydesign(ids = ~1, data = GSS_LDS, weights = GSS_LDS$WEIGHT)

LDS_1<-as.data.frame(svytable(~YEAR_2 + active_LDS, LDS.design))

LDS2=LDS_1 %>% group_by(YEAR_2) %>% mutate(PERC = (Freq/sum(Freq) * 100))
LDS2$PERC= round(LDS2$PERC, 0)

LDS2 <- subset(LDS2, active_LDS == 1)

p<-ggplot(data=LDS2, aes(x=YEAR_2, y=PERC, group=1)) +
  geom_line()+
  geom_point()

p + labs(title = "US Latter-day Saints by Activity, 1972-2018",
         caption = "Source: General Social Survey, 1972-2018", 
         x = "Years", y = "% Active (attends about once a week)") + theme(plot.caption = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(labels= c(yearlabels)) + scale_color_discrete(labels=c(cohortlabels))
ggsave('LDSACtive_GSS.PNG')