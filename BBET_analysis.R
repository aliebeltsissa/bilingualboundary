# SETUP --------------------------------------------------------------
# clear workspace
rm(list=ls());
ls();

# load required libraries
library(paletteer);
library(extrafont);
library(showtext);
library(dplyr);
library(ggplot2);
library(plotrix);
library(scales);
library(lmerTest);
library(emmeans);
library(tidyr);
library(MuMIn);
library(effects);

# colours
cols <- paletteer::scale_colour_paletteer_d("colorBlindness::SteppedSequential5Steps");
simple_col <- "#1a8de5";
complex_col <- "#859a2f";
green_col <- "#2b725a";
  
par(family="CMU Serif");
font_add("CMU Serif", "C:/Users/annal/Downloads/cmu/cmunrm.ttf");
showtext_auto();

setwd("C:/Users/annal/Documents/GitHub/bilingualboundary/data");


# READING IN DATA --------------------------------------------------
all_ET <- read.csv("all_ET.csv",header=T,sep=",");
all_scores <- read.csv("all_scores.csv",header=T,sep=",");

all_ET <- subset(all_ET, select = -c(X,post_target,type,TextBlock,adjusted_seq,fixations,rec_id,msgs,noshorts_seq));

all_ET$morph_type <- factor(all_ET$morph_type, levels=c("simple", "complex"));
all_ET$morph_type <- relevel(all_ET$morph_type,"simple");
all_ET$lst_type <- factor(all_ET$lst_type);
all_ET$target <- factor(all_ET$target);
all_ET$preview <- factor(all_ET$preview);
all_ET$trial_type <- factor(all_ET$trial_type, levels=c("identical", "cognate", "legal_nonword", "illegal_nonword"));
all_ET$trial_issue <- factor(all_ET$trial_issue);
all_ET$trial_issue[all_ET$trial_issue=='OUT-OF-BOUNDS'] <- 'N';
all_ET$morph_type <- factor(all_ET$morph_type);
all_ET$morph_type <- relevel(all_ET$morph_type,"simple");
summary(all_ET);

all_scores <- subset(all_scores, select = -c(X));
all_scores$sbj_ID <- as.factor(all_scores$sbj_ID);
all_scores$gender <- as.factor(all_scores$gender);
summary(all_scores);

## Participant exclusions --------------------------------------------------
# exclude participants with comprehension score lower than 2.5 SD from mean
summary(all_scores$comp_score);
# min=80.00, Q1=93.09, med=95.00, mean=95.08, Q3=97.50, max=100

# NAs are for the trials with skipped N-1 words or target words
all_data <- merge(all_ET, all_scores,by="sbj_ID");

word_data <- read.csv("C:/Users/annal/Documents/Me/SISSA/BBET/BBET_design/full_word_data.csv",header=T,sep=",");
word_data$morph_type <- factor(word_data$morph_type);
word_data$morph_type <- relevel(word_data$morph_type,"simple");
word_data$target <- as.factor(word_data$target);
word_data$cognate <- as.factor(word_data$cognate);
word_data$legal_nonword <- as.factor(word_data$legal_nonword);
word_data$Illegal_nonword <- as.factor(word_data$Illegal_nonword);
summary(word_data);

all_data <- merge(subset(all_data,select=-c(morph_type)),word_data,by="target");

ggplot(all_scores, aes(x=comp_score)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=1,colour="black") +
  geom_vline(aes(xintercept=mean(comp_score)),colour="red",linewidth=1) +
  geom_vline(aes(xintercept=mean(comp_score)+2.5*sd(comp_score)),colour="blue",
             linetype="dashed",linewidth=1) +
  geom_vline(aes(xintercept=mean(comp_score)-2.5*sd(comp_score)),colour="blue",
             linetype="dashed",linewidth=1) +
  labs(x="Comprehension check score", y="Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

mean(all_scores$comp_score)-2.5*sd(all_scores$comp_score); # min=84.19
mean(all_scores$comp_score); # mean=95.08
mean(all_scores$comp_score)+2.5*sd(all_scores$comp_score); # max=100

for (i in all_scores$sbj_ID) {
  if (all_scores[all_scores$sbj_ID==i,]$comp_score < mean(all_scores$comp_score)-2.5*sd(all_scores$comp_score)) {
    print(paste("Participant ",i," had a comprehension check score below 2.5*SD."))
  }
}


# ET metrics histograms
all_data_long <- gather(all_data, key="ETmeasure", value="count", c('GD_all','FFD_all','FoM_all','GPD_all'));
all_data_long_sbj58 <- all_data_long[all_data_long$sbj_ID=='58',];

ggplot(all_data_long[all_data_long$trial_issue=='N',], aes(x=count)) +
  geom_histogram(colour='black',binwidth=100) +
  facet_wrap(~ETmeasure, scales='free') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

clean_data_long <- gather(all_data, key="ETmeasure", value="count", c('GD_cleanedfixs','FFD_cleanedfixs','FoM_cleanedfixs','GPD_cleanedfixs'));

ggplot(clean_data_long[clean_data_long$trial_issue=='N',], aes(x=count)) +
  geom_histogram(colour='black',binwidth=100) +
  facet_wrap(~ETmeasure, scales='free') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

# boxplots of data
mean_data <- all_data[all_data$trial_issue=='N',] %>%
  group_by(sbj_ID) %>%
  summarise_at(vars(GD_all,FFD_all,FoM_all,GPD_all),
               list(Mean = mean),na.rm=TRUE)

outliers_GPD <- mean_data %>%
  group_by() %>%  # no grouping variable, but keeps dplyr happy
  mutate(
    Q1 = quantile(GPD_all_Mean, 0.25, na.rm = TRUE),
    Q3 = quantile(GPD_all_Mean, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  filter(GPD_all_Mean < Q1 - 1.5 * IQR | GPD_all_Mean > Q3 + 1.5 * IQR);

ggplot(mean_data, aes(y=GD_all_Mean)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

ggplot(mean_data, aes(x=1,y=GPD_all_Mean)) +
  geom_boxplot(outlier.shape = NA) +  # hide default outliers
  geom_point(data=outliers_GPD,colour="red",size=3) +
  geom_text(data=outliers_GPD,aes(label=sbj_ID),hjust=-0.3,size=8,family="CMU Serif") +
  theme(
    axis.text.x=element_blank(),axis.ticks.x=element_blank(),
    axis.title.x=element_blank(),panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),panel.background=element_blank(), 
    axis.line=element_line(colour="black"),
    text=element_text(family="CMU Serif",size=35, colour = "black"),
    legend.background = element_rect(fill = NA));
# mean(GPD_all) = 523ms
# mean(GPD_all) for sbj 58 = 998ms

ggplot(mean_data, aes(y=FFD_all_Mean)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

ggplot(mean_data, aes(y=FoM_all_Mean)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

# EXCLUDE PARTICIPANT 58

## Dataframes ----
all_scoresclean <- all_scores[!all_scores$sbj_ID %in% c('58'),];
all_ETclean <- all_ET[!all_ET$sbj_ID %in% c('58'),];
all_data_clean <- merge(all_ETclean, all_scoresclean,by="sbj_ID");
all_data_clean <- merge(subset(all_data_clean,select=-c(morph_type)),word_data,by="target");
# now n=49

# participants who possibly saw some previews
preview_participants <- c('15','16','25','31','38','42','45','53');
all_ET_nopreviews <- all_ETclean[!all_ETclean$sbj_ID %in% preview_participants,];
all_data_nopreviews <- all_data_clean[!all_data_clean$sbj_ID %in% preview_participants,];
length(unique(all_data_nopreviews$sbj_ID)); # 41 participants total

# participants who possibly saw some previews in at least 20% of trials
preview20_participants <- c('25','31','38','42','45','53');
all_ET_no20previews <- all_ETclean[!all_ETclean$sbj_ID %in% preview20_participants,];
all_data_no20previews <- all_data_clean[!all_data_clean$sbj_ID %in% preview20_participants,];
length(unique(all_data_no20previews$sbj_ID)); # 43 participants total

## Log-transforming data ----
all_data_no20previews$FFD_log <- log(all_data_no20previews$FFD_cleanedfixs);
all_data_no20previews$FoM_log <- log(all_data_no20previews$FoM_cleanedfixs);
all_data_no20previews$GD_log <- log(all_data_no20previews$GD_cleanedfixs);
all_data_no20previews$GPD_log <- log(all_data_no20previews$GPD_cleanedfixs);

plot(sort(all_data_no20previews$FFD_log));
plot(sort(all_data_no20previews$FoM_log));
plot(sort(all_data_no20previews$GD_log));
plot(sort(all_data_no20previews$GPD_log));

## FFDs < 800ms ----
# exclude all FFD points if over 800ms
all_data_no20previews$FFD_noover800 <- all_data_no20previews$FFD_all;
all_data_no20previews$FFD_noover800[all_data_no20previews$FFD_noover800>800] <- NA;

## Skip rates ----
all_data_no20previews$skip <- 0;
all_data_no20previews$skip[all_data_no20previews$trial_issue=='T_SKIP'] <- 1;

summary(all_data_no20previews$trial_issue);
prop.table(table(all_data_no20previews$trial_issue));
# 5179 (94.66%) trials normal
# 245 (4.48%) trials skipped N-1
# 47 (0.86%) trials skipped target

summary(all_data_no20previews$trial_type[all_data_no20previews$trial_issue=='T_SKIP']);
prop.table(table(all_data_no20previews$trial_type[all_data_no20previews$trial_issue=='T_SKIP']));
# identical skipped 27.66% of time
# cognate skipped 28.79% of time
# legal nonword skipped 21.28% of time
# illegal nonword skipped 21.28% of time

summary(all_data_no20previews$target_length[all_data_no20previews$trial_issue=='N']);
summary(all_data_no20previews$target_length[all_data_no20previews$trial_issue=='T_SKIP']);
# virtually identical

summary(all_data_no20previews$target_zipf[all_data_no20previews$trial_issue=='N']);
summary(all_data_no20previews$target_zipf[all_data_no20previews$trial_issue=='T_SKIP']);
# virtually identical

# contrast coding trial_type
backward_diff <- matrix(c(-3/4, 1/4, 1/4, 1/4, -1/2, -1/2, 1/2, 1/2, 
                            -1/4, -1/4, -1/4, 3/4), ncol = 3);
all_data_no20previews$trial_type_cod <- all_data_no20previews$trial_type;
contrasts(all_data_no20previews$trial_type_cod) <- backward_diff;

prereg_data <- subset(all_data_no20previews, trial_issue == "N");



# ET measures visualisation ----
ggplot(prereg_data, aes(x=FFD_log)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=0.05,colour="black") +
  geom_vline(aes(xintercept=mean(FFD_log,na.rm=TRUE)),colour="red",linewidth=1) +
  geom_vline(aes(xintercept=mean(FFD_log,na.rm=TRUE)+2.5*sd(FFD_log,na.rm=TRUE)),colour="blue",
             linetype="dashed",linewidth=1) +
  geom_vline(aes(xintercept=mean(FFD_log,na.rm=TRUE)-2.5*sd(FFD_log,na.rm=TRUE)),colour="blue",
             linetype="dashed",linewidth=1) +
  labs(x="log FFD (ms)", y="Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

ggplot(prereg_data, aes(x=FoM_log)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=0.05,colour="black") +
  geom_vline(aes(xintercept=mean(FoM_log,na.rm=TRUE)),colour="red",linewidth=1) +
  geom_vline(aes(xintercept=mean(FoM_log,na.rm=TRUE)+2.5*sd(FoM_log,na.rm=TRUE)),colour="blue",
             linetype="dashed",linewidth=1) +
  geom_vline(aes(xintercept=mean(FoM_log,na.rm=TRUE)-2.5*sd(FoM_log,na.rm=TRUE)),colour="blue",
             linetype="dashed",linewidth=1) +
  labs(x="log FoM (ms)", y="Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

ggplot(prereg_data, aes(x=GD_log)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=0.05,colour="black") +
  geom_vline(aes(xintercept=mean(GD_log,na.rm=TRUE)),colour="red",linewidth=1) +
  geom_vline(aes(xintercept=mean(GD_log,na.rm=TRUE)+2.5*sd(GD_log,na.rm=TRUE)),colour="blue",
             linetype="dashed",linewidth=1) +
  geom_vline(aes(xintercept=mean(GD_log,na.rm=TRUE)-2.5*sd(GD_log,na.rm=TRUE)),colour="blue",
             linetype="dashed",linewidth=1) +
  labs(x="log GD (ms)", y="Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

ggplot(prereg_data, aes(x=GPD_log)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=0.05,colour="black") +
  geom_vline(aes(xintercept=mean(GPD_log,na.rm=TRUE)),colour="red",linewidth=1) +
  geom_vline(aes(xintercept=mean(GPD_log,na.rm=TRUE)+2.5*sd(GPD_log,na.rm=TRUE)),colour="blue",
             linetype="dashed",linewidth=1) +
  geom_vline(aes(xintercept=mean(GPD_log,na.rm=TRUE)-2.5*sd(GPD_log,na.rm=TRUE)),colour="blue",
             linetype="dashed",linewidth=1) +
  labs(x="log GPD (ms)", y="Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour="black"),
        text = element_text(family="CMU Serif",size=35,colour="black"),
        legend.background=element_rect(fill=NA));

## ET SD trimming ----
prereg_data$FFD_trimmed <- prereg_data$FFD_cleanedfixs;
prereg_data$FFD_trimmed[prereg_data$FFD_trimmed < 
                                    mean(prereg_data$FFD_cleanedfixs,na.rm=TRUE)-
                                    2.5*sd(prereg_data$FFD_cleanedfixs,na.rm=TRUE)] <- NA;
prereg_data$FFD_trimmed <- prereg_data$FFD_cleanedfixs;
prereg_data$FFD_trimmed[prereg_data$FFD_trimmed > 
                                    mean(prereg_data$FFD_cleanedfixs,na.rm=TRUE)+
                                    2.5*sd(prereg_data$FFD_cleanedfixs,na.rm=TRUE)] <- NA;

prereg_data$FoM_trimmed <- prereg_data$FoM_cleanedfixs;
prereg_data$FoM_trimmed[prereg_data$FoM_trimmed < 
                                    mean(prereg_data$FoM_cleanedfixs,na.rm=TRUE)-
                                    2.5*sd(prereg_data$FoM_cleanedfixs,na.rm=TRUE)] <- NA;
prereg_data$FoM_trimmed <- prereg_data$FoM_cleanedfixs;
prereg_data$FoM_trimmed[prereg_data$FoM_trimmed > 
                                    mean(prereg_data$FoM_cleanedfixs,na.rm=TRUE)+
                                    2.5*sd(prereg_data$FoM_cleanedfixs,na.rm=TRUE)] <- NA;

prereg_data$GD_trimmed <- prereg_data$GD_cleanedfixs;
prereg_data$GD_trimmed[prereg_data$GD_trimmed < 
                                    mean(prereg_data$GD_cleanedfixs,na.rm=TRUE)-
                                    2.5*sd(prereg_data$GD_cleanedfixs,na.rm=TRUE)] <- NA;
prereg_data$GD_trimmed <- prereg_data$GPD_cleanedfixs;
prereg_data$GD_trimmed[prereg_data$GD_trimmed > 
                                    mean(prereg_data$GD_cleanedfixs,na.rm=TRUE)+
                                    2.5*sd(prereg_data$GD_cleanedfixs,na.rm=TRUE)] <- NA;

prereg_data$GPD_trimmed <- prereg_data$GPD_cleanedfixs;
prereg_data$GPD_trimmed[prereg_data$GD_trimmed < 
                                   mean(prereg_data$GPD_cleanedfixs,na.rm=TRUE)-
                                   2.5*sd(prereg_data$GPD_cleanedfixs,na.rm=TRUE)] <- NA;
prereg_data$GPD_trimmed <- prereg_data$GPD_cleanedfixs;
prereg_data$GPD_trimmed[prereg_data$GPD_trimmed > 
                                   mean(prereg_data$GPD_cleanedfixs,na.rm=TRUE)+
                                   2.5*sd(prereg_data$GPD_cleanedfixs,na.rm=TRUE)] <- NA;

## log ET SD trimming ----
prereg_data$FFD_logtrimmed <- prereg_data$FFD_log;
prereg_data$FFD_logtrimmed[prereg_data$FFD_logtrimmed < 
                             mean(prereg_data$FFD_log,na.rm=TRUE)-
                             2.5*sd(prereg_data$FFD_log,na.rm=TRUE)] <- NA;
prereg_data$FFD_logtrimmed <- prereg_data$FFD_log;
prereg_data$FFD_logtrimmed[prereg_data$FFD_logtrimmed > 
                             mean(prereg_data$FFD_log,na.rm=TRUE)+
                             2.5*sd(prereg_data$FFD_log,na.rm=TRUE)] <- NA;

prereg_data$FoM_logtrimmed <- prereg_data$FoM_log;
prereg_data$FoM_logtrimmed[prereg_data$FoM_logtrimmed < 
                             mean(prereg_data$FoM_log,na.rm=TRUE)-
                             2.5*sd(prereg_data$FoM_log,na.rm=TRUE)] <- NA;
prereg_data$FoM_logtrimmed <- prereg_data$FoM_log;
prereg_data$FoM_logtrimmed[prereg_data$FoM_logtrimmed > 
                             mean(prereg_data$FoM_log,na.rm=TRUE)+
                             2.5*sd(prereg_data$FoM_log,na.rm=TRUE)] <- NA;

prereg_data$GD_logtrimmed <- prereg_data$GD_log;
prereg_data$GD_logtrimmed[prereg_data$GD_logtrimmed < 
                            mean(prereg_data$GD_log,na.rm=TRUE)-
                            2.5*sd(prereg_data$GD_log,na.rm=TRUE)] <- NA;
prereg_data$GD_logtrimmed <- prereg_data$GD_log;
prereg_data$GD_logtrimmed[prereg_data$GD_log > 
                            mean(prereg_data$GD_log,na.rm=TRUE)+
                            2.5*sd(prereg_data$GD_log,na.rm=TRUE)] <- NA;

prereg_data$GPD_logtrimmed <- prereg_data$GPD_log;
prereg_data$GPD_logtrimmed[prereg_data$GD_logtrimmed < 
                             mean(prereg_data$GPD_log,na.rm=TRUE)-
                             2.5*sd(prereg_data$GPD_log,na.rm=TRUE)] <- NA;
prereg_data$GPD_logtrimmed <- prereg_data$GPD_log;
prereg_data$GPD_logtrimmed[prereg_data$GPD_logtrimmed > 
                             mean(prereg_data$GPD_log,na.rm=TRUE)+
                             2.5*sd(prereg_data$GPD_log,na.rm=TRUE)] <- NA;





# Correlation of all gaze metrics ----------------------------------------
cor(all_ETclean[all_ETclean$trial_issue=="N",
                c("GD_all","FFD_all","GPD_all","FoM_all")],
    use='pairwise.complete.obs'); # all data

cor(all_ETclean[all_ETclean$trial_issue=="N",
                c("GD_all","FFD_all","GPD_all","FoM_all")],
    use='pairwise.complete.obs'); # all data

cor(all_ETclean[all_ETclean$trial_issue=="N",
                c("GD_noshortfixs","FFD_noshortfixs","GPD_noshortfixs","FoM_noshortfixs")],
    use='pairwise.complete.obs'); # no short fixs

cor(all_ETclean[all_ETclean$trial_issue=="N",
                c("GD_nolongfixs","FFD_nolongfixs","GPD_nolongfixs","FoM_nolongfixs")],
    use='pairwise.complete.obs'); # no long fixs

cor(all_ETclean[all_ETclean$trial_issue=="N",
                c("GD_cleanedfixs","FFD_cleanedfixs","GPD_cleanedfixs","FoM_cleanedfixs")],
    use='pairwise.complete.obs'); # cleaned data (no short or long fixs)

cor(all_ET_nopreviews[all_ET_nopreviews$trial_issue=="N",
                c("GD_cleanedfixs","FFD_cleanedfixs","GPD_cleanedfixs","FoM_cleanedfixs")],
    use='pairwise.complete.obs'); # without participants having seen previews

cor(all_ET_no20previews[all_ET_no20previews$trial_issue=="N",
                      c("GD_cleanedfixs","FFD_cleanedfixs","GPD_cleanedfixs","FoM_cleanedfixs")],
    use='pairwise.complete.obs'); # without participants having seen 20% of previews

cor(all_data_prereg[all_data_prereg$trial_issue=="N",
                      c("GD_cleanedfixs","FFD_cleanedfixs","GPD_cleanedfixs","FoM_cleanedfixs")],
    use='pairwise.complete.obs'); # without participants having seen previews





# Plotting -------------------------------------------------------------
setwd("C:/Users/annal/Documents/Me/SISSA/BBET/BBET_analysis/full_analysis");

# FFD
FFD_plt <- ggplot(all_data_nopreviews[all_data_nopreviews$trial_issue=='N',], aes(x=trial_type, y=FFD_cleanedfixs, colour=morph_type, group=morph_type)) + 
  scale_colour_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "FFD (ms)", colour = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 30, colour = "black"),
        text=element_text(family="CMU Serif",size=40),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(FFD_plt, filename = "FFD_bycondition.png", bg = "transparent", width=7,height=6);

#FoM
FoM_plt <- ggplot(all_data_nopreviews[all_data_nopreviews$trial_issue=='N',], aes(x=trial_type, y=FoM_cleanedfixs, colour=morph_type, group=morph_type)) + 
  scale_colour_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "FoM (ms)", colour = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 30, colour = "black"),
        text=element_text(family="CMU Serif",size=40),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(FoM_plt, filename = "FoM_bycondition.png", bg = "transparent", width=7,height=6);
# normal to have warning "removed rows": by the nature of FoM, we have an NA if
# there was only 1 fixation for that trial, leading to removed rows

# GD
GD_plt <- ggplot(all_data_nopreviews[all_data_nopreviews$trial_issue=='N',], aes(x=trial_type, y=GD_cleanedfixs, colour=morph_type, group=morph_type)) + 
  scale_colour_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "GD (ms)", colour = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 30, colour = "black"),
        text=element_text(family="CMU Serif",size=40),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(GD_plt, filename = "GD_bycondition.png", bg = "transparent", width=7,height=6);

# GPD
GPD_plt <- ggplot(all_data_nopreviews[all_data_nopreviews$trial_issue=='N',], aes(x=trial_type, y=GPD_cleanedfixs, colour=morph_type, group=morph_type)) + 
  scale_colour_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "GPD (ms)", colour = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 30, colour = "black"),
        text=element_text(family="CMU Serif",size=40),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(GPD_plt, filename = "GPD_bycondition.png", bg = "transparent", width=7,height=6);




# LMERS ------------------------------------------------------------------
# contrast coding
prereg_data <- prereg_data %>% 
  mutate(morph_coded = case_when(morph_type == 'simple' ~ -0.5,
                                 morph_type == 'complex' ~ 0.5));

emm_options(lmerTest.limit = 12150, pbkrtest.limit = 12150,
            lmer.df="satterthwaite");


## PRE-REGISTRATION -----
lm_FFD_prereg <- lmer(FFD_cleanedfixs ~ scale(trial_id) + 
                              target_length + target_zipf + 
                              morph_score + lextale_score + changes_seen + 
                              morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                            data=prereg_data);
summary(emmeans(lm_FFD_prereg, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_FFD_prereg, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_prereg);
fitted_values <- fitted(lm_FFD_prereg);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_prereg); # R2m = 0.05, R2c = 0.15

lm_FoM_prereg <- lmer(FoM_cleanedfixs ~ scale(trial_id) + 
                              target_length + target_zipf + 
                              morph_score + lextale_score + changes_seen + 
                              morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                            data=prereg_data);
summary(emmeans(lm_FoM_prereg, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_FoM_prereg, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_prereg);
fitted_values <- fitted(lm_FoM_prereg);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_prereg); # R2m = 0.05, R2c = 0.14

lm_GD_prereg <- lmer(GD_cleanedfixs ~ scale(trial_id) + 
                             target_length + target_zipf +
                             morph_score + lextale_score + changes_seen + 
                             morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                           data=prereg_data);
summary(emmeans(lm_GD_prereg, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_GD_prereg, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_prereg);
fitted_values <- fitted(lm_GD_prereg);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_prereg); # R2m = 0.14, R2c = 0.34

lm_GPD_prereg <- lmer(GPD_cleanedfixs ~ scale(trial_id) + 
                              target_length + target_zipf + 
                              morph_score + lextale_score + changes_seen + 
                              morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                            data=prereg_data);
summary(emmeans(lm_GPD_prereg, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_GPD_prereg, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_prereg);
fitted_values <- fitted(lm_GPD_prereg);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # slightly heteroscedastic?
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # still some heavy tailing at upper bound
# R2
r.squaredGLMM(lm_GPD_prereg); # R2m = 0.10, R2c = 0.23

lm_skip_prereg <- lme4::glmer(skip ~ scale(trial_id) + 
                        target_length + target_zipf + 
                        morph_score + lextale_score + changes_seen + 
                        morph_type*trial_type + (1|sbj_ID) + (1|target), 
                      data=all_data_no20previews[all_data_no20previews$trial_issue=='N'|all_data_no20previews$trial_issue=='T_SKIP',],
                      family='binomial');
# Error: Response is constant





## no participants having seen previews ----
lm_FFD_nopreviews <- lmer(FFD_cleanedfixs ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_nopreviews[all_data_nopreviews$trial_issue=='N',]);
summary(emmeans(lm_FFD_nopreviews, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FFD_nopreviews, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_nopreviews);
fitted_values <- fitted(lm_FFD_nopreviews);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_nopreviews); # R2m = 0.05, R2c = 0.16

lm_FoM_nopreviews <- lmer(FoM_cleanedfixs ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_nopreviews[all_data_nopreviews$trial_issue=='N',]);
summary(emmeans(lm_FoM_nopreviews, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FoM_nopreviews, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_nopreviews);
fitted_values <- fitted(lm_FoM_nopreviews);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_nopreviews); # R2m = 0.05, R2c = 0.17

lm_GD_nopreviews <- lmer(GD_cleanedfixs ~ scale(trial_id) + 
                           target_length + target_zipf +
                           morph_score + lextale_score + changes_seen + 
                           morph_type*trial_type + (1|sbj_ID) + (1|target), 
                         data=all_data_nopreviews[all_data_nopreviews$trial_issue=='N',]);
summary(emmeans(lm_GD_nopreviews, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GD_nopreviews, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_nopreviews);
fitted_values <- fitted(lm_GD_nopreviews);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_nopreviews); # R2m = 0.14, R2c = 0.35

lm_GPD_nopreviews <- lmer(GPD_cleanedfixs ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_nopreviews[all_data_nopreviews$trial_issue=='N',]);
summary(emmeans(lm_GPD_nopreviews, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GPD_nopreviews, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_nopreviews);
fitted_values <- fitted(lm_GPD_nopreviews);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # slightly heteroscedastic?
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # still some heavy tailing at upper bound
# R2
r.squaredGLMM(lm_GPD_nopreviews); # R2m = 0.10, R2c = 0.23


## Log-transformed data ----
lm_FFD_log <- lmer(FFD_log ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                   data=prereg_data);
summary(emmeans(lm_FFD_log, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_FFD_log, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_log);
fitted_values <- fitted(lm_FFD_log);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_log); # R2m = 0.05, R2c = 0.15

lm_FoM_log <- lmer(FoM_log ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                   data=prereg_data);
summary(emmeans(lm_FoM_log, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_FoM_log, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_log);
fitted_values <- fitted(lm_FoM_log);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_log); # R2m = 0.05, R2c = 0.14

lm_GD_log <- lmer(GD_log ~ scale(trial_id) + 
                    target_length + target_zipf +
                    morph_score + lextale_score + changes_seen + 
                    morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                  data=prereg_data);
summary(emmeans(lm_GD_log, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_GD_log, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_log);
fitted_values <- fitted(lm_GD_log);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_log); # R2m = 0.14, R2c = 0.34

lm_GPD_log <- lmer(GPD_log ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                   data=prereg_data);
summary(emmeans(lm_GPD_log, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_GPD_log, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_log);
fitted_values <- fitted(lm_GPD_log);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # slightly heteroscedastic?
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # still some heavy tailing at upper bound
# R2
r.squaredGLMM(lm_GPD_log); # R2m = 0.10, R2c = 0.23


## Trimmed Log-transformed data ----
lm_FFD_logtrimmed <- lmer(FFD_logtrimmed ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                   data=prereg_data);
summary(emmeans(lm_FFD_logtrimmed, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_FFD_logtrimmed, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_logtrimmed);
fitted_values <- fitted(lm_FFD_logtrimmed);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_logtrimmed); # R2m = 0.05, R2c = 0.15

lm_FoM_logtrimmed <- lmer(FoM_logtrimmed ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                   data=prereg_data);
summary(emmeans(lm_FoM_logtrimmed, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_FoM_logtrimmed, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_logtrimmed);
fitted_values <- fitted(lm_FoM_logtrimmed);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_logtrimmed); # R2m = 0.05, R2c = 0.14

lm_GD_logtrimmed <- lmer(GD_logtrimmed ~ scale(trial_id) + 
                    target_length + target_zipf +
                    morph_score + lextale_score + changes_seen + 
                    morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                  data=prereg_data);
summary(emmeans(lm_GD_logtrimmed, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_GD_logtrimmed, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_logtrimmed);
fitted_values <- fitted(lm_GD_logtrimmed);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_logtrimmed); # R2m = 0.14, R2c = 0.34

lm_GPD_logtrimmed <- lmer(GPD_logtrimmed ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                   data=prereg_data);
summary(emmeans(lm_GPD_logtrimmed, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_GPD_logtrimmed, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_logtrimmed);
fitted_values <- fitted(lm_GPD_logtrimmed);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # slightly heteroscedastic?
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # still some heavy tailing at upper bound
# R2
r.squaredGLMM(lm_GPD_logtrimmed); # R2m = 0.10, R2c = 0.23



## Trimmed Log-transformed data ----
lm_FFD_logtrimmed2 <- lmer(FFD_logtrimmed ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                          data=prereg_data, 
                          subset=abs(scale(resid(lm_FFD_logtrimmed))) < 2.5);
summary(emmeans(lm_FFD_logtrimmed2, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_FFD_logtrimmed2, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_logtrimmed2);
fitted_values <- fitted(lm_FFD_logtrimmed2);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_logtrimmed2); # R2m = 0.05, R2c = 0.15

lm_FoM_logtrimmed2 <- lmer(FoM_logtrimmed ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                          data=prereg_data,
                          subset=abs(scale(resid(lm_FoM_logtrimmed))) < 2.5);
summary(emmeans(lm_FoM_logtrimmed2, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_FoM_logtrimmed2, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_logtrimmed2);
fitted_values <- fitted(lm_FoM_logtrimmed2);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_logtrimmed2); # R2m = 0.05, R2c = 0.14

lm_GD_logtrimmed2 <- lmer(GD_logtrimmed ~ scale(trial_id) + 
                           target_length + target_zipf +
                           morph_score + lextale_score + changes_seen + 
                           morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                         data=prereg_data,
                         subset=abs(scale(resid(lm_GD_logtrimmed))) < 2.5);
summary(emmeans(lm_GD_logtrimmed2, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_GD_logtrimmed2, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_logtrimmed2);
fitted_values <- fitted(lm_GD_logtrimmed2);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_logtrimmed2); # R2m = 0.14, R2c = 0.34

lm_GPD_logtrimmed2 <- lmer(GPD_logtrimmed ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_coded*trial_type_cod + (1|sbj_ID) + (1|target), 
                          data=prereg_data,
                          subset=abs(scale(resid(lm_GPD_logtrimmed))) < 2.5);
summary(emmeans(lm_GPD_logtrimmed2, pairwise~morph_coded|trial_type_cod), adjust="bonferroni");
summary(emmeans(lm_GPD_logtrimmed2, pairwise~trial_type_cod|morph_coded), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_logtrimmed2);
fitted_values <- fitted(lm_GPD_logtrimmed2);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # slightly heteroscedastic?
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # still some heavy tailing at upper bound
# R2
r.squaredGLMM(lm_GPD_logtrimmed2); # R2m = 0.10, R2c = 0.23
















## Pre-reg excluding outliers -----
lm_FFD_trimmed <- lmer(FFD_trimmed ~ scale(trial_id) + 
                        target_length + target_zipf + 
                        morph_score + lextale_score + changes_seen + 
                        morph_type*trial_type + (1|sbj_ID) + (1|target), 
                      data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_FFD_trimmed, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FFD_trimmed, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_trimmed);
fitted_values <- fitted(lm_FFD_trimmed);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_trimmed); # R2m = 0.05, R2c = 0.15

lm_FoM_trimmed <- lmer(FoM_trimmed ~ scale(trial_id) + 
                        target_length + target_zipf + 
                        morph_score + lextale_score + changes_seen + 
                        morph_type*trial_type + (1|sbj_ID) + (1|target), 
                      data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_FoM_trimmed, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FoM_trimmed, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_trimmed);
fitted_values <- fitted(lm_FoM_trimmed);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_trimmed); # R2m = 0.05, R2c = 0.14

lm_GD_trimmed <- lmer(GD_trimmed ~ scale(trial_id) + 
                       target_length + target_zipf +
                       morph_score + lextale_score + changes_seen + 
                       morph_type*trial_type + (1|sbj_ID) + (1|target), 
                     data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_GD_trimmed, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GD_trimmed, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_trimmed);
fitted_values <- fitted(lm_GD_trimmed);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_trimmed); # R2m = 0.14, R2c = 0.34

lm_GPD_trimmed <- lmer(GPD_trimmed ~ scale(trial_id) + 
                        target_length + target_zipf + 
                        morph_score + lextale_score + changes_seen + 
                        morph_type*trial_type + (1|sbj_ID) + (1|target), 
                      data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_GPD_trimmed, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GPD_trimmed, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_trimmed);
fitted_values <- fitted(lm_GPD_trimmed);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # slightly heteroscedastic?
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # still some heavy tailing at upper bound
# R2
r.squaredGLMM(lm_GPD_trimmed); # R2m = 0.10, R2c = 0.23


## all participants (except sbj 58)
lm_FFD_cleanedfixs <- lmer(FFD_cleanedfixs ~ scale(trial_id) + 
                             target_length + target_zipf + 
                             morph_score + lextale_score + changes_seen + 
                             morph_type*trial_type + (1|sbj_ID) + (1|target), 
                           data=all_data_clean[all_data_clean$trial_issue=='N',]);
summary(emmeans(lm_FFD_cleanedfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FFD_cleanedfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_cleanedfixs);
fitted_values <- fitted(lm_FFD_cleanedfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_cleanedfixs); # R2m = 0.04, R2c = 0.18

lm_FoM_cleanedfixs <- lmer(FoM_cleanedfixs ~ scale(trial_id) + 
                             target_length + target_zipf + 
                             morph_score + lextale_score + changes_seen + 
                             morph_type*trial_type + (1|sbj_ID) + (1|target), 
                           data=all_data_clean[all_data_clean$trial_issue=='N',]);
summary(emmeans(lm_FoM_cleanedfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FoM_cleanedfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_cleanedfixs);
fitted_values <- fitted(lm_FoM_cleanedfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_cleanedfixs); # R2m = 0.04, R2c = 0.16

lm_GD_cleanedfixs <- lmer(GD_cleanedfixs ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_clean[all_data_clean$trial_issue=='N',]);
summary(emmeans(lm_GD_cleanedfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GD_cleanedfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_cleanedfixs);
fitted_values <- fitted(lm_GD_cleanedfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_cleanedfixs); # R2m = 0.12, R2c = 0.34

lm_GPD_cleanedfixs <- lmer(GPD_cleanedfixs ~ scale(trial_id) + 
                             target_length + target_zipf + 
                             morph_score + lextale_score + changes_seen + 
                             morph_type*trial_type + (1|sbj_ID) + (1|target), 
                           data=all_data_clean[all_data_clean$trial_issue=='N',]);
summary(emmeans(lm_GPD_cleanedfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GPD_cleanedfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_cleanedfixs);
fitted_values <- fitted(lm_GPD_cleanedfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # unbalanced Y axis
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # heavy-tailed at upper bound
# R2
r.squaredGLMM(lm_GPD_cleanedfixs); # R2m = 0.09, R2c = 0.23



## without FFDs over 800ms -----
lm_FFD_nolongFFDs <- lmer(FFD_noover800 ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_FFD_nolongFFDs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FFD_nolongFFDs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_nolongFFDs);
fitted_values <- fitted(lm_FFD_nolongFFDs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_nolongFFDs); # R2m = 0.04, R2c = 0.17



## without long fixations ----
lm_FFD_nolongfixs <- lmer(FFD_nolongfixs ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_FFD_nolongfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FFD_nolongfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_nolongfixs);
fitted_values <- fitted(lm_FFD_nolongfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_nolongfixs); # R2m = 0.04, R2c = 0.17

lm_FoM_nolongfixs <- lmer(FoM_nolongfixs ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_FoM_nolongfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FoM_nolongfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_nolongfixs);
fitted_values <- fitted(lm_FoM_nolongfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_nolongfixs); # R2m = 0.04, R2c = 0.15

lm_GD_nolongfixs <- lmer(GD_nolongfixs ~ scale(trial_id) + 
                           target_length + target_zipf + 
                           morph_score + lextale_score + changes_seen + 
                           morph_type*trial_type + (1|sbj_ID) + (1|target), 
                         data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_GD_nolongfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GD_nolongfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_nolongfixs);
fitted_values <- fitted(lm_GD_nolongfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_nolongfixs); # R2m = 0.12, R2c = 0.32

lm_GPD_nolongfixs <- lmer(GPD_nolongfixs ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_GPD_nolongfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GPD_nolongfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_nolongfixs);
fitted_values <- fitted(lm_GPD_nolongfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # unbalanced Y axis
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # heavy-tailed at higher bound
# R2
r.squaredGLMM(lm_GPD_nolongfixs); # R2m = 0.10, R2c = 0.26


## without short fixations ----
lm_FFD_noshortfixs <- lmer(FFD_noshortfixs ~ scale(trial_id) + 
                             target_length + target_zipf + 
                             morph_score + lextale_score + changes_seen + 
                             morph_type*trial_type + (1|sbj_ID) + (1|target), 
                           data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_FFD_noshortfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FFD_noshortfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_noshortfixs);
fitted_values <- fitted(lm_FFD_noshortfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_noshortfixs); # R2m = 0.04, R2c = 0.17

lm_FoM_noshortfixs <- lmer(FoM_noshortfixs ~ scale(trial_id) + 
                             target_length + target_zipf + 
                             morph_score + lextale_score + changes_seen + 
                             morph_type*trial_type + (1|sbj_ID) + (1|target), 
                           data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_FoM_noshortfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FoM_noshortfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_noshortfixs);
fitted_values <- fitted(lm_FoM_noshortfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_noshortfixs); # R2m = 0.03, R2c = 0.16

lm_GD_noshortfixs <- lmer(GD_noshortfixs ~ scale(trial_id) + 
                            target_length + target_zipf + 
                            morph_score + lextale_score + changes_seen + 
                            morph_type*trial_type + (1|sbj_ID) + (1|target), 
                          data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_GD_noshortfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GD_noshortfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_noshortfixs);
fitted_values <- fitted(lm_GD_noshortfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_noshortfixs); # R2m = 0.11, R2c = 0.32

lm_GPD_noshortfixs <- lmer(GPD_noshortfixs ~ scale(trial_id) + 
                             target_length + target_zipf + 
                             morph_score + lextale_score + changes_seen + 
                             morph_type*trial_type + (1|sbj_ID) + (1|target), 
                           data=all_data_no20previews[all_data_no20previews$trial_issue=='N',]);
summary(emmeans(lm_GPD_noshortfixs, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GPD_noshortfixs, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_noshortfixs);
fitted_values <- fitted(lm_GPD_noshortfixs);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # unbalanced Y axis
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # heavy-tailed at upper bound
# R2
r.squaredGLMM(lm_GPD_noshortfixs); # R2m = 0.07, R2c = 0.18



## all data -----
lm_FFD_all <- lmer(FFD_all ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_type*trial_type + (1|sbj_ID) + (1|target), 
                   data=all_data[all_data$trial_issue=='N',]);
summary(emmeans(lm_FFD_all, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FFD_all, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FFD_all);
fitted_values <- fitted(lm_FFD_all);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FFD_all); # R2m = 0.04, R2c = 0.17

lm_FoM_all <- lmer(FoM_all ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_type*trial_type + (1|sbj_ID) + (1|target), 
                   data=all_data[all_data$trial_issue=='N',]);
summary(emmeans(lm_FoM_all, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_FoM_all, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_FoM_all);
fitted_values <- fitted(lm_FoM_all);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_FoM_all); # R2m = 0.03, R2c = 0.15

lm_GD_all <- lmer(GD_all ~ scale(trial_id) + 
                    target_length + target_zipf + 
                    morph_score + lextale_score + changes_seen + 
                    morph_type*trial_type + (1|sbj_ID) + (1|target), 
                  data=all_data[all_data$trial_issue=='N',]);
summary(emmeans(lm_GD_all, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GD_all, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GD_all);
fitted_values <- fitted(lm_GD_all);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # pretty good, some outliers at higher end
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # curve at upper end
# R2
r.squaredGLMM(lm_GD_all); # R2m = 0.11, R2c = 0.32

lm_GPD_all <- lmer(GPD_all ~ scale(trial_id) + 
                     target_length + target_zipf + 
                     morph_score + lextale_score + changes_seen + 
                     morph_type*trial_type + (1|sbj_ID) + (1|target), 
                   data=all_data[all_data$trial_issue=='N',]);
summary(emmeans(lm_GPD_all, pairwise~morph_type|trial_type), adjust="bonferroni");
summary(emmeans(lm_GPD_all, pairwise~trial_type|morph_type), adjust="bonferroni");
# Residuals & fitted values
residuals <- residuals(lm_GPD_all);
fitted_values <- fitted(lm_GPD_all);
plot(fitted_values, residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals");
abline(h = 0, col = "red", lty = 2); # unbalanced Y axis
# QQ Plot
qqnorm(residuals);
qqline(residuals, col = "red"); # heavy tail at upper bound
# R2
r.squaredGLMM(lm_GPD_all); # R2m = 0.07, R2c = 0.19






# Effects plots --------------------------------------------------------------
## Pre-registration ----
ef_lm_FFD_prereg <- data.frame(effect(c("morph_type*trial_type"),lm_FFD_prereg));
ggplot(ef_lm_FFD_prereg, aes(x=trial_type,y=fit,group=morph_type)) +
  geom_line(aes(colour=morph_type,linetype=morph_type),linewidth=1,position=position_dodge(width=0.2)) +
  geom_point(aes(colour=morph_type,shape=morph_type),size=3,position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=morph_type),width=0.2,position=position_dodge(width=0.2)) +
  
  labs(x="Preview type",y="FFD (ms)") +
  scale_x_discrete(labels=c("Identical","Cognate","Legal nonword","Illegal nonword")) +
  scale_linetype_manual(values=c("dashed", "solid"),name="Morphological type") +
  scale_colour_manual(values=c("#1a8de5", "#859a2f"),name="Morphological type") +
  scale_shape_manual(values=c(16,17),name="Morphological type") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 20, colour = "black"),
        text=element_text(family="CMU Serif",size=20),
        legend.position=c(0.2,0.8),legend.background=element_rect(fill=NA));

ef_lm_FoM_prereg <- data.frame(effect(c("morph_type*trial_type"),lm_FoM_prereg));
ggplot(ef_lm_FoM_prereg, aes(x=trial_type,y=fit,group=morph_type)) +
  geom_line(aes(colour=morph_type,linetype=morph_type),linewidth=1,position=position_dodge(width=0.2)) +
  geom_point(aes(colour=morph_type,shape=morph_type),size=3,position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=morph_type),width=0.2,position=position_dodge(width=0.2)) +
  
  labs(x="Preview type",y="FoM (ms)") +
  scale_x_discrete(labels=c("Identical","Cognate","Legal nonword","Illegal nonword")) +
  scale_linetype_manual(values=c("dashed", "solid"),name="Morphological type") +
  scale_colour_manual(values=c("#1a8de5", "#859a2f"),name="Morphological type") +
  scale_shape_manual(values=c(16,17),name="Morphological type") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 20, colour = "black"),
        text=element_text(family="CMU Serif",size=20),
        legend.position=c(0.2,0.8),legend.background=element_rect(fill=NA));

ef_lm_GD_prereg <- data.frame(effect(c("morph_type*trial_type"),lm_GD_prereg));
ggplot(ef_lm_GD_prereg, aes(x=trial_type,y=fit,group=morph_type)) +
  geom_line(aes(colour=morph_type,linetype=morph_type),linewidth=1,position=position_dodge(width=0.2)) +
  geom_point(aes(colour=morph_type,shape=morph_type),size=3,position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=morph_type),width=0.2,position=position_dodge(width=0.2)) +
  
  labs(x="Preview type",y="GD (ms)") +
  scale_x_discrete(labels=c("Identical","Cognate","Legal nonword","Illegal nonword")) +
  scale_linetype_manual(values=c("dashed", "solid"),name="Morphological type") +
  scale_colour_manual(values=c("#1a8de5", "#859a2f"),name="Morphological type") +
  scale_shape_manual(values=c(16,17),name="Morphological type") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 20, colour = "black"),
        text=element_text(family="CMU Serif",size=20),
        legend.position=c(0.2,0.8),legend.background=element_rect(fill=NA));

ef_lm_GPD_prereg <- data.frame(effect(c("morph_type*trial_type"),lm_GPD_prereg));
ggplot(ef_lm_GPD_prereg, aes(x=trial_type,y=fit,group=morph_type)) +
  geom_line(aes(colour=morph_type,linetype=morph_type),linewidth=1,position=position_dodge(width=0.2)) +
  geom_point(aes(colour=morph_type,shape=morph_type),size=3,position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, color=morph_type),width=0.2,position=position_dodge(width=0.2)) +
  
  labs(x="Preview type",y="GPD (ms)") +
  scale_x_discrete(labels=c("Identical","Cognate","Legal nonword","Illegal nonword")) +
  scale_linetype_manual(values=c("dashed", "solid"),name="Morphological type") +
  scale_colour_manual(values=c("#1a8de5", "#859a2f"),name="Morphological type") +
  scale_shape_manual(values=c(16,17),name="Morphological type") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 20, colour = "black"),
        text=element_text(family="CMU Serif",size=20),
        legend.position=c(0.2,0.8),legend.background=element_rect(fill=NA));


## Pre-registration (log-transformed) ----
ef_lm_FFD_log <- effect(c("morph_coded*trial_type_cod"),lm_FFD_log);
ef_lm_FFD_log <- data.frame(ef_lm_FFD_log);
ef_lm_FFD_log <- ef_lm_FFD_log[ef_lm_FFD_log$morph_coded==-0.5|ef_lm_FFD_log$morph_coded==0.5,];
ef_lm_FFD_log$morph_type <- ef_lm_FFD_log$morph_coded;
ef_lm_FFD_log$morph_type[ef_lm_FFD_log$morph_type==-0.5] <- "simple";
ef_lm_FFD_log$morph_type[ef_lm_FFD_log$morph_type==0.5] <- "complex";
ef_lm_FFD_log$morph_type <- factor(ef_lm_FFD_log$morph_type, levels=c("simple","complex"));

FFD_log_plt <- ggplot(ef_lm_FFD_log, aes(y=exp(fit), x=trial_type_cod, group=morph_type)) +
  geom_line(aes(color=morph_type, linetype=morph_type), linewidth=1, position=position_dodge(width=0.2)) +
  geom_point(aes(color=morph_type, shape=morph_type), size=3, position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=exp(lower), ymax=exp(upper), color=morph_type), width=0.2, position=position_dodge(width=0.2)) +
  
  labs(x="Preview type",y="FFD (ms)") +
  scale_x_discrete(labels=c("Identical","Cognate","Legal nonword","Illegal nonword")) +
  scale_linetype_manual(values=c("dashed", "solid"),name="Morphological type") +
  scale_colour_manual(values=c("#859a2f","#1a8de5"),name="Morphological type") +
  scale_shape_manual(values=c(16,17),name="Morphological type") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 60, colour = "black"),
        text=element_text(family="CMU Serif",size=60),
        legend.position=c(0.2,0.8),legend.background=element_rect(fill=NA));
ggsave("FFD_log_effects.png",width=10,height=7,FFD_log_plt,device="png");

ef_lm_FoM_log <- effect(c("morph_coded*trial_type_cod"),lm_FoM_log);
ef_lm_FoM_log <- data.frame(ef_lm_FoM_log);
ef_lm_FoM_log <- ef_lm_FoM_log[ef_lm_FoM_log$morph_coded==-0.5|ef_lm_FoM_log$morph_coded==0.5,];
ef_lm_FoM_log$morph_type <- ef_lm_FoM_log$morph_coded;
ef_lm_FoM_log$morph_type[ef_lm_FoM_log$morph_type==-0.5] <- "simple";
ef_lm_FoM_log$morph_type[ef_lm_FoM_log$morph_type==0.5] <- "complex";
ef_lm_FoM_log$morph_type <- factor(ef_lm_FoM_log$morph_type, levels=c("simple","complex"));

FoM_log_plt <- ggplot(ef_lm_FoM_log, aes(y=exp(fit), x=trial_type_cod, group=morph_type)) +
  geom_line(aes(color=morph_type, linetype=morph_type), linewidth=1, position=position_dodge(width=0.2)) +
  geom_point(aes(color=morph_type, shape=morph_type), size=3, position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=exp(lower), ymax=exp(upper), color=morph_type), width=0.2, position=position_dodge(width=0.2)) +
  
  labs(x="Preview type",y="FoM (ms)") +
  scale_x_discrete(labels=c("Identical","Cognate","Legal nonword","Illegal nonword")) +
  scale_linetype_manual(values=c("dashed", "solid"),name="Morphological type") +
  scale_colour_manual(values=c("#859a2f","#1a8de5"),name="Morphological type") +
  scale_shape_manual(values=c(16,17),name="Morphological type") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 60, colour = "black"),
        text=element_text(family="CMU Serif",size=60),
        legend.position=c(0.2,0.8),legend.background=element_rect(fill=NA));
ggsave("FoM_log_effects.png",width=10,height=7,FoM_log_plt,device="png");

ef_lm_GD_log <- effect(c("morph_coded*trial_type_cod"),lm_GD_log);
ef_lm_GD_log <- data.frame(ef_lm_GD_log);
ef_lm_GD_log <- ef_lm_GD_log[ef_lm_GD_log$morph_coded==-0.5|ef_lm_GD_log$morph_coded==0.5,];
ef_lm_GD_log$morph_type <- ef_lm_GD_log$morph_coded;
ef_lm_GD_log$morph_type[ef_lm_GD_log$morph_type==-0.5] <- "simple";
ef_lm_GD_log$morph_type[ef_lm_GD_log$morph_type==0.5] <- "complex";
ef_lm_GD_log$morph_type <- factor(ef_lm_GD_log$morph_type, levels=c("simple","complex"));

GD_log_plt <- ggplot(ef_lm_GD_log, aes(y=exp(fit), x=trial_type_cod, group=morph_type)) +
  geom_line(aes(color=morph_type, linetype=morph_type), linewidth=1, position=position_dodge(width=0.2)) +
  geom_point(aes(color=morph_type, shape=morph_type), size=3, position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=exp(lower), ymax=exp(upper), color=morph_type), width=0.2, position=position_dodge(width=0.2)) +
  
  labs(x="Preview type",y="GD (ms)") +
  scale_x_discrete(labels=c("Identical","Cognate","Legal nonword","Illegal nonword")) +
  scale_linetype_manual(values=c("dashed", "solid"),name="Morphological type") +
  scale_colour_manual(values=c("#859a2f","#1a8de5"),name="Morphological type") +
  scale_shape_manual(values=c(16,17),name="Morphological type") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 60, colour = "black"),
        text=element_text(family="CMU Serif",size=60),
        legend.position=c(0.2,0.8),legend.background=element_rect(fill=NA));
ggsave("GD_log_effects.png",width=10,height=7,GD_log_plt,device="png");

ef_lm_GPD_log <- effect(c("morph_coded*trial_type_cod"),lm_GPD_log);
ef_lm_GPD_log <- data.frame(ef_lm_GPD_log);
ef_lm_GPD_log <- ef_lm_GPD_log[ef_lm_GPD_log$morph_coded==-0.5|ef_lm_GPD_log$morph_coded==0.5,];
ef_lm_GPD_log$morph_type <- ef_lm_GPD_log$morph_coded;
ef_lm_GPD_log$morph_type[ef_lm_GPD_log$morph_type==-0.5] <- "simple";
ef_lm_GPD_log$morph_type[ef_lm_GPD_log$morph_type==0.5] <- "complex";
ef_lm_GPD_log$morph_type <- factor(ef_lm_GPD_log$morph_type, levels=c("simple","complex"));

GPD_log_plt <- ggplot(ef_lm_GPD_log, aes(y=exp(fit), x=trial_type_cod, group=morph_type)) +
  geom_line(aes(color=morph_type, linetype=morph_type), linewidth=1, position=position_dodge(width=0.2)) +
  geom_point(aes(color=morph_type, shape=morph_type), size=3, position=position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin=exp(lower), ymax=exp(upper), color=morph_type), width=0.2, position=position_dodge(width=0.2)) +
  
  labs(x="Preview type",y="GPD (ms)") +
  scale_x_discrete(labels=c("Identical","Cognate","Legal nonword","Illegal nonword")) +
  scale_linetype_manual(values=c("dashed", "solid"),name="Morphological type") +
  scale_colour_manual(values=c("#859a2f","#1a8de5"),name="Morphological type") +
  scale_shape_manual(values=c(16,17),name="Morphological type") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 60, colour = "black"),
        text=element_text(family="CMU Serif",size=60),
        legend.position=c(0.2,0.8),legend.background=element_rect(fill=NA));
ggsave("GPD_log_effects.png",width=10,height=7,GPD_log_plt,device="png");




