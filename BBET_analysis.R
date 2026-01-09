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

# colours
cols <- paletteer_d("colorBlindness::SteppedSequential5Steps");
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

all_ET$trial_issue[all_ET$GPD_all>6000] <- "LONG_GPD";
all_ET$morph_type <- factor(all_ET$morph_type, levels=c("simple", "complex"));
all_ET$lst_type <- factor(all_ET$lst_type);
all_ET$target <- factor(all_ET$target);
all_ET$preview <- factor(all_ET$preview);
all_ET$trial_type <- factor(all_ET$trial_type, levels=c("identical", "cognate", "legal_nonword", "illegal_nonword"));
all_ET$trial_issue <- factor(all_ET$trial_issue);
summary(all_ET);

all_scores <- subset(all_scores, select = -c(X));
all_scores$sbj_ID <- as.factor(all_scores$sbj_ID);
all_scores$gender <- as.factor(all_scores$gender);
summary(all_scores);
# gender
# F: 27, M: 23
# age
# min: 19, Q1: 20.25, med: 23, mean: 23.04, Q3: 25, max: 32 (6 NAs)
# comp_score
# min: 85%, Q1: 92.50%, med: 95%, mean: 95.12%, Q3: 97.50%, max: 100%
# LexTALE
# min: 75%, Q1: 80%, med: 85.62%, mean: 85.17%, Q3: 90%, max: 98.75%
# morph_score
# min: 75%, Q1: 85%, med: 90%, mean: 89.38%, Q3: 95%, max: 100%
# changes_seen
# min: 0%, Q1: 6.88%, med: 16%, mean: 16.41%, Q3: 23.13%, max: 40%
# N.1_skips
# min: 0, Q1: 2, med: 5, mean: 6, Q3: 9, max: 18, NAs: 7 (why do I have NAs?)
# target_skips
# min: 0, Q1: 0, med: 0, mean: 1.76, Q3: 2, max: 13, NAs: 7 (same question)

## Participant exclusions --------------------------------------------------
# exclude participants with comprehension score lower than 2.5 SD from mean
summary(all_scores$comp_score);
# min=80.00, Q1=93.09, med=95.00, mean=95.08, Q3=97.50, max=100

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
        text = element_text(family="CMU Serif",size=35,color="black"),
        legend.background=element_rect(fill=NA))

mean(all_scores$comp_score)-2.5*sd(all_scores$comp_score); # min=84.19
mean(all_scores$comp_score); # mean=95.08
mean(all_scores$comp_score)+2.5*sd(all_scores$comp_score); # max=100

for (i in all_scores$sbj_ID) {
  if (all_scores[all_scores$sbj_ID==i,]$comp_score < mean(all_scores$comp_score)-2.5*sd(all_scores$comp_score)) {
    print(paste("Participant ",i," had a comprehension check score below 2.5*SD."))
  }
}
# EXCLUDE PARTICIPANT 58

all_scoresclean <- all_scores[!all_scores$sbj_ID %in% c('58'),];
all_ETclean <- all_ET[!all_ET$sbj_ID %in% c('58'),];
# now n=49

# Correlation of all gaze metrics ----------------------------------------
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

all_data <- merge(all_ETclean, all_scoresclean,by="sbj_ID");
# NAs are for the trials with skipped N-1 words or target words

# ET metrics histograms
par(mfrow=c(2,2));
par(mar=c(5,5,2,2));
hist(all_data$GD_all[all_data$trial_issue=='N'],breaks=50,main="GD",xlab="GD (ms)");
hist(all_data$FFD_all[all_data$trial_issue=='N'],breaks=50,main="FFD",xlab="FFD (ms)");
hist(all_data$GPD_all[all_data$trial_issue=='N'],breaks=seq(0,16000,100),main="GPD",xlab="GPD (ms)");
hist(all_data$FoM_all[all_data$trial_issue=='N'],breaks=50,main="FoM",xlab="FoM (ms)");
par(mfrow=c(1,1));
hist(all_data$GPD_all[all_data$GPD_all<2000 & all_data$trial_issue=='N'],breaks=50);
hist(all_data$GPD_all[all_data$GPD_all>2000 & all_data$trial_issue=='N'],breaks=50); # excluded? okay?

# ET metrics boxplots
par(mfrow=c(2,2));
boxplot(all_data$GD_all, main="GD", ylab = "GD (ms)",yaxs="i");
abline(h=mean(all_data$GD_all), lty=5);
boxplot(all_data$FFD_all, main="FFD", ylab = "FFD (ms)",yaxs="i");
abline(h=mean(all_data$FFD_all), lty=5);
boxplot(all_data$GPD_all, main="GPD", ylab = "GPD (ms)",yaxs="i");
abline(h=mean(all_data$GPD_all), lty=5);
boxplot(all_data$FoM_all, main="FoM", ylab = "FoM (ms)",yaxs="i");
abline(h=mean(all_data$FoM_all), lty=5);
par(mfrow=c(1,1));


# Plotting -------------------------------------------------------------
setwd("C:/Users/annal/Documents/Me/SISSA/BBET/BBET_analysis/full_analysis");

# FFD
FFD_plt <- ggplot(all_data[all_data$trial_issue=='N',], aes(x=trial_type, y=FFD_cleanedfixs, color=morph_type, group=morph_type)) + 
  scale_color_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "FFD (ms)", color = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 30, color = "black"),
        text=element_text(family="CMU Serif",size=40),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(FFD_plt, filename = "FFD_bycondition.png", bg = "transparent", width=7,height=6);

#FoM
FoM_plt <- ggplot(all_data[all_data$trial_issue=='N',], aes(x=trial_type, y=FoM_cleanedfixs, color=morph_type, group=morph_type)) + 
  scale_color_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "FoM (ms)", color = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 30, color = "black"),
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
GD_plt <- ggplot(all_data[all_data$trial_issue=='N',], aes(x=trial_type, y=GD_cleanedfixs, color=morph_type, group=morph_type)) + 
  scale_color_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "GD (ms)", color = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 30, color = "black"),
        text=element_text(family="CMU Serif",size=40),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(GD_plt, filename = "GD_bycondition.png", bg = "transparent", width=7,height=6);

# GPD
GPD_plt <- ggplot(all_data[all_data$trial_issue=='N',], aes(x=trial_type, y=GPD_cleanedfixs, color=morph_type, group=morph_type)) + 
  scale_color_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "GPD (ms)", color = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "CMU Serif", size = 30, color = "black"),
        text=element_text(family="CMU Serif",size=40),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(GPD_plt, filename = "GPD_bycondition.png", bg = "transparent", width=7,height=6);

# Means for each condition ------------------------------------------------
## FFD ----
FFD_sID <- mean(all_data$FFD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='identical']);
# 253ms
FFD_sCG <- mean(all_data$FFD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='cognate']);
# 279ms
FFD_sLN <- mean(all_data$FFD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='legal_nonword']);
# 278ms
FFD_sIN <- mean(all_data$FFD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='illegal_nonword']);
# 303ms
FFD_cID <- mean(all_data$FFD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='identical']);
# 256ms
FFD_cCG <- mean(all_data$FFD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='cognate']);
# 267ms
FFD_cLN <- mean(all_data$FFD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='legal_nonword']);
# 276ms
FFD_cIN <- mean(all_data$FFD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='illegal_nonword']);
# 297ms
FFDsimple <- c(FFD_sID, FFD_sCG, FFD_sLN, FFD_sIN);
FFDcomplex <- c(FFD_cID, FFD_cCG, FFD_cLN, FFD_cIN);
FFD <- data.frame(FFDsimple,FFDcomplex);
colnames(FFD) <- c("simple","complex");
rownames(FFD) <- c("ID","CG","LN","IN");

FFD_IDcost <- FFD_sCG - FFD_sID; # 26.08ms
FFD_complexcost <- mean(c(FFD_cID-FFD_sID,FFD_cIN-FFD_sIN,FFD_cIN-FFD_sIN)); # -2.81ms
FFD_Sbenefit <- FFD_sCG-FFD_sLN; # 0.25ms
FFD_Obenefit <- FFD_sLN-FFD_sIN; # -24.30ms
FFD_Mbenefit <- -(FFD_complexcost - (FFD_cCG-FFD_sCG)); # -8.60ms
  
## FoM ----
FoM_sID <- mean(all_data$FoM_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='identical'],na.rm=TRUE);
# 241ms
FoM_sCG <- mean(all_data$FoM_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='cognate'],na.rm=TRUE);
# 261ms
FoM_sLN <- mean(all_data$FoM_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='legal_nonword'],na.rm=TRUE);
# 253ms
FoM_sIN <- mean(all_data$FoM_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='illegal_nonword'],na.rm=TRUE);
# 276ms
FoM_cID <- mean(all_data$FoM_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='identical'],na.rm=TRUE);
# 245ms
FoM_cCG <- mean(all_data$FoM_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='cognate'],na.rm=TRUE);
# 251ms
FoM_cLN <- mean(all_data$FoM_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='legal_nonword'],na.rm=TRUE);
# 262ms
FoM_cIN <- mean(all_data$FoM_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='illegal_nonword'],na.rm=TRUE);
# 267ms
FoMsimple <- c(FoM_sID, FoM_sCG, FoM_sLN, FoM_sIN);
FoMcomplex <- c(FoM_cID, FoM_cCG, FoM_cLN, FoM_cIN);
FoM <- data.frame(FoMsimple,FoMcomplex);
colnames(FoM) <- c("simple","complex");
rownames(FoM) <- c("ID","CG","LN","IN");

FoM_IDcost <- FoM_sCG - FoM_sID; # 20.22ms
FoM_complexcost <- mean(c(FoM_cID-FoM_sID,FoM_cIN-FoM_sIN,FoM_cIN-FoM_sIN)); # -4.94ms
FoM_Sbenefit <- FoM_sCG-FoM_sLN; # 7.83ms
FoM_Obenefit <- FoM_sLN-FoM_sIN; # -23.24ms
FoM_Mbenefit <- -(FoM_complexcost - (FoM_cCG-FoM_sCG)); # -4.65ms

## GPD ----
GPD_sID <- mean(all_data$GPD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='identical']);
# 414ms
GPD_sCG <- mean(all_data$GPD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='cognate']);
# 459ms
GPD_sLN <- mean(all_data$GPD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='legal_nonword']);
# 473ms
GPD_sIN <- mean(all_data$GPD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='illegal_nonword']);
# 591ms
GPD_cID <- mean(all_data$GPD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='identical']);
# 446ms
GPD_cCG <- mean(all_data$GPD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='cognate']);
# 493ms
GPD_cLN <- mean(all_data$GPD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='legal_nonword']);
# 515ms
GPD_cIN <- mean(all_data$GPD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='illegal_nonword']);
# 596ms
GPDsimple <- c(GPD_sID, GPD_sCG, GPD_sLN, GPD_sIN);
GPDcomplex <- c(GPD_cID, GPD_cCG, GPD_cLN, GPD_cIN);
GPD <- data.frame(GPDsimple,GPDcomplex);
colnames(GPD) <- c("simple","complex");
rownames(GPD) <- c("ID","CG","LN","IN");

GPD_IDcost <- GPD_sCG - GPD_sID; # 45.51ms
GPD_complexcost <- mean(c(GPD_cID-GPD_sID,GPD_cIN-GPD_sIN,GPD_cIN-GPD_sIN)); # 14.20ms
GPD_Sbenefit <- GPD_sCG-GPD_sLN; # -13.87ms
GPD_Obenefit <- GPD_sLN-GPD_sIN; # -118.18ms
GPD_Mbenefit <- -(GPD_complexcost - (GPD_cCG-GPD_sCG)); # 19.37ms

## GD ----
GD_sID <- mean(all_data$GD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='identical']);
# 338ms
GD_sCG <- mean(all_data$GD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='cognate']);
# 368ms
GD_sLN <- mean(all_data$GD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='legal_nonword']);
# 375ms
GD_sIN <- mean(all_data$GD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='illegal_nonword']);
# 399ms
GD_cID <- mean(all_data$GD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='identical']);
# 401ms
GD_cCG <- mean(all_data$GD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='cognate']);
# 401ms
GD_cLN <- mean(all_data$GD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='legal_nonword']);
# 418ms
GD_cIN <- mean(all_data$GD_cleanedfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='illegal_nonword']);
# 448ms
GDsimple <- c(GD_sID, GD_sCG, GD_sLN, GD_sIN);
GDcomplex <- c(GD_cID, GD_cCG, GD_cLN, GD_cIN);
GD <- data.frame(GDsimple,GDcomplex);
colnames(GD) <- c("simple","complex");
rownames(GD) <- c("ID","CG","LN","IN");

GD_IDcost <- GD_sCG - GD_sID; # 30.34ms
GD_complexcost <- mean(c(GD_cID-GD_sID,GD_cIN-GD_sIN,GD_cIN-GD_sIN)); # 54.02ms
GD_Sbenefit <- GD_sCG-GD_sLN; # -7.61ms
GD_Obenefit <- GD_sLN-GD_sIN; # -23.13ms
GD_Mbenefit <- -(GD_complexcost - (GD_cCG-GD_sCG)); # -21.35ms

## All costs/benefits together ----
FFD_costs <- c(FFD_IDcost,FFD_complexcost,FFD_Obenefit,FFD_Sbenefit,FFD_Mbenefit);
FoM_costs <- c(FoM_IDcost,FoM_complexcost,FoM_Obenefit,FoM_Sbenefit,FoM_Mbenefit);
GPD_costs <- c(GPD_IDcost,GPD_complexcost,GPD_Obenefit,GPD_Sbenefit,GPD_Mbenefit);
GD_costs <- c(GD_IDcost,GD_complexcost,GD_Obenefit,GD_Sbenefit,GD_Mbenefit);
label_costs <- c("Identical cost","Complex cost","Orthography benefit",
                  "Semantics benefit","Morphology benefit");
all_costs <- data.frame(label_costs,FFD_costs,FoM_costs,GPD_costs,GD_costs);
rownames(all_costs) <- c("ID_cost","complex_cost","Obenefit","Sbenefit","Mbenefit");
colnames(all_costs) <- c("type","FFD","FoM","GPD","GD");
all_costs$type <- factor(all_costs$type, levels=unique(all_costs$type));

FFD_costs <- ggplot(all_costs, aes(x=type, y=FFD)) +
  geom_bar(stat="identity",fill=green_col) +
  geom_hline(yintercept=0) +
  labs(y = "FFD (ms)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "CMU Serif", size = 20, color = "black"),
        text=element_text(family="CMU Serif",size=40),
        rect = element_rect(fill="transparent"));
ggsave(FFD_costs, filename = "FFD_costs.png", bg = "transparent", width=7,height=6);

FoM_costs <- ggplot(all_costs, aes(x=type, y=FoM)) +
  geom_bar(stat="identity",fill=green_col) +
  geom_hline(yintercept=0) +
  labs(y = "FoM (ms)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "CMU Serif", size = 20, color = "black"),
        text=element_text(family="CMU Serif",size=40),
        rect = element_rect(fill="transparent"));
ggsave(FoM_costs, filename = "FoM_costs.png", bg = "transparent", width=7,height=6);

GPD_costs <- ggplot(all_costs, aes(x=type, y=GPD)) +
  geom_bar(stat="identity",fill=green_col) +
  geom_hline(yintercept=0) +
  labs(y = "GPD (ms)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "CMU Serif", size = 20, color = "black"),
        text=element_text(family="CMU Serif",size=40),
        rect = element_rect(fill="transparent"));
ggsave(GPD_costs, filename = "GPD_costs.png", bg = "transparent", width=7,height=6);

GD_costs <- ggplot(all_costs, aes(x=type, y=GD)) +
  geom_bar(stat="identity",fill=green_col) +
  geom_hline(yintercept=0) +
  labs(y = "GD (ms)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "CMU Serif", size = 20, color = "black"),
        text=element_text(family="CMU Serif",size=40),
        rect = element_rect(fill="transparent"));
ggsave(GD_costs, filename = "GD_costs.png", bg = "transparent", width=7,height=6);


# LMERS ------------------------------------------------------------------
# contrast coding
all_data <- all_data %>% 
  mutate(morph_coded = case_when(morph_type == 'simple' ~ -0.5,
                                 morph_type == 'complex' ~ 0.5))
# all data
lm_FFD_all <- lmer(FFD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FFD_all, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_all <- lmer(FoM_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FoM_all, pairwise ~ trial_type, adjust = "tukey");
lm_GD_all <- lmer(GD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GD_all, pairwise ~ trial_type, adjust = "tukey");
lm_GPD_all <- lmer(GPD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GPD_all, pairwise ~ trial_type, adjust = "tukey");

# all data without long GPDs
lm_FFD_nolongGPDs <- lmer(FFD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_FFD_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_nolongGPDs <- lmer(FoM_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_FoM_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_GD_nolongGPDs <- lmer(GD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_GD_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_GPD_nolongGPDs <- lmer(GPD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_GPD_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");

# all data without short fixations
lm_FFD_noshortfixs <- lmer(FFD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FFD_noshortfixs, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_noshortfixs <- lmer(FoM_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FoM_noshortfixs, pairwise ~ trial_type, adjust = "tukey");
lm_GD_noshortfixs <- lmer(GD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GD_noshortfixs, pairwise ~ trial_type, adjust = "tukey");
lm_GPD_noshortfixs <- lmer(GPD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GPD_noshortfixs, pairwise ~ trial_type, adjust = "tukey");

# all data without long fixations
lm_FFD_nolongfixs <- lmer(FFD_nolongfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FFD_nolongfixs, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_nolongfixs <- lmer(FoM_nolongfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FoM_nolongfixs, pairwise ~ trial_type, adjust = "tukey");
lm_GD_nolongfixs <- lmer(GD_nolongfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GD_nolongfixs, pairwise ~ trial_type, adjust = "tukey");
lm_GPD_nolongfixs <- lmer(GPD_nolongfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GPD_nolongfixs, pairwise ~ trial_type, adjust = "tukey");

# all data without short or long fixations
lm_FFD_cleanedfixs <- lmer(FFD_cleanedfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FFD_cleanedfixs, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_cleanedfixs <- lmer(FoM_cleanedfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FoM_cleanedfixs, pairwise ~ trial_type, adjust = "tukey");
lm_GD_cleanedfixs <- lmer(GD_cleanedfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GD_cleanedfixs, pairwise ~ trial_type, adjust = "tukey");
lm_GPD_cleanedfixs <- lmer(GPD_cleanedfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GPD_cleanedfixs, pairwise ~ trial_type, adjust = "tukey")

