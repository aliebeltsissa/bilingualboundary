library(paletteer);
cols <- paletteer_d("colorBlindness::SteppedSequential5Steps");
simple_col <- "#1a8de5";
complex_col <- "#859a2f";
green_col <- "#2b725a";
  
library(extrafont);
font_import();
loadfonts();
par(family="Montserrat");

library(dplyr);

setwd("C:/Users/annal/OneDrive/Documents/GitHub/bilingualboundary/data");

all_ET <- read.csv("all_ET.csv",header=T,sep=",");
all_scores <- read.csv("all_scores.csv",header=T,sep=",");

all_ET <- subset(all_ET, select = -c(X,pre_target,post_target,type,TextBlock,adjusted_seq,fixations,rec_id,msgs));

all_ET$morph_type <- factor(all_ET$morph_type, levels=c("simple", "complex"));
all_ET$lst_type <- factor(all_ET$lst_type);
all_ET$target <- factor(all_ET$target);
all_ET$preview <- factor(all_ET$preview);
all_ET$trial_type <- factor(all_ET$trial_type, levels=c("identical", "cognate", "legal_nonword", "illegal_nonword"));
summary(all_ET);

all_scores <- subset(all_scores, select = -c(X));

all_scores$sbj_ID <- as.factor(all_scores$sbj_ID);
all_scores$gender <- as.factor(all_scores$gender);
summary(all_scores);
# gender
# F: 18, M: 14
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

# correlation of all gaze metrics
cor(all_ET[,c("GD_all","FFD_all","GPD_all","FoM_all")],use='pairwise.complete.obs');

all_data <- merge(all_ET, all_scores,by="sbj_ID");
summary(all_data);
# NAs are for the trials with skipped N-1 words or target words

# ET metrics histograms
par(mfrow=c(2,2));
par(mar=c(5,5,2,2));
hist(all_data$GD_all,breaks=50,main="GD",xlab="GD (ms)");
hist(all_data$FFD_all,breaks=50,main="FFD",xlab="FFD (ms)");
hist(all_data$GPD_all,breaks=seq(0,16000,100),main="GPD",xlab="GPD (ms)");
hist(all_data$FoM_all,breaks=50,main="FoM",xlab="FoM (ms)");
par(mfrow=c(1,1));
hist(all_data$GPD_all[all_data$GPD<2000],breaks=50);
hist(all_data$GPD_all[all_data$GPD>2000],breaks=50); # excluded? okay?

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

# potential trial exclusions - too long GPD
all_data$trial_issue[all_data$GPD_all>6000] <- "LONG_GPD";
all_data$trial_issue <- factor(all_data$trial_issue);

setwd("C:/Users/annal/OneDrive/Documents/Me/SISSA/BBET/BBET_analysis/ISB_analysis");
library(ggplot2);
# FFD
FFD_plt <- ggplot(all_data, aes(x=trial_type, y=FFD_all, color=morph_type, group=morph_type)) + 
  scale_color_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "FFD (ms)", color = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "Montserrat", size = 15, color = "black"),
        text=element_text(family="Montserrat",size=18),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(FFD_plt, filename = "FFD_bycondition.png", bg = "transparent", width=7,height=6);

#FoM
FoM_plt <- ggplot(all_data, aes(x=trial_type, y=FoM_all, color=morph_type, group=morph_type)) + 
  scale_color_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "FoM (ms)", color = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "Montserrat", size = 15, color = "black"),
        text=element_text(family="Montserrat",size=18),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(FoM_plt, filename = "FoM_bycondition.png", bg = "transparent", width=7,height=6);

# GD
GD_plt <- ggplot(all_data, aes(x=trial_type, y=GD_all, color=morph_type, group=morph_type)) + 
  scale_color_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "GD (ms)", color = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "Montserrat", size = 15, color = "black"),
        text=element_text(family="Montserrat",size=18),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(GD_plt, filename = "GD_bycondition.png", bg = "transparent", width=7,height=6);

# GPD
GPD_plt <- ggplot(all_data, aes(x=trial_type, y=GPD_all, color=morph_type, group=morph_type)) + 
  scale_color_manual(values=c(simple_col,complex_col)) +
  labs(x = "Condition", y = "GPD (ms)", color = "Morphology type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(family = "Montserrat", size = 15, color = "black"),
        text=element_text(family="Montserrat",size=18),
        legend.position=c(0.3,0.9),
        rect = element_rect(fill="transparent")) +
  stat_summary(geom = "point",fun = "mean",size = 4,shape = 19) +
  stat_summary(geom = "line",fun = "mean",linewidth=1.25) +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.1) +
  scale_x_discrete(labels=c("Identical", "Cognate", "Orthography", "Baseline"));
ggsave(GPD_plt, filename = "GPD_bycondition.png", bg = "transparent", width=7,height=6);

# means for each condition
library(plotrix);
#FFD
FFD_sID <- mean(all_data$FFD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='identical']);
# 246ms
FFD_sCG <- mean(all_data$FFD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='cognate']);
# 274ms
FFD_sLN <- mean(all_data$FFD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='legal_nonword']);
# 278ms
FFD_sIN <- mean(all_data$FFD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='illegal_nonword']);
# 302ms
FFD_cID <- mean(all_data$FFD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='identical']);
# 250ms
FFD_cCG <- mean(all_data$FFD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='cognate']);
# 261ms
FFD_cLN <- mean(all_data$FFD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='legal_nonword']);
# 281ms
FFD_cIN <- mean(all_data$FFD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='illegal_nonword']);
# 296ms
FFDsimple <- c(FFD_sID, FFD_sCG, FFD_sLN, FFD_sIN);
FFDcomplex <- c(FFD_cID, FFD_cCG, FFD_cLN, FFD_cIN);
FFD <- data.frame(FFDsimple,FFDcomplex);
colnames(FFD) <- c("simple","complex");
rownames(FFD) <- c("ID","CG","LN","IN");

FFD_IDcost <- FFD_sCG - FFD_sID; # 28ms
FFD_complexcost <- mean(c(FFD_cID-FFD_sID,FFD_cIN-FFD_sIN,FFD_cIN-FFD_sIN)); # -3ms
FFD_Sbenefit <- FFD_sCG-FFD_sLN; # -4ms
FFD_Obenefit <- FFD_sLN-FFD_sIN; # -25ms
FFD_Mbenefit <- (FFD_complexcost - (FFD_cCG-FFD_sCG)); # 11ms
  
#FoM
FoM_sID <- mean(all_data$FoM_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='identical'],na.rm=TRUE);
# 232ms
FoM_sCG <- mean(all_data$FoM_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='cognate'],na.rm=TRUE);
# 260ms
FoM_sLN <- mean(all_data$FoM_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='legal_nonword'],na.rm=TRUE);
# 250ms
FoM_sIN <- mean(all_data$FoM_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='illegal_nonword'],na.rm=TRUE);
# 2289ms
FoM_cID <- mean(all_data$FoM_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='identical'],na.rm=TRUE);
# 241ms
FoM_cCG <- mean(all_data$FoM_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='cognate'],na.rm=TRUE);
# 249ms
FoM_cLN <- mean(all_data$FoM_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='legal_nonword'],na.rm=TRUE);
# 268ms
FoM_cIN <- mean(all_data$FoM_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='illegal_nonword'],na.rm=TRUE);
# 259ms
FoMsimple <- c(FoM_sID, FoM_sCG, FoM_sLN, FoM_sIN);
FoMcomplex <- c(FoM_cID, FoM_cCG, FoM_cLN, FoM_cIN);
FoM <- data.frame(FoMsimple,FoMcomplex);
colnames(FoM) <- c("simple","complex");
rownames(FoM) <- c("ID","CG","LN","IN");

FoM_IDcost <- FoM_sCG - FoM_sID; # 28ms
FoM_complexcost <- mean(c(FoM_cID-FoM_sID,FoM_cIN-FoM_sIN,FoM_cIN-FoM_sIN)); # -17ms
FoM_Sbenefit <- FoM_sCG-FoM_sLN; # 11ms
FoM_Obenefit <- FoM_sLN-FoM_sIN; # -39ms
FoM_Mbenefit <- (FoM_complexcost - (FoM_cCG-FoM_sCG)); # -5

#GPD
GPD_sID <- mean(all_data$GPD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='identical']);
# 427ms
GPD_sCG <- mean(all_data$GPD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='cognate']);
# 474ms
GPD_sLN <- mean(all_data$GPD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='legal_nonword']);
# 468ms
GPD_sIN <- mean(all_data$GPD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='illegal_nonword']);
# 594ms
GPD_cID <- mean(all_data$GPD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='identical']);
# 441ms
GPD_cCG <- mean(all_data$GPD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='cognate']);
# 501ms
GPD_cLN <- mean(all_data$GPD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='legal_nonword']);
# 552ms
GPD_cIN <- mean(all_data$GPD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='illegal_nonword']);
# 615ms
GPDsimple <- c(GPD_sID, GPD_sCG, GPD_sLN, GPD_sIN);
GPDcomplex <- c(GPD_cID, GPD_cCG, GPD_cLN, GPD_cIN);
GPD <- data.frame(GPDsimple,GPDcomplex);
colnames(GPD) <- c("simple","complex");
rownames(GPD) <- c("ID","CG","LN","IN");

GPD_IDcost <- GPD_sCG - GPD_sID; # 47ms
GPD_complexcost <- mean(c(GPD_cID-GPD_sID,GPD_cIN-GPD_sIN,GPD_cIN-GPD_sIN)); # 18ms
GPD_Sbenefit <- GPD_sCG-GPD_sLN; # 6ms
GPD_Obenefit <- GPD_sLN-GPD_sIN; # -126ms
GPD_Mbenefit <- (GPD_complexcost - (GPD_cCG-GPD_sCG)); # -9ms

#GD
GD_sID <- mean(all_data$GD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='identical']);
# 323ms
GD_sCG <- mean(all_data$GD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='cognate']);
# 363ms
GD_sLN <- mean(all_data$GD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='legal_nonword']);
# 372ms
GD_sIN <- mean(all_data$GD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='simple'&all_data$trial_type=='illegal_nonword']);
# 401ms
GD_cID <- mean(all_data$GD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='identical']);
# 394ms
GD_cCG <- mean(all_data$GD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='cognate']);
# 389ms
GD_cLN <- mean(all_data$GD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='legal_nonword']);
# 419ms
GD_cIN <- mean(all_data$GD_noshortfixs[all_data$trial_issue=='N'&all_data$morph_type=='complex'&all_data$trial_type=='illegal_nonword']);
# 437ms
GDsimple <- c(GD_sID, GD_sCG, GD_sLN, GD_sIN);
GDcomplex <- c(GD_cID, GD_cCG, GD_cLN, GD_cIN);
GD <- data.frame(GDsimple,GDcomplex);
colnames(GD) <- c("simple","complex");
rownames(GD) <- c("ID","CG","LN","IN");

GD_IDcost <- GD_sCG - GD_sID; # 39ms
GD_complexcost <- mean(c(GD_cID-GD_sID,GD_cIN-GD_sIN,GD_cIN-GD_sIN)); # 48ms
GD_Sbenefit <- GD_sCG-GD_sLN; # -9ms
GD_Obenefit <- GD_sLN-GD_sIN; # -29ms
GD_Mbenefit <- -(GD_complexcost - (GD_cCG-GD_sCG)); # -21ms

# all costs/benefits together
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

library(scales)
FFD_costs <- ggplot(all_costs, aes(x=type, y=FFD)) +
  geom_bar(stat="identity",fill=green_col) +
  geom_hline(yintercept=0) +
  labs(y = "FFD (ms)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "Montserrat", size = 12, color = "black"),
        text=element_text(family="Montserrat",size=18),
        rect = element_rect(fill="transparent")) +
  scale_x_discrete(labels = wrap_format(10));
ggsave(FFD_costs, filename = "FFD_costs.png", bg = "transparent", width=7,height=6);

FoM_costs <- ggplot(all_costs, aes(x=type, y=FoM)) +
  geom_bar(stat="identity",fill=green_col) +
  geom_hline(yintercept=0) +
  labs(y = "FoM (ms)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "Montserrat", size = 12, color = "black"),
        text=element_text(family="Montserrat",size=18),
        rect = element_rect(fill="transparent")) +
  scale_x_discrete(labels = wrap_format(10));
ggsave(FoM_costs, filename = "FoM_costs.png", bg = "transparent", width=7,height=6);

GPD_costs <- ggplot(all_costs, aes(x=type, y=GPD)) +
  geom_bar(stat="identity",fill=green_col) +
  geom_hline(yintercept=0) +
  labs(y = "GPD (ms)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "Montserrat", size = 12, color = "black"),
        text=element_text(family="Montserrat",size=18),
        rect = element_rect(fill="transparent")) +
  scale_x_discrete(labels = wrap_format(10));
ggsave(GPD_costs, filename = "GPD_costs.png", bg = "transparent", width=7,height=6);

GD_costs <- ggplot(all_costs, aes(x=type, y=GD)) +
  geom_bar(stat="identity",fill=green_col) +
  geom_hline(yintercept=0) +
  labs(y = "GD (ms)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(family = "Montserrat", size = 12, color = "black"),
        text=element_text(family="Montserrat",size=18),
        rect = element_rect(fill="transparent")) +
  scale_x_discrete(labels = wrap_format(10));
ggsave(GD_costs, filename = "GD_costs.png", bg = "transparent", width=7,height=6);


# contrast coding
all_data <- all_data %>% 
  mutate(morph_coded = case_when(morph_type == 'simple' ~ -0.5,
                               morph_type == 'complex' ~ 0.5))

# lmers
library(lmerTest);
library(emmeans);
# all data
lm_FFD_all <- lmer(FFD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FFD_all, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_all <- lmer(FoM_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FoM_all, pairwise ~ trial_type, adjust = "tukey");
lm_GD_all <- lmer(GD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
# morph_score sig: beta=-6, p=0.02
emmeans(lm_GD_all, pairwise ~ trial_type, adjust = "tukey");
lm_GPD_all <- lmer(GPD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GPD_all, pairwise ~ trial_type, adjust = "tukey")

# without short fixations
lm_FFD_noshortfixs <- lmer(FFD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FFD_noshortfixs, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_noshortfixs <- lmer(FoM_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_FoM_noshortfixs, pairwise ~ trial_type, adjust = "tukey");
lm_GD_noshortfixs <- lmer(GD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GD_noshortfixs, pairwise ~ trial_type, adjust = "tukey");
# morph_score sig: beta=-6, p=0.02
lm_GPD_noshortfixs <- lmer(GPD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N'|all_data$trial_issue=='LONG_GPD',]);
emmeans(lm_GPD_noshortfixs, pairwise ~ trial_type, adjust = "tukey")

# all data without long GPDs
lm_FFD_nolongGPDs <- lmer(FFD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_FFD_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_nolongGPDs <- lmer(FoM_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_FoM_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_GD_nolongGPDs <- lmer(GD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
# morph_score sig: beta=-6, p=0.02
emmeans(lm_GD_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_GPD_nolongGPDs <- lmer(GPD_all ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_GPD_nolongGPDs, pairwise ~ trial_type, adjust = "tukey")

# without short fixations or long GPDs
lm_FFD_noshortfixs_nolongGPDs <- lmer(FFD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_FFD_noshortfixs_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_FoM_noshortfixs_nolongGPDs <- lmer(FoM_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_FoM_noshortfixs_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_GD_noshortfixs_nolongGPDs <- lmer(GD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
# morph_score sig: beta=-6, p=0.02
emmeans(lm_GD_noshortfixs_nolongGPDs, pairwise ~ trial_type, adjust = "tukey");
lm_GPD_noshortfixs_nolongGPDs <- lmer(GPD_noshortfixs ~ scale(trial_id) + morph_score + lextale_score + changes_seen + morph_coded*trial_type + (1|sbj_ID), data=all_data[all_data$trial_issue=='N',]);
emmeans(lm_GPD_noshortfixs_nolongGPDs, pairwise ~ trial_type, adjust = "tukey")

