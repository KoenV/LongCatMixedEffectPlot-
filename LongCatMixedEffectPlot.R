##########################################################################################################################
# Function LongCatMixedEffectPlot: makes a plot of the mean of multiple groups when time is categorical
# Author: koen.vanbrabant@kuleuven.be
# date: 8/12/2016
######################################################################################################################
# dependencies
library(ggplot2); theme_set(theme_bw())
library(lme4)
library(plyr)
library(gtable)
library(grid)
library(Hmisc)

# function
LongCatMixedEffectPlot = function(fit,conf.level=.95,dodge.level=.70){
  # get the data out of the model,droplevel and give name
  data = data.frame(fit@frame[,1],fit@frame[,2],fit@frame[,3],fit@frame[,4])
  names(data) = c('y','moment','condition','id')
  condition.levels = levels(data$condition)
  list_with_info = vector('list',length(condition.levels))
  
  # calculate mean and CI
  for(i in 1:length(condition.levels)){
    re_fit = lmer(y ~ - 1 + moment*relevel(condition,ref=condition.levels[i]) + (1|id), REML=FALSE,data=data)
    effect.names = labels(fixef(re_fit))   
    estimates = unname(fixef(re_fit)[1:length(effect.names)])
    c0 = suppressMessages(confint(re_fit,parm=effect.names[1:(length(estimates)/2)],level=conf.level))
    mean_ci.df = data.frame(estimates[1:(length(estimates)/2)],c0)
    mean_ci.df$moment = rownames(mean_ci.df)
    mean_ci.df$moment.cont = 1:nrow(mean_ci.df)
    mean_ci.df$condition = condition.levels[i]
    names(mean_ci.df) = c('y','lower_ci','upper_ci','moment','moment.cont','condition')
    list_with_info[[i]] = mean_ci.df
  }
  df_with_info = do.call("rbind", list_with_info)
  df_with_info$moment = rep(levels(fit@frame$moment),nlevels(fit@frame$condition.cat))
  
  
  # get number of observations for each measurement moment conditional on condition
  n_table = ddply(data[!is.na(data$y),],c('condition','moment'),summarise,total=sum(!is.na(y)))
  n_table$moment.cont = df_with_info$moment.cont

  
  ##### plotting
  ## get labels for mean plot
  plot_labels = list(
    x = ifelse(label(fit@frame)[2]=='','',label(fit@frame)[2]),
    y = ifelse(label(fit@frame)[1]=='',names(fit@frame)[1],label(fit@frame)[1]),
    shape='')
  ##make mean plot
  mean.plot = ggplot() + 
      geom_point(data=df_with_info,aes(y=y,x=moment.cont,shape=condition),
                                     position=position_dodge(width = dodge.level)) +
    geom_linerange(data=df_with_info,aes(ymin=lower_ci,ymax=upper_ci,x=moment.cont,group=condition),
                   alpha=.5,position=position_dodge(width = dodge.level)) + scale_x_discrete(limits=levels(fit@frame$moment)) +
    labs(plot_labels) + theme_bw() + theme(legend.position='top')
  
  
  # number of subjects
  risk.plot = ggplot(n_table) + geom_text(aes(x=moment.cont,y=condition,label=total),size=3.5) +
    theme_bw() + 
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_text(face="bold"),
          axis.text.y = element_blank(), 
          axis.title.y = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), panel.border = element_blank()
    ) + theme(plot.title = element_text(hjust = 0)) 
  
  ## combine mean and risk.plot
  both = rbind(ggplotGrob(mean.plot), ggplotGrob(risk.plot), size="last")
  panels <- both$layout$t[grep("panel", both$layout$name)]
  both$heights[panels[1]] = unit(1,"null") 
  both$heights[panels[2]] = unit(2,"lines")
  grid.newpage()
  grid.draw(both)
}
    

























