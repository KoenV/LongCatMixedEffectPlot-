##########################################################################################################################
# Function LongCatMixedEffectPlot: makes a plot of the mean of multiple groups when time is categorical
# Analyses: koen.vanbrabant@kuleuven.be
# date: 3/12/2016
###########################################################################################################################
# TODO: (1) give labels for plot; (2) select CI level

# dependencies
library(ggplot2); theme_set(theme_bw())
library(lme4)
library(plyr)

# function
LongCatMixedEffectPlot = function(fit){
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
    c0 = suppressMessages(confint(re_fit,parm=effect.names[1:(length(estimates)/2)]))
    mean_ci.df = data.frame(estimates[1:(length(estimates)/2)],c0)
    mean_ci.df$moment = rownames(mean_ci.df)
    mean_ci.df$moment.cont = 1:nrow(mean_ci.df)
    mean_ci.df$condition = condition.levels[i]
    names(mean_ci.df) = c('y','lower_ci','upper_ci','moment','moment.cont','condition')
    list_with_info[[i]] = mean_ci.df
  }
  df_with_info = do.call("rbind", list_with_info)
  df_with_info$moment = rep(unique(data$moment),length(list_with_info))
  

  # get number of observations for each measurement moment conditional on condition
  data$dummy_count = 1
  n_table = ddply(data[!is.na(data$y),],c('condition','moment'),summarise,total=sum(dummy_count))
  n_table$moment.cont = df_with_info$moment.cont
  n_table$placement = range(df_with_info$y)[1] - (range(df_with_info$y)[2] - range(df_with_info$y)[1])/1.5

  

  # plotting
  # get labels for plot
  plot_labels = list(
    x=names(fit@frame)[2],
    y = names(fit@frame)[1],
    shape=''
  )
  
  # make the plot
  ggplot() + 
      geom_point(data=df_with_info,aes(y=y,x=moment.cont,shape=condition),
                                     position=position_dodge(width = .7)) +
    geom_linerange(data=df_with_info,aes(ymin=lower_ci,ymax=upper_ci,x=moment.cont,group=condition),
                   alpha=.5,position=position_dodge(width = .7)) + scale_x_discrete(limits=df_with_info$moment) +
  
    geom_text(data=n_table,aes(x=moment.cont,y=placement,label=c(total),group=condition),
                                    position = position_dodge(width = .7)) +
    labs(plot_labels) 
}
    

  
  






