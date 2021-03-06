################################################################################
# Function LongCatMixedEffectPlot: makes a plot of the mean of multiple groups 
# when time is categorical
# Author: koen.vanbrabant@kuleuven.be
# date: 22/06/2017
################################################################################

# dependencies
library(ggplot2); theme_set(theme_bw())
library(lme4)
library(plyr)
library(gtable)
library(grid)
library(Hmisc)
library(lmerTest)

# function
LongCatMixedEffectPlot = function(
    fit,
    conf.level=.95,
    dodge.level=.70,
    transformation = c(),
    package=c('lme4','nlme')){
    
    if(package=='lme4'){
        
        # get the data out of the model,droplevel and give name
        data = data.frame(fit@frame[,1],fit@frame[,2],fit@frame[,3],fit@frame[,4])
        names(data) = c('y','moment','condition','id')
        condition.levels = levels(data$condition)
    
        # make a data.frame with estimates and CI
        nr_moment = length(unique(data$moment))
        nr_condition = length(unique(data$condition))
        
        output = suppressWarnings(lsmeans::lsmeans(fit,
            specs=names(fit@frame)[2:3]))
        
        ouput_v2 = summary(output)[,c(1,2,3,6,7)]
        
        ouput_v2$moment.cont = rep(1:nr_moment,nr_condition)
        
        names(ouput_v2) = c('moment','condition','y','lower_ci','upper_ci','moment.cont')
    
    
        levels = levels(data$moment)
        ouput_v2$moment = factor(ouput_v2$moment,levels = levels,ordered = TRUE)
    
    
        # get number of observations for each measurement moment conditional on condition
        data$dummy_count = 1
    
        n_table = ddply(data[!is.na(data$y),],c('condition','moment'),
            summarise,total=sum(!is.na(y)))
    
        n_table$moment.cont = ouput_v2$moment.cont
    
        if(transformation=='ihs'){
            ouput_v2[,c('y','lower_ci','upper_ci')] = sinh(ouput_v2[,c('y','lower_ci','upper_ci')])
            
        }
    
        ##### plotting
        ## get labels for mean plot
        plot_labels = list(
            x = ifelse(label(fit@frame)[2]=='','',label(fit@frame)[2]),
            y = ifelse(label(fit@frame)[1]=='',names(fit@frame)[1],label(fit@frame)[1]),
            shape='')
        ##make mean plot
        mean.plot = ggplot() + 
            geom_point(data=ouput_v2,
                aes(y=y,x=moment,shape=condition),
                position=position_dodge(width = dodge.level)) +
            geom_linerange(data=ouput_v2,
                aes(ymin=lower_ci,ymax=upper_ci,x=moment.cont,group=condition),
                alpha=.5,position=position_dodge(width = dodge.level)) + 
            scale_x_discrete(limits=levels(fit@frame$moment),expand=c(.01,.01)) +
            labs(plot_labels) + theme_bw() + theme(legend.position='top') +
            theme(axis.title.y=element_text(vjust=0.0))
    
    
        # number of subjects
        risk.plot = ggplot(n_table) + geom_text(aes(x=moment.cont,
            y=condition,label=total),size=3.5) +
            theme_bw() + 
            theme(axis.text.x = element_blank(), 
                axis.title.x = element_blank(), 
                axis.text.y = element_text(face="bold"),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(), panel.border = element_blank()) +
            theme(plot.title = element_text(hjust = 0)) 
    
    ## combine mean and risk.plot
    
    
    
        if(nlevels(ouput_v2$condition)<4){
            both = rbind(ggplotGrob(mean.plot), ggplotGrob(risk.plot), size="last")
            panels <- both$layout$t[grep("panel", both$layout$name)]
            both$heights[panels[1]] = unit(1,"null") 
            both$heights[panels[2]] = unit(2,"lines")
            grid.newpage()
            grid.draw(both)
            
        }else{
            both = rbind(ggplotGrob(mean.plot), ggplotGrob(risk.plot), size="last")
            panels = both$layout$t[grep("panel", both$layout$name)]
            both$heights[panels[1]] = unit(1.2,"null") 
            both$heights[panels[2]] = unit(.3,"null")
            grid.newpage()
            grid.draw(both)
            
        }
    }
    
    
    if(package=='nlme'){
        # get the data out of the model,droplevel and give name
        data = fit$data
        data = data[,c(rownames(attr(fit$terms,'factors')),names(fit$groups))]
        
    
        names(data) = c('y','moment','condition','id')
        condition.levels = levels(data$condition)
        
        # make a data.frame with estimates and CI
        nr_moment = length(unique(data$moment))
        nr_condition = length(unique(data$condition))
        
        output = suppressWarnings(lsmeans::lsmeans(fit,
            specs=attr(fit$terms,'term.labels')[1:2]))
        
        ouput_v2 = summary(output)[,c(1,2,3,6,7)]
        
        ouput_v2$moment.cont = rep(1:nr_moment,nr_condition)
        
        names(ouput_v2) = c('moment','condition','y','lower_ci','upper_ci','moment.cont')
        
        
        levels = levels(data$moment)
        ouput_v2$moment = factor(ouput_v2$moment,levels = levels,ordered = TRUE)
        
        if(transformation=='ihs'){
            ouput_v2[,c('y','lower_ci','upper_ci')] = sinh(ouput_v2[,c('y','lower_ci','upper_ci')])
            
        }
        
        # get number of observations for each measurement moment conditional on condition
        data$dummy_count = 1
        
        n_table = ddply(data[!is.na(data$y),],c('condition','moment'),
            summarise,total=sum(!is.na(y)))
        
        n_table$moment.cont = ouput_v2$moment.cont
        
        
        ##### plotting
        ## get labels for mean plot
        plot_labels = list(
            x = ifelse(label(data)[2]=='','',label(data)[2]),
            y = ifelse(label(data)[1]=='',names(data)[1],label(data)[1]),
            shape='')
        ##make mean plot
        mean.plot = ggplot() + 
            geom_point(data=ouput_v2,
                aes(y=y,x=moment,shape=condition),
                position=position_dodge(width = dodge.level)) +
            geom_linerange(data=ouput_v2,
                aes(ymin=lower_ci,ymax=upper_ci,x=moment.cont,group=condition),
                alpha=.5,position=position_dodge(width = dodge.level)) + 
            scale_x_discrete(limits=levels(data$moment),expand=c(.01,.01)) +
            labs(plot_labels) + theme_bw() + theme(legend.position='top') +
            theme(axis.title.y=element_text(vjust=0.0))
        
        
        # number of subjects
        risk.plot = ggplot(n_table) + geom_text(aes(x=moment.cont,
            y=condition,label=total),size=3.5) +
            theme_bw() + 
            theme(axis.text.x = element_blank(), 
                axis.title.x = element_blank(), 
                axis.text.y = element_text(face="bold"),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(), panel.border = element_blank()) +
            theme(plot.title = element_text(hjust = 0)) 
        
        ## combine mean and risk.plot
        
        
        
        if(nlevels(ouput_v2$condition)<4){
            both = rbind(ggplotGrob(mean.plot), ggplotGrob(risk.plot), size="last")
            panels <- both$layout$t[grep("panel", both$layout$name)]
            both$heights[panels[1]] = unit(1,"null") 
            both$heights[panels[2]] = unit(2,"lines")
            grid.newpage()
            grid.draw(both)
            
        }else{
            both = rbind(ggplotGrob(mean.plot), ggplotGrob(risk.plot), size="last")
            panels = both$layout$t[grep("panel", both$layout$name)]
            both$heights[panels[1]] = unit(1.2,"null") 
            both$heights[panels[2]] = unit(.3,"null")
            grid.newpage()
            grid.draw(both)
            
        }
    }
    
}























