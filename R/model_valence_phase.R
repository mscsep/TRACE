# TRACE Meta-analyses output v6.3.19 [based on code Valeria]
# "Valeria Bonapersona & Milou Sep"
# Code based on / adapted from Bonapersona, V. (2019, May 20). The behavioral phenotype of early life adversity: a 3-level meta-analysis of rodent studies. Retrieved from osf.io/ra947


# Visualisation & posthoc tests

library(ggplot2)


# for testing
# model=mod.A
# TRACE_output(mod.H, 'clinical data')

TRACE_output <- function (model, title, subtitle){
  
  
  ##RQ 1: "Main effects PTSD on Learning and memory?
  Learning <- anova(model, L = c(.5,0, .5, 0))
  Memory <- anova(model, L = c(0,.5, 0, .5))
  
  ## ---> CODE needs to be checked!
  # some info: Test linear combinations (http://www.metafor-project.org/doku.php/tips:testing_factors_lincoms)
  # http://www.metafor-project.org/doku.php/tips:multiple_factors_interactions
  
  # RQ2 "Main effects of PTSD on neutral / stressful   # 20.4.21 now the other way around (with label emotional ipv stresvull)
  Neutral <- anova(model, L = c(.5,.5, 0, 0))
  Stressful <- anova(model, L = c(0,0, .5, .5))
  
  ## posthoc RQ2. is learning or memory most effected by valence? (Posthoc)
  phase.neutral <- anova(model, L = c(-1,1, 0, 0)) 
  phase.stressful <- anova(model, L = c(0,0, -1, 1)) # learning -
  
  # Posthoc RQ2 influence of valence on learning or memory
  valence.learning <-anova(model, L = c(-1,0, 1, 0)) # neutral = -
  valence.memory <- anova(model, L = c(0,-1, 0, 1))
  
  
  #RQ3: neutral and stressful learning and memory in  ## PRECIES hetzelfde als de values in ht model! -> so below values from e.g. model$se used
  #  nLearning <- anova(mod.H, L = c(1,0, 0, 0))
  #  nMemory <- anova(mod.H, L = c(0,1, 0, 0))
  # sLearning <- anova(mod.H, L = c(0,0, 1, 0))
  #  sMemory <- anova(mod.H, L = c(0,0, 0, 1))
  # 
  
  ##Summary results organized in table
  #resultMain <- data.frame(matrix(data = NA, nrow = 17, ncol = 8))
  resultMain <- data.frame(matrix(data = NA, nrow = 12, ncol = 8))
  
  
  colnames(resultMain) <- c("test", "ci.lb", "ci.ub", 
                            "effectsize", "se", "Zvalue",  
                            "Pvalue", "Pvalue_bonfCorr")
  
  resultMain[,1] <- c("Learning", "Memory", "Neutral", "Stressful",
                      "phase.neutral", "phase.stressful",
                      "valence.learning", "valence.memory",
                      "nLearning", "nMemory", "sLearning", "sMemory")
  
  resultMain[,4] <- round(c(Learning$Lb, Memory$Lb, 
                            Neutral$Lb, Stressful$Lb,
                            phase.neutral$Lb, phase.stressful$Lb,
                            valence.learning$Lb, valence.memory$Lb,
                            model$beta), digits = 4) #effect size
  
  resultMain[,5] <- round(c(Learning$se, Memory$se, 
                            Neutral$se, Stressful$se,
                            phase.neutral$se, phase.stressful$se,
                            valence.learning$se, valence.memory$se,
                            # nLearning$se, nMemory$se, sLearning$se, sMemory$se, # idem as model$se
                            model$se), digits = 4) #se
  
  resultMain[,6] <- round(c(Learning$zval, Memory$zval, 
                            Neutral$zval, Stressful$zval,
                            phase.neutral$zval, phase.stressful$zval,
                            valence.learning$zval, valence.memory$zval,
                            model$zval), digits = 4)  #zvalues
  
  resultMain[,7] <- round(c(Learning$pval, Memory$pval, 
                            Neutral$pval, Stressful$pval,
                            phase.neutral$pval, phase.stressful$pval,
                            valence.learning$pval, valence.memory$pval,
                            model$pval), digits = 4)  #pvalues
  
  resultMain[,2] <- round(resultMain[,4] - (resultMain[,5] * 1.96), digits = 4) #CI lower 
  resultMain[,3] <- round(resultMain[,4] + (resultMain[,5] * 1.96), digits = 4) #CI upper
  
  # 16.4.21 was 2 changed to 12, as the number of comparisons (within one hypothesis).. NB same resutls for correction by 2 or 12 (for human & animal)
  # Beter checken! (not needed for now? only report model effect ("different from 0"), not yet the difference between each other.))
  resultMain[,8] <- round(c(p.adjust(resultMain[ 1,7], method = "bonferroni", n = 12), # learning (I think 2, as each estimate is used once for a phase contrast (L, M) and once for a valence contrast (S,N))
                            p.adjust(resultMain[ 2,7], method = "bonferroni", n = 12), # memory
                            p.adjust(resultMain[ 3,7], method = "bonferroni", n = 12), # neutral
                            p.adjust(resultMain[ 4,7], method = "bonferroni", n = 12), # stressful
                            p.adjust(resultMain[ 5,7], method = "bonferroni", n = 12), # l vs m (neutral) (also 2 for all posthocs?)
                            p.adjust(resultMain[ 6,7], method = "bonferroni", n = 12), # l vs m (stress)
                            p.adjust(resultMain[ 7,7], method = "bonferroni", n = 12), # neutral vs stres (learning)
                            p.adjust(resultMain[ 8,7], method = "bonferroni", n = 12), # neutral vs stres (memory)
                            
                            p.adjust(resultMain[ 9,7], method = "bonferroni", n = 12), 
                            p.adjust(resultMain[ 10,7], method = "bonferroni", n = 12), 
                            p.adjust(resultMain[ 11,7], method = "bonferroni", n = 12), 
                            p.adjust(resultMain[ 12,7], method = "bonferroni", n = 12)), 
                            
                            # resultMain[9,7],
                            # resultMain[10,7],
                            # resultMain[11,7],
                            # resultMain[12,7])
                           digits = 4) #pvalue bonf correction

  
  plot_data<-resultMain %>% filter(test %in% c("nLearning", "nMemory", "sLearning", "sMemory"))
  plot_data$valence <- ifelse(plot_data$test %in% c("nLearning", "nMemory"), "Neutral", "Stressful")
  plot_data$phase <- ifelse(plot_data$test %in% c("nLearning", "sLearning"), "Learning", "Memory")
  
  # signal sig. categories. Add index for p-values smaller than 0.05
    plot_data$sig<- ifelse(plot_data$Pvalue_bonfCorr  <0.05, 1, 0)

    
  
#str(plot_data)
# title='test'
    
  y_sig_position<-  ifelse(title=='Clinical Data', 1  , 3.3 )
  #  title='Preclinical Data'
  
    plot_MILOU <-
    ggplot(plot_data, 
               aes(x = test, y = effectsize, fill=valence)) +
     
    ylab("Hedge's G (CI)") + # could also be "Standardized mean difference"
     ggtitle(title, subtitle = subtitle) +

      facet_wrap(.~ phase, scales='free_x', strip.position = "bottom" )+

      theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5, face="italic")) +
    
      scale_fill_manual(
        values= c( "Neutral" = "steelblue2", # Colours: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
                   "Stressful" = "tomato2"))+
     
    geom_bar(stat = "identity")+ 
      
    geom_hline(yintercept = 0, size = 1) + 
    geom_errorbar(aes(ymin = ci.lb, 
                      ymax = ci.ub),
                  width = .3) +
      
   geom_point(data = plot_data[plot_data$sig ==1, ],aes(x=test, y=y_sig_position),shape = "*", size=10, show.legend = FALSE)  # to make position dynamic use: y=ci.ub+1.5
  
  
  
  
  out<-list(resultMain,plot_MILOU)
  return(out)
}
