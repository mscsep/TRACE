# TRACE Meta-analyses output v6.3.19 [based on code Valeria]
library(ggplot2)


# for testing
#model=mod.H
# TRACE_output(mod.H, 'clinical data')

TRACE_output <- function (model, title){
  
  
  ##RQ 1: "Main effects PTSD on Learning and memory?
  Learning <- anova(model, L = c(.5,0, .5, 0))
  Memory <- anova(model, L = c(0,.5, 0, .5))
  
  ## ---> CODE needs to be checked!
  # some info: Test linear combinations (http://www.metafor-project.org/doku.php/tips:testing_factors_lincoms)
  
  # RQ2 "Main effects of
  Neutral <- anova(model, L = c(.5,.5, 0, 0))
  Stressful <- anova(model, L = c(0,0, .5, .5))
  
  ## posthoc RQ2. is learning or memory most effected by valence? (Posthoc)
  phase.neutral <- anova(model, L = c(-1,1, 0, 0)) 
  phase.stressful <- anova(model, L = c(0,0, -1, 1)) # learning -
  
  
  #RQ3: neutral and stressful learning and memory in  ## PRECIES hetzelfde als de values in ht model!
  # nLearning <- anova(mod.H, L = c(1,0, 0, 0))
  # nMemory <- anova(mod.H, L = c(0,1, 0, 0))
  # sLearning <- anova(mod.H, L = c(0,0, 1, 0))
  # sMemory <- anova(mod.H, L = c(0,0, 0, 1))
  # 
  
  ##Summary results organized in table
  #resultMain <- data.frame(matrix(data = NA, nrow = 17, ncol = 8))
  resultMain <- data.frame(matrix(data = NA, nrow = 10, ncol = 8))
  
  
  colnames(resultMain) <- c("test", "ci.lb", "ci.ub", 
                            "effectsize", "se", "Zvalue",  
                            "Pvalue", "Pvalue_bonfCorr")
  
  resultMain[,1] <- c("Learning", "Memory", "Neutral", "Stressful",
                      "phase.neutral", "phase.stressful",
                      "nLearning", "nMemory", "sLearning", "sMemory")
  
  resultMain[,4] <- round(c(Learning$Lb, Memory$Lb, 
                            Neutral$Lb, Stressful$Lb,
                            phase.neutral$Lb, phase.stressful$Lb,
                            model$beta), digits = 4) #effect size
  
  resultMain[,5] <- round(c(Learning$se, Memory$se, 
                            Neutral$se, Stressful$se,
                            phase.neutral$se, phase.stressful$se,
                            model$se), digits = 4) #se
  
  resultMain[,6] <- round(c(Learning$zval, Memory$zval, 
                            Neutral$zval, Stressful$zval,
                            phase.neutral$zval, phase.stressful$zval,
                            model$zval), digits = 4)  #zvalues
  
  resultMain[,7] <- round(c(Learning$pval, Memory$pval, 
                            Neutral$pval, Stressful$pval,
                            phase.neutral$pval, phase.stressful$pval,
                            model$pval), digits = 4)  #pvalues
  
  resultMain[,2] <- round(resultMain[,4] - (resultMain[,5] * 1.96), digits = 4) #CI lower 
  resultMain[,3] <- round(resultMain[,4] + (resultMain[,5] * 1.96), digits = 4) #CI upper
  
  
  # Beter checken!
  resultMain[,8] <- round(c(p.adjust(resultMain[ 1,7], method = "bonferroni", n = 2), # 2 groeps compared in learning
                            p.adjust(resultMain[ 2,7], method = "bonferroni", n = 2), # memory
                            p.adjust(resultMain[ 3,7], method = "bonferroni", n = 2), # neutral
                            p.adjust(resultMain[ 4,7], method = "bonferroni", n = 2), # stressful
                            p.adjust(resultMain[ 5,7], method = "bonferroni", n = 1), # l vs m (neutral) --> correction needed?
                            p.adjust(resultMain[ 6,7], method = "bonferroni", n = 1), # l vs m (stress)
                            resultMain[7,7],
                            resultMain[8,7],
                            resultMain[9,7],
                            resultMain[10,7]), digits = 4) #pvalue bonf correction
  
  
  
  # plot<-ggplot(resultMain[resultMain$test %in% c("nLearning", "nMemory", "sLearning", "sMemory"),], 
  #              aes(x = test, y = effectsize)) +
  #   ylab("Standardized mean difference (CI)") +
  #   ggtitle("Learning and Memory in PTSD",
  #           subtitle = title) +
  #   # theme_bw() +
  #   theme_classic() +
  #   geom_bar(stat = "identity", 
  #            fill = "light grey", 
  #            color = "black") + 
  #   geom_hline(yintercept = 0, size = 2) + 
  #   geom_errorbar(aes(ymin = ci.lb, 
  #                     ymax = ci.ub),
  #                 width = .6)
  
  
  plot_data<-resultMain %>% filter(test %in% c("nLearning", "nMemory", "sLearning", "sMemory"))
  plot_data$valence <- ifelse(plot_data$test %in% c("nLearning", "nMemory"), "Neutral", "Stressful")
  plot_data$phase <- ifelse(plot_data$test %in% c("nLearning", "sLearning"), "Learning", "Memory")
  
#str(plot_data)
# title='test'
  
   plot_MILOU <-
    ggplot(plot_data, 
               aes(x = test, y = effectsize, fill=valence)) +
     
    ylab("Standardized mean difference (CI)") +
    # ggtitle("Learning and Memory in PTSD",
    #         subtitle = title) +
     ggtitle( title) +
     
      facet_wrap(.~ phase, scales='free_x')+
      
 ylim(-3.1,3.3)+

     
    theme_classic() +
      scale_fill_manual(
        values= c( "Neutral" = "steelblue2", # Colours: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
                   "Stressful" = "tomato2"))+
     
    geom_bar(stat = "identity")+ 
      
    geom_hline(yintercept = 0, size = 2) + 
    geom_errorbar(aes(ymin = ci.lb, 
                      ymax = ci.ub),
                  width = .6)
  
  
  
  
  out<-list(resultMain,plot_MILOU)
  return(out)
}
