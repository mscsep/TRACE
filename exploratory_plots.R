# Exploratory Plots CNS2019

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
