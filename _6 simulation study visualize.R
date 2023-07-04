# load data
load("C:/Users/lionb/Downloads/study_scenarios.RData")
study_scenarios$pred <- rnorm(nrow(study_scenarios), 
                              mean = mean(study_scenarios$pred, na.rm=T),
                              sd = sd(study_scenarios$pred, na.rm=T)
                              )
study_scenarios$sd <- rnorm(nrow(study_scenarios), 
                              mean = mean(study_scenarios$sd, na.rm=T),
                              sd = sd(study_scenarios$sd, na.rm=T)
)
study_scenarios$perc_fraudedAll <- rnorm(nrow(study_scenarios), 
                            mean = mean(study_scenarios$perc_fraudedAll, na.rm=T),
                            sd = sd(study_scenarios$perc_fraudedAll, na.rm=T)
)

# prepare data
study_scenarios$bias <- study_scenarios$perc_fraudedAll - study_scenarios$pred
study_scenarios$f_inc <- str_c("f_inc = ", study_scenarios$f_inc)
study_scenarios$f_ext <- str_c("f_ext = ", study_scenarios$f_ext)
p_base <- ggplot(study_scenarios[study_scenarios$fraud_type!="clean",], aes(x=n, y=bias, group=fraud_type, color=fraud_type)) + 
  geom_errorbar(aes(ymin=bias-sd, ymax=bias+sd), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point() + ylim(-0.05, 0.05) +
  ylab("Bias") + xlab("Number of Electoral Units") +
  scale_color_brewer(palette="Paired", 
                     name = "Fraud Type", labels = c("Adding", "Removing", "Switching")) + 
  theme_minimal(base_size = 18)
bias_fraud <- p_base + facet_grid(vars(f_inc), vars(f_ext))

bias_clean <- ggplot(study_scenarios[study_scenarios$fraud_type=="clean",], aes(x=n, y=bias, group=fraud_type, color=fraud_type)) + 
  geom_errorbar(aes(ymin=bias-sd, ymax=bias+sd), width=.5, 
                position=position_dodge(0.05)) +
  geom_line(size=1.3) + geom_point(size=1.3) + ylim(-0.05, 0.05) +
  ylab("Bias") + xlab("Number of Electoral Units") +
  scale_color_brewer(palette="Dark2", direction = 1,
                     name = "Fraud Type", labels = c("Clean")) + 
  theme_minimal(base_size = 20)
