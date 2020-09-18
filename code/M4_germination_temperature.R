### typha germination percent / temperature

load("input_data/typha_temp.RData")

png("Figures/Final_curves/Typha_germ_temp_mid_range.png", width = 700, height = 600)

ggplot(data = temp, aes(x = temp_midRng_c, y = germination_perc))+
  geom_jitter(alpha = 0.2, size = 3)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=25), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  labs(x = "Temperature (Celsius)", y = "Germination (%)")

dev.off()

head(temp)
mean(temp$temp_midRng_c) ## 17.7559
mean(temp$high_temperature_c) ## 22.24528

length(na.omit(temp$germination_perc)) ## 316

png("Figures/Final_curves/Typha_germ_temp_high.png", width = 700, height = 600)

ggplot(data = temp, aes(x = high_temperature_c, y = germination_perc))+
  geom_jitter(alpha = 0.2, size = 3)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  theme(text = element_text(size=25), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  labs(x = "Temperature (Celsius)", y = "Germination (%)")

dev.off()

summary(tmp_germ_mdl <- (lm(germination_perc ~ temperature_range_c + high_temperature_c + I(high_temperature_c^2), data = temp)))
save(tmp_germ_mdl, file = "tmp_germ_mdl.rda")

# png("Figures/Final_curves/Typha_germ_temp_mid_range.png", width = 700, height = 600)

ggplot(data = temp, aes(x = temperature_range_c, y = germination_perc))+
  geom_jitter(alpha = 0.2, size = 3)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))+
  scale_y_continuous(limits=c(0,100)) +
  scale_x_continuous(limits=c(0,30)) +
  theme(text = element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  labs(x = "Temperature (C)", y = "Germination (%)")

dev.off()

unique(temp$source[!is.na(temp$germination_perc)])