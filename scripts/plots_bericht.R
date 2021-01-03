library(forecast)
library(stats)
library(ggplot2)
set.seed(78)
y <- arima.sim(list(order = c(1,0,1), ar=0.5, ma=0.5), n=100)
pdf("C:/Users/Laura/Documents/Uni/Fallstudien II/Projekt 1/Bericht/Plots/beispiel_acf.pdf", height=6, width=6)
plt <- ggAcf(y) + 
  ggtitle("") + 
  ylab(expression(hat(rho))) + 
  theme_grey(base_size = 22)
print(plt)
dev.off()
pdf("C:/Users/Laura/Documents/Uni/Fallstudien II/Projekt 1/Bericht/Plots/beispiel_pacf.pdf", height=6, width=6)
plt <- ggPacf(y) + 
  ggtitle("") + 
  ylab(expression(hat(psi))) + 
  theme_grey(base_size = 22)
print(plt)
dev.off()
