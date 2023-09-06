
## Precept 2 figures code
library(tidyverse)
library(reshape2)
library(ggExtra)
library(gridExtra)
library(grid)

setwd("/Users/ianlundberg/Dropbox/SOC504/2018 materials/Precept/precept2/figs")

log.lik <- function(pi,y) {
  (log(pi) - log(1 - pi)) * sum(y) + 5 * length(y) * log(1 - pi)
}


samples <- data.frame(u = runif(100000)) %>%
  mutate(z = qnorm(u))

addSamp <- function(i) {
  pdf(paste0("UOfUSims/sim",i,".pdf"), onefile=FALSE,
      height = 5, width = 7)
  
  p <- ggplot(mutate(data.frame(u=seq(.001,.999,.001)),
                  z=qnorm(u)),
           aes(x = z, y = u)) +
    geom_line(color = "blue", size = 2) +
    scale_x_continuous(name=bquote('\nZ ~ '*F^{-1}*'(p) ~ Normal(0,1)'),
                       limits = qnorm(c(.001,.999))) + 
    scale_y_continuous(name="U ~ F(Z) ~ Uniform(0,1)\n",
                       limits = 0:1) +
    geom_segment(data = samples[i,],
                 aes(x=z, xend=z,
                     y=0, yend=u),
             size=1.5, linetype="F1", color = "seagreen") +
    geom_segment(data = samples[i,],
                 aes(x=qnorm(.001), xend=z,
                     y=u, yend=u),
                 size=1.5, linetype="F1", color = "seagreen") +
    theme(text=element_text(size=16)) +
    geom_line(data = samples[1:i,],
              aes(x = z, y = u),
              alpha = 0) +
    ggtitle(paste(prettyNum(i, big.mark = ","),
                  ifelse(i == 1, "draw","draws")))
  hU <- ggplot(samples[1:i,],
               aes(x = u)) +
    geom_histogram(bins = 30, fill = "seagreen") +
    scale_x_continuous(name = NULL, breaks = NULL,
                       limits = c(0,1)) +
    scale_y_continuous(name = NULL, breaks = NULL,
                       trans = "reverse") +
    coord_flip() +
    theme(panel.background = element_blank())
  
  hZ <- ggplot(samples[1:i,],
               aes(x = z)) +
    geom_histogram(bins = 30, fill = "seagreen") +
    scale_x_continuous(name = NULL, breaks = NULL,
                       limits = qnorm(c(.001,.999))) +
    scale_y_continuous(name = NULL, breaks = NULL,
                       trans = "reverse") +
    theme(panel.background = element_blank())
  
  grobs <- lapply(list(hU,p,hZ),ggplotGrob)
  grobs[[1]]$heights <- grobs[[2]]$heights
  grobs[[3]]$widths <- grobs[[2]]$widths
  
  grid.arrange(grobs = grobs,
               layout_matrix = rbind(c(1,2),
                                     c(NA,3)),
               widths = c(.3,2),
               heights = c(2,.3))
  dev.off()
}
for (i in c(1:25,100,500,1000,10000)) {
  addSamp(i)
}
  
  
optimize.out <- optimize(f = log.lik,
                         interval = c(0,1),
                         maximum = T,
                         y = y)

set.seed(08544)
y <- rbinom(100,5,.2)
data.frame(pi = seq(0.1,0.3,0.01)) %>%
  mutate(`Log likelihood` = log.lik(pi,y)) %>%
  ggplot(aes(x = pi, y = `Log likelihood`)) +
  geom_line() +
  scale_x_continuous(name = expression(pi)) +
  ggsave("LogLik1.pdf",
         height = 2, width = 5)

g <- function(pi) {
  1 - (1 - pi) ^ 5
}

data.frame(pi = seq(0.01,0.99,0.01)) %>%
  mutate(g = g(pi))  %>%
  ggplot(aes(x = pi, y = g)) +
  geom_line(size = 1.2) +
  xlab(expression(paste(pi == {},"P(Paper gets an R&R)"))) +
  ylab(expression(atop(g(pi) == 1 - (1 - pi) ^ 5,
                       "P(At least 1 of 5 papers gets an R&R)"))) +
  theme(axis.title.x = element_text(color = "blue"),
        axis.text.x = element_text(color = "blue"),
        axis.title.y = element_text(color = "seagreen", angle = 0, vjust = .5, hjust = .5),
        axis.text.y = element_text(color = "seagreen")) +
  xlim(c(0,1)) + ylim(c(0,1)) +
  coord_fixed() +
  ggsave("g_pi.pdf",
         height = 3, width = 7)


data.frame(pi = seq(0.1,0.3,0.01)) %>%
  mutate(loglik = log.lik(pi,y),
         g_pi = g(pi)) %>%
  melt(id = "loglik") %>%
  #mutate(variable = gsub("g_pi","g(pi) == 1 - (1 - pi) ^ 5",variable)) %>%
  ggplot(aes(x = value, y = loglik,
             color = variable)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("seagreen","blue")) +
  geom_vline(data = data.frame(variable = c("pi","g_pi"),
                               xintercept = c(optimize.out$maximum,
                                              g(optimize.out$maximum))),
             aes(xintercept = xintercept, color = variable),
             linetype = 2, size = 1.2) +
  geom_text(data = data.frame(x = c(.23,.545),
                              y = c(-255,-255),
                              variable = c("pi","g_pi"),
                              label = c(
                                paste("hat(pi)[MLE] ==",
                                      round(optimize.out$maximum,3)),
                                paste("atop(g(hat(pi)[MLE]) == 1 - (1 - ",
                                      round(optimize.out$maximum,3),
                                      ") ^ 5, {} == {",
                                      round(g(optimize.out$maximum),3),
                                      " == widehat(g(pi))[MLE]})")
                              )),
            aes(x = x, y = y, label = label, color = variable),
            parse = T, fontface = "bold") +
  geom_text(data = data.frame(x = c(0.3, 0.45),
                              y = -241.5,
                              variable = c("pi","g_pi"),
                              label = c("P(R&R on a given\nsubmission)",
                                        "P(At least 1 R&R out of\n 5 submissions)")),
            aes(x = x, y = y, label = label, color = variable),
            fontface = "bold") +
  geom_text(data = data.frame(x = c(0.3, 0.45),
                              y = -244,
                              variable = c("pi","g_pi"),
                              label = c("{}==pi",
                                        "{}=={g(pi)==1-(1-pi)^5}")),
            aes(x = x, y = y, label = label, color = variable),
            parse = T, fontface = "bold") +
  xlab("Probability") +
  ylab("Log likelihood") +
  theme(legend.position = "none") +
  ggsave("LogLik_both.pdf",
         height = 3, width = 10)

  
  ## OLD
  data.frame(pi = seq(0.1,0.3,0.01)) %>%
    mutate(`Log likelihood` = log.lik(pi,y)) %>%
    ggplot(aes(x = pi, y = `Log likelihood`)) +
    geom_vline(xintercept = optimize.out$maximum,
               linetype = 2,
               color = "blue") +
    annotate(geom = "text",
             x = 0.22, y = -250,
             label = paste("hat(pi)[MLE] ==",
                           round(optimize.out$maximum,3)),
             parse = T,
             color = "blue") +
    geom_line(color = "seagreen",
              size = 1.2) +
    scale_x_continuous(name = expression(pi),
                       limits = c(0,1)) +
    ggsave("LogLik_pi.pdf",
           height = 3, width = 10)
  
  data.frame(pi = seq(0.1,0.3,0.01)) %>%
    mutate(`Log likelihood` = log.lik(pi,y)) %>%
    ggplot(aes(x = g(pi), y = `Log likelihood`)) +
    geom_vline(xintercept = g(optimize.out$maximum),
               linetype = 2,
               color = "blue") +
    annotate(geom = "text",
             x = 0.545, y = -255,
             label = paste("atop(g(hat(pi)[MLE]) == 1 - (1 - ",
                           round(optimize.out$maximum,3),
                           ") ^ 5, {} == {",
                           round(g(optimize.out$maximum),3),
                           " == widehat(g(pi))[MLE]})"),
             parse = T,
             color = "blue") +
    geom_line(color = "seagreen",
              size = 1.2) +
    scale_x_continuous(name = expression(g(pi) == 1 - (1 - pi) ^ 5),
                       limits = c(0,1)) +
    ggsave("LogLik_g_pi.pdf",
           height = 3, width = 10)

