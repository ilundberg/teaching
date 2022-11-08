
library(tidyverse)
library(foreach)

theme_set(theme_bw())

# Numbers are from Card & Krueger 1994 Table 3 Rows 1 and 2
d <- data.frame(Employment = c(23.33,20.44,21.17,21.03),
                State = c("PA","NJ","PA","NJ"),
                Time = c(-1,-1,1,1)) %>%
  mutate(State = fct_rev(State))

diffs <- d %>%
  group_by(State) %>%
  summarize(diff = diff(Employment))

d_counterfactual <- d %>%
  filter(Time == -1 & State == "NJ") %>%
  mutate(Employment = Employment + diffs$diff[diffs$State == "PA"]) %>%
  mutate(Time = 1)

p1 <- d %>%
  ggplot(aes(x = Time, y = Employment, color = State)) +
  ylim(c(17.5,24)) +
  geom_point(size = 3, alpha = 0) +
  scale_x_continuous(breaks = c(-1,0,1),
                     labels = c("Before","INTERVENTION\n","After")) +
  ylab("Employment Per Store") + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  scale_color_discrete(labels = c("PA (Never treated)","NJ (Becomes treated)"))

p2 <- p1 +
  geom_line(data = d %>% filter(State == "PA"))

p3 <- p2 + 
  geom_line(data = d %>%
              filter(State == "NJ" & Time == -1) %>%
              bind_rows(d_counterfactual),
            linetype = "dashed") +
  geom_point(data = d_counterfactual,
             shape = 1, size = 3) +
  geom_text(data = d_counterfactual,
            label = "Counterfactual\nNJ",
            hjust = 1, vjust = 1, x = .95,
            show.legend = F)
p4 <- p3 +
  annotate(geom = "text", color = "gray", x = -.6, y = 21.5,
           label = "Parallel Trends\nAssumption") +
  annotate(geom = "segment", color = "gray", x = -.6, y = 22,
           xend = -.5, yend = 22.5, arrow = arrow(length = unit(.1,"in"))) +
  annotate(geom = "segment", color = "gray", x = -.6, y = 21,
           xend = -.5, yend = 20.5, arrow = arrow(length = unit(.1,"in")))

p5 <- p4 +
  geom_line(data = d %>%
              filter(State == "NJ" & Time == 1) %>%
              bind_rows(d_counterfactual),
            linetype = "dashed") +
  geom_text(data = d %>%
              filter(State == "NJ" & Time == 1) %>%
              bind_rows(d_counterfactual)%>%
              group_by(State) %>%
              summarize_all(.funs = mean),
            hjust = 1, label = "Estimated\nCausal\nEffect",
            x = .95, show.legend = F)

ggsave(plot = p1, filename = "ck_slide1.pdf",
       height = 4, width = 5)
ggsave(plot = p2, filename = "ck_slide2.pdf",
       height = 4, width = 5)
ggsave(plot = p3, filename = "ck_slide3.pdf",
       height = 4, width = 5)
ggsave(plot = p4, filename = "ck_slide4.pdf",
       height = 4, width = 5)
ggsave(plot = p5, filename = "ck_slide5.pdf",
       height = 4, width = 5)

# Parallel trends more credible
foreach(i = 1:5, .combine = "rbind") %do% {
  d %>%
    filter(Time == -1) %>%
    mutate(Time = Time - i,
           Employment = Employment - i*.5*diffs$diff[diffs$State == "PA"])
} %>%
  bind_rows(d) %>%
  ggplot(aes(x = Time, y = Employment, color = State)) +
  geom_point(size = 3, alpha = 0) +
  geom_line(data = d %>%
              filter(State == "NJ" & Time == -1) %>%
              bind_rows(d_counterfactual),
            linetype = "dashed") +
  geom_point(data = d_counterfactual,
             shape = 1, size = 3) +
  geom_line() +
  scale_x_continuous(breaks = c(0),
                     labels = c("INTERVENTION\n")) +
  ylab("Employment Per Store") + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  scale_color_discrete(labels = c("PA (Never treated)","NJ (Becomes treated)"))

ggsave("parallel_trends_credible.pdf",
       height = 3.5, width = 5)

# Parallel trends not credible
p <- foreach(i = 1:5, .combine = "rbind") %do% {
  d %>%
    filter(Time == -1) %>%
    mutate(Time = Time - i,
           Employment = case_when(State == "PA" ~ Employment - i*.5*diffs$diff[diffs$State == "PA"],
                                  State == "NJ" ~ Employment - i*.2*diffs$diff[diffs$State == "PA"]))
} %>%
  bind_rows(d) %>%
  ggplot(aes(x = Time, y = Employment, color = State, alpha = State)) +
  geom_point(size = 3, alpha = 0) +
  geom_line() +
  scale_x_continuous(breaks = c(0),
                     labels = c("INTERVENTION\n")) +
  ylab("Employment Per Store") + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  scale_color_discrete(labels = c("PA (Never treated)","NJ (Becomes treated)")) +
  scale_alpha_manual(values = c(1,1), guide = "none")

p +
  geom_line(data = d %>%
              filter(State == "NJ" & Time == -1) %>%
              bind_rows(d_counterfactual),
            linetype = "dashed") +
  geom_point(data = d_counterfactual,
             shape = 1, size = 3)
ggsave(filename = "parallel_trends_doubtful.pdf",
       height = 3.5, width = 5)

p  +
  geom_line(data = d %>%
              filter(State == "NJ" & Time == -1) %>%
              bind_rows(d_counterfactual),
            linetype = "dashed") +
  geom_point(data = d_counterfactual,
             shape = 1, size = 3) +
  scale_alpha_manual(values = c(0,1), guide = "none")
ggsave(filename = "parallel_trends_doubtful_2.pdf",
       height = 3.5, width = 5)

p  +
  geom_line(data = d %>%
              filter(State == "NJ" & Time == -1) %>%
              bind_rows(d_counterfactual),
            alpha = 0,
            linetype = "dashed") +
  geom_point(data = d_counterfactual,
             alpha = 0,
             shape = 1, size = 3) +
  geom_line(data = d %>%
              filter(State == "NJ" & Time == -1) %>%
              bind_rows(d %>%
                          filter(State == "NJ" & Time == -1) %>%
                          mutate(Time = 1,
                                 Employment = Employment + .4*diffs$diff[diffs$State == "PA"])),
            linetype = "dashed") +
  geom_point(data = d %>%
               filter(State == "NJ" & Time == -1) %>%
               mutate(Time = 1,
                      Employment = Employment + .4*diffs$diff[diffs$State == "PA"]),
             shape = 1, size = 3) +
  scale_alpha_manual(values = c(0,1), guide = "none")
ggsave(filename = "parallel_trends_doubtful_3.pdf",
       height = 3.5, width = 5)

p  +
  ylim(c(18.5,23.5)) +
  geom_line(data = d %>%
              filter(State == "NJ" & Time == -1) %>%
              bind_rows(d %>%
                          filter(State == "NJ" & Time == -1) %>%
                          mutate(Time = 1,
                                 Employment = Employment + .4*diffs$diff[diffs$State == "PA"])),
            linetype = "dashed") +
  geom_point(data = d %>%
               filter(State == "NJ" & Time == -1) %>%
               mutate(Time = 1,
                      Employment = Employment + .4*diffs$diff[diffs$State == "PA"]),
             shape = 1, size = 3) +
  scale_alpha_manual(values = c(0,1), guide = "none") +
  theme(legend.position = "none")
ggsave(filename = "parallel_trends_doubtful_4.pdf",
       height = 3, width = 5)

# When ITS fails

d <- data.frame(Time = -10:10) %>%
  filter(Time != 0) %>%
  mutate(Y0 = rnorm(n(), mean = - (Time + 4) ^ 2 / 10 + 2, sd = .1), #mean = log(Time + 13)
         Y1 = rnorm(n(), mean = 4, sd = .1),
         A = case_when(Time > 0 ~ "Treated",
                       Time < 0 ~ "Untreated"),
         Y = ifelse(A == "Treated", Y1, Y0))

fit <- lm(Y ~ poly(Time,2), data = d %>% filter(A == "Untreated"))
fitted <- data.frame(Time = seq(-10,10,.1),
                     A = "Untreated")
fitted$Y <- predict(fit, newdata = fitted)
fitted$se <- (predict(fit, newdata = fitted, se = T))$se.fit

p1 <- d %>%
  ggplot(aes(x = Time, y = Y, color = A)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ylim(c(-17.5,5)) +
  geom_point() +
  scale_x_continuous(breaks = 0, labels = "INTERVENTION\n") +
  scale_color_discrete(name = element_blank()) +
  ylab("Outcome")
p2 <- p1 +
  geom_line(data = fitted %>% filter(Time < 0))
p3 <- p1 +
  geom_line(data = fitted)

p4 <- p3 +
  annotate(geom = "text", x = 1.5, y = 1.6, hjust = 0, label = "Credible\nEstimate",
           size = 3, fontface = "bold", color = "darkgray") +
  annotate(geom = "segment", x = 1, xend = 1, y = .5, yend = 3.5,
           color = "darkgray", arrow = arrow(length = unit(.1,"in"))) +
  annotate(geom = "segment", x = 1, xend = 1, y = 3.5, yend = .5,
           color = "darkgray", arrow = arrow(length = unit(.1,"in")))
p5 <- p4 +
  annotate(geom = "text", x = 9.5, y = 1.6, hjust = 1, label = "Doubtful\nEstimate",
           size = 3, fontface = "bold", color = "darkgray") +
  annotate(geom = "segment", x = 10, xend = 10, y = -15, yend = 3.5,
           color = "darkgray", arrow = arrow(length = unit(.1,"in"))) +
  annotate(geom = "segment", x = 10, xend = 10, y = 3.5, yend = -15,
           color = "darkgray", arrow = arrow(length = unit(.1,"in")))
ggsave(filename = "its_problem_1.pdf",
       plot = p1,
       height = 3.7, width = 5)
ggsave(filename = "its_problem_2.pdf",
       plot = p2,
       height = 3.7, width = 5)
ggsave(filename = "its_problem_3.pdf",
       plot = p3,
       height = 3.7, width = 5)
ggsave(filename = "its_problem_4.pdf",
       plot = p4,
       height = 3.7, width = 5)
ggsave(filename = "its_problem_5.pdf",
       plot = p5,
       height = 3.7, width = 5)



