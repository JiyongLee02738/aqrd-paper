# Loading packages ----
library(haven)
library(tidyverse)
library(gt)
library(tibble)
library(rdd)
library(rdrobust)
library(janitor)
library(modelsummary)
library(ggpubr)
library(kableExtra)

# Loading datasets ----
primary_analysis <- read_dta("data/primary_analysis.dta")
# Races between an extremist and moderate 
primary_analysis_1 <- primary_analysis |> 
  filter(absdist > median(absdist)) # cutoff for ideological score 

# Summary statistics ----
treatment_summary <- primary_analysis |> 
  summarize(treatment_mean = mean(treat), 
            treatment_observations = n(),
            treatment_std_dev = sd(treat))

summary_stats <- primary_analysis |> 
  group_by(treat) |> 
  summarize(vote_share = mean(dv, na.rm=TRUE), # vote share 
            victory = mean(dv_win, na.rm = TRUE), # victory 
            party_share = mean(party_share, na.rm=TRUE), # share from party 
            total_party = mean(tot_amountY), # total from party 
            share_pac = mean(group_share), # share from PACs
            total_pac = mean(tot_amountQ), # total from PACs
            N = n() # # obs 
  ) |>  # treat is 0 when N is 259 (Moderate in General)
  mutate(N = round(N)) |> 
  select(-treat) 

summary_stats <- as.data.frame(summary_stats) |> 
  rename("Vote share" = vote_share,
         "Victory" = victory,
         "Share from party" = party_share,
         "Total from party" = total_party,
         "Share from PACs" = share_pac,
         "Total from PACs" = total_pac,
         "# Observations" = N) 
  #mutate(num(across(where(is.numeric)),sigfig = 2)) ##

# replicate summary stats table -- add standard deviations??

vec_sd <- c(sd(primary_analysis$dv),
            sd(primary_analysis$dv_win),
            sd(primary_analysis$party_share),
            sd(primary_analysis$tot_amountY),
            sd(primary_analysis$group_share),
            sd(primary_analysis$tot_amountQ),
            NA)

summary_stats_1 <- as.data.frame(t(summary_stats)) |> 
  rename("Moderate in General" = V1,
         "Extremist in General" = V2) |> 
  tibble::rownames_to_column(var = " ")  |> 
  mutate(SD = vec_sd)
         #N = c(489, 504, 413, 504, 496, 504, NA))

summary_stats_2 <- summary_stats_1 |> 
  gt() |> 
  fmt_number(rows = c(1:3, 5),
             decimals = 2) |> 
  fmt_number(rows = c(4, 6),
             decimals = 0)
#gtsave(summary_stats_2, "summary_stats_rep.png")

# Table 1 ----
table1_col1 <- primary_analysis |> 
  filter(safe_for_party == 1) |> 
  count(both_primaries_inc, this_primary_inc_other_open,
        this_primary_open_other_inc, fully_open_general) |> 
  select(n) |> 
  rename("Safe For Party" = n) 

table1_col2 <- primary_analysis |> 
  filter(competitive == 1) |> 
  count(both_primaries_inc, this_primary_inc_other_open,
        this_primary_open_other_inc, fully_open_general) |> 
  select(n) |> 
  rename("Competitive" = n) 

table1_col3 <- primary_analysis |> 
  filter(safe_for_party == 0 & competitive == 0) |> 
  count(both_primaries_inc, this_primary_inc_other_open,
        this_primary_open_other_inc, fully_open_general) |> 
  select(n) |> 
  rename("Safe For Other Party" = n) 

table1 <- bind_cols(table1_col1, table1_col2, table1_col3) |> 
  mutate("Incumbent Presence" = c("No incumbents (open seat)",
                                  "Open primary, incumbent in other party",
                                  "Incumbent in primary, other primary open",
                                  "Incumbents in both primaries")) |> 
  relocate("Incumbent Presence")

table1_total <- adorn_totals(table1, where = c("row", "col")) |> 
  gt() |> 
  fmt_number(decimals = 0)

#gtsave(table1_total, "summary_stats_rep_2.png")


# Figure 2 ----
primary_analysis_1 <- primary_analysis |> 
  filter(margin <= 0.2, absdist > 
           median(absdist)) |> # dist big 
  mutate(rv_int = cut_interval(rv, 
                               length = 0.02,
                               right = F))
#mutate(treat = as.factor(treat)))  

primary_analysis_2 <- primary_analysis_1 |> 
  group_by(rv_int) |> 
  summarize(mean_vote_share_bin = mean(dv))

primary_analysis_2 <- primary_analysis_1 |> 
  count(rv_int) |> 
  right_join(primary_analysis_2, by = "rv_int")

primary_analysis_2 <- primary_analysis_2 |> 
  mutate(rv_int = c(-10:9)/50) # as.numeric(rv_int)

# -0.2, 20 levels each is 0.02
ggplot(primary_analysis_1, aes(x = as.numeric(rv_int), 
                               y = mean_vote_share_bin)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = primary_analysis_2, size = 2) + 
  geom_smooth(method = "lm", 
              aes(x = rv, y = dv, group = treat),
              formula = y ~ x, se = FALSE) +
  geom_point(data = primary_analysis_1, aes(x = rv, y = dv), 
             alpha = 0.4, size = 1) +
  labs(x = "Extreme Candidate Primary Election Winning Margin",
       y = "General Election Vote Share",
       title = "") +
  annotate("text", x = 0.17, y = 0.4, label = "N=233") +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#ggsave("figure2.jpg")
# Figure 3
model1 <- lm(dv_win ~ treat + rv + rv2 + rv3, 
             data = primary_analysis |> 
               filter(absdist > median(absdist)))
model2 <- lm(dv_win ~ treat + rv + rv2 + rv3, 
             data = primary_analysis |> 
               filter(absdist > median(absdist),
                      fully_open_general == 1))
model3 <- lm(dv_win ~ treat + rv + rv2 + rv3, 
             data = primary_analysis |> 
               filter(absdist > median(absdist),
                      safe_for_party == 1))

list_m1 <- list("Overall" = model1, "Open Elections" = model2, 
                "Safe Districts For Party" = model3)
modelsummary(list_m1,
             gof_map = c("nobs"),
             coef_omit = "rv|rv_treat|Intercept",
             fmt_decimal = (pdigits = 2),
             coef_map = c("treat" = "Extremist win"),
             output = "tb.jpg") 
webshot::install_phantomjs()
install.packages("magick")

# Table 2 - Main regression ----
# outcome is vote share in general election
# column 1 -- bandwidth=5, local linear 
m1 <- lm(dv ~ treat + rv + rv_treat, data = primary_analysis |> 
           filter(absdist > median(absdist),
                  margin < 0.05)) # winning margin 
# column 2 -- cubic
m2 <- lm(dv ~ treat + rv + rv2 + rv3, data = primary_analysis |> 
           filter(absdist > median(absdist)))
# column 3 -- IK
m3 <- RDestimate(dv ~ rv, data = primary_analysis |> 
                   filter(absdist > median(absdist)))
est <- format(round(m3$est[1], 2), nsmall = 3)
se <- format(round(m3$se[1], 2), nsmall = 3)
obs <- format(round(m3$obs[1], 2), nsmall = 0)
#m3$bw[1]*100 # 8.51

# outcome is victory in general election 
# column 4 -- bandwidth=5, local linear 
m4 <- lm(dv_win ~ treat + rv + rv_treat, data = primary_analysis |> 
           filter(absdist > median(absdist),
                  margin < 0.05)) # winning margin 
# column 5 -- cubic
m5 <- lm(dv_win ~ treat + rv + rv2 + rv3, data = primary_analysis |> 
           filter(absdist > median(absdist)))
# Column 6 -- IK
m6 <- RDestimate(dv_win ~ rv, data = primary_analysis |> 
                   filter(absdist > median(absdist))) 
est1 <- format(round(m6$est[1], 2), nsmall = 3)
se1 <- format(round(m6$se[1], 2), nsmall = 3)
obs1 <- format(round(m6$obs[1], 2), nsmall = 0)
#m6$bw[1]*100 # 9.68

# create a modelsummary table 
summary_rows <- tribble(~ term, ~ m1, ~ m3, ~m2, 
                        ~ m4, ~ m6, ~ m5,
                        "RDD bandwidth", "5", "8.43", "-", 
                        "5", "9.68", "-",
                        "Specification", "Local linear","IK", "Cubic", 
                        "Local linear", "IK", "Cubic")

list_m <- list("General Election Vote Share" = m1,
               "General Election Vote Share" = m2,
               "General Election Victory" = m4, 
               "General Election Victory" = m5)
col3 <- c(est, se, obs)
col6 <- c(est1, se1, obs1)
cols <- data.frame(col3, col6) 
names(cols) <- c("General Election Vote Share  ",
                 "General Election Victory")

attr(cols, "position") <- c(3, 6)

modelsummary(list_m,
             gof_map = c("nobs"),
             coef_map = c("treat" = "Extremist win"),
             coef_omit = "rv|rv_treat|Intercept",
             add_rows = summary_rows,
             add_columns = cols,
             fmt = "%.2f") |> 
             footnote(general = "Standard errors are in parentheses. 
                      Column 2 and 5 are estimates using the IK bandwidth from rdd package in R.") # "%.2g"
# Notes: Standard errors are in parentheses. Column 5 and 6 are estimates using the IK bandwidth from rdd package in R.

# Figure 4 ----
# run lm(dv ~ treat + rv + rv2 + rv3) for obs with absdist > i
# voteshare effect 

dat <- data.frame(matrix(NA, nrow = 45, ncol = 5))
count <- 1

# absdist 0.45, 0.46
# WHY this specification

for (i in seq(0, 0.44, by = 0.01)){ #54 times
  mod <- try(lm(dv ~ treat + rv + rv2 + rv3, primary_analysis |> 
                  filter(absdist > i)), silent = TRUE) # distribution of absdist 
  if(!inherits(mod, "try-error")) {
    dat[count, 1] <- i
    dat[count, 2] <- coef(mod)["treat"]
    dat[count, 3] <- coef(mod)["treat"] - 1.96*summary(mod)$coefficients["treat", "Std. Error"]
    dat[count, 4] <- coef(mod)["treat"] + 1.96*summary(mod)$coefficients["treat", "Std. Error"]
    dat[count, 5] <- nobs(mod) # double check 
    
    count <- count + 1
  } 
}

cutoff_vote <- dat |> 
  rename(cutoff = X1,
         est = X2,
         lower = X3,
         upper = X4,
         n = X5) |> 
  filter(cutoff <= 0.39) # or 0.39

#data <- cutoff_vote
top_plot <- ggplot(aes(x = cutoff, y = est), data = cutoff_vote) +
  geom_point(size = 2) +
  geom_pointrange(aes(ymin = lower, ymax  = upper), color = "gray18") +
  theme(axis.ticks.x = element_blank()) +
  theme_classic() + # how can 0.4 show up here 
  scale_x_continuous(limits = c(0, 0.4)) +
  labs(y = "Estimated Effect on Vote Share") 

bottom_plot <- ggplot(aes(x = cutoff, y = n), data = cutoff_vote) +
  geom_segment(aes(xend = cutoff, yend = 0), 
               linewidth = 1.8, color = "gray14") + # xend
  theme_classic() +
  labs(y = "Sample Size", # axis titles 
       x = "Ideological Distance Cutoff") 

ggarrange(top_plot, bottom_plot, nrow = 2, ncol = 1)
ggsave("plot1.jpeg")

# Figure 5 ----
# victory effect 
# have obs for all values of absdist up to max(absdist)
dat1 <- data.frame(matrix(NA, nrow = 45, ncol = 5))
count1 <- 1

# cutoff 
# absdist

# why this specification
for (i in seq(0, 0.44, by = 0.01)){ #54 times
  mod1 <- try(lm(dv_win ~ treat + rv + rv2 + rv3, primary_analysis |> # same absdist
                   filter(absdist > i)), silent = TRUE) # distribution of absdist 
  if(!inherits(mod, "try-error")) {
    dat1[count1, 1] <- i
    dat1[count1, 2] <- coef(mod1)["treat"]
    dat1[count1, 3] <- coef(mod1)["treat"] - 1.96*summary(mod1)$coefficients["treat", "Std. Error"]
    dat1[count1, 4] <- coef(mod1)["treat"] + 1.96*summary(mod1)$coefficients["treat", "Std. Error"]
    dat1[count1, 5] <- nobs(mod1) # double check 
    
    count1 <- count1 + 1
  } 
}

cutoff_win <- dat1 |> 
  rename(cutoff = X1,
         est = X2,
         lower = X3,
         upper = X4,
         n = X5) |> 
  filter(cutoff <= 0.39) # or 0.39

#data <- cutoff_vote
top_plot <- ggplot(aes(x = cutoff, y = est), data = cutoff_win) +
  geom_point(size = 2) +
  geom_pointrange(aes(ymin = lower, ymax  = upper), color = "gray18") +
  theme(axis.ticks.x = element_blank()) +
  theme_classic() + # how can 0.4 show up here 
  scale_x_continuous(limits = c(0, 0.4)) +
  labs(y = "Estimated Effect on Victory") 

bottom_plot <- ggplot(aes(x = cutoff, y = n), data = cutoff_win) +
  geom_segment(aes(xend = cutoff, yend = 0), 
               linewidth = 1.8, color = "gray14") + # xend
  theme_classic() +
  labs(y = "Sample Size", # axis titles 
       x = "Ideological Distance Cutoff") 

ggarrange(top_plot, bottom_plot, nrow = 2, ncol = 1)
ggsave("plot2.jpg")

