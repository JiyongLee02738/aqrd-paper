# Loading libraries ----
library(readr)
library(tidyverse)
library(haven)
library(gt)
library(tibble)
library(webshot2)
library(janitor)
library(ggpubr)
library(rdd)
library(kableExtra)
library(modelsummary)

# Read dataset ----
recipients <- read_csv("data/dime_recipients_1979_2020.csv")

# Cleaning ----
df_recipients_all <- recipients |> 
  filter(grepl('fd', election)) |>  # filtered for federal election
  filter(recipient.type == "cand") |> # candidate 
  filter(seat != "federal:senate") |> # house primary 
  select(-starts_with("nimsp")) |> 
  select(-c(party.orig,
            before.switch.ICPSR,
            after.switch.ICPSR,
            NID,
            FEC.ID,
            Cand.ID,
            ICPSR2,
            comtype,
            igcat,
            ind.exp.oppose,
            ind.exp.support,
            total.contribs.from.candidate, #
            total.party.contribs,
            total.pac.contribs,
            total.unitemized, #
            total.indiv.contribs,
            total.disbursements,
            total.receipts,
            num.givers.total,
            num.givers,
            seat,
            fecyear,
            recipient.cfscore.dyn,
            contributor.cfscore,
            #s.elec.stat,
            #r.elec.stat,
            bonica.cid,
            dwdime,
            dwnom1,
            recipient.type))

df1 <- df_recipients_all |> 
  filter(is.na(prim.vote.pct) == FALSE,
         prim.vote.pct != 1)
        
df1_winner <- df1 |> 
  filter(pwinner == "W")

df1_lost <- df1 |> 
  filter(pwinner == "L")

# Merging ----
test_1 <- df1_winner |> # for each obs to have two candidates in the same election
  full_join(df1_lost, by = c("distcyc", "party")) |> # district, year, party
  filter(!(is.na(name.y) == TRUE),
         !is.na(prim.vote.pct.x)==TRUE,
         !is.na(prim.vote.pct.y)==TRUE) # pwinner.x is all W and pwinner.y is all L

duplicates_1 <- test_1 |> 
  group_by(name.x, distcyc) |> # for each district and primary winner (W)
  filter(n() > 1) |> # identify election w/ more than 2 candidates
  ungroup() |>  
  mutate(margin_diff = prim.vote.pct.x - prim.vote.pct.y) |> # vote share of W - vote share of L
  group_by(name.x, distcyc) |> 
  slice_min(margin_diff, n=1) |> # identify top 2 candidates -> smallest margin_diff
  select(-margin_diff) # to match # columns with test_1

no_duplicates <- setdiff(test_1, duplicates_1) 

df_final <- rbind(no_duplicates, duplicates_1) |> # CHECK
  mutate(ideo_diff = abs(recipient.cfscore.x - recipient.cfscore.y)) |> 
  filter(ideo_diff > median(ideo_diff)) |> #check dist of ideo_diff
  # keep elections between extreme and moderate candidates 
  # extreme candidate, absolute value of ideology score is higher (CHECK)
  mutate(prim.vote.share.y = prim.vote.pct.y / 100,
         prim.vote.share.x = prim.vote.pct.x / 100) |> 
  mutate(extreme.x = ifelse(abs(recipient.cfscore.x) > abs(recipient.cfscore.y),
                          1,0), # CHECK
         treat = ifelse(pwinner.x == "W" & extreme.x == 1, 1, 0)) |> 
  mutate(rv = ifelse(extreme.x == 1, 
                     (prim.vote.share.x / (prim.vote.share.x + prim.vote.share.y)) - 0.5,
                     (prim.vote.share.y / (prim.vote.share.x + prim.vote.share.y)) - 0.5), # winning margin of extreme cand
         rv2 = rv^2,
         rv3 = rv^3,
         rv4 = rv^4,
         rv_treat = ifelse(treat == 1, rv, 0),
         margin = abs(rv), # absolute value of rv
         gen.voteshare.x = gen.vote.pct.x / 100,
         gen.voteshare.y = gen.vote.pct.y / 100,
         dv = ifelse(is.na(gen.voteshare.x) == FALSE, 
                     gen.voteshare.x, 
                     gen.voteshare.y),
         dv_win = ifelse(gwinner.x == "W", 1, 0)) |>
  mutate(rv = ifelse(treat == 1 & rv < 0, NA, rv),
         rv = ifelse(treat == 0 & rv > 0, NA, rv)) |> 
  filter(is.na(rv)==FALSE) 

# Summary Stats ----
# TABLES 
treatment_summary_2_1 <- df_final |> 
  summarize(treatment_mean = mean(treat), 
            treatment_observations = n(),
            treatment_std_dev = sd(treat))

summary_stats_2_1 <- df_final |> 
  group_by(treat) |> 
  summarize(vote_share = mean(dv, na.rm=TRUE), # vote share 
            victory = mean(dv_win, na.rm = TRUE), # victory 
            ideological_score = mean(recipient.cfscore.x, na.rm=TRUE), # share from party 
            N = n()) |> # obs 
  select(-treat)

summary_stats_2_1 <- as.data.frame(summary_stats_2_1) |> 
  rename("Vote share" = vote_share,
         "Victory" = victory,
         "Ideological Score" = ideological_score,
         "# Observations" = N) 

vec_sd_2 <- c(sd(df_final$dv, na.rm=TRUE), # gen.voteshare.x
            sd(df_final$dv_win, na.rm=TRUE),
            sd(df_final$recipient.cfscore.x, na.rm=TRUE),
            NA)

summary_stats_2_2 <- as.data.frame(t(summary_stats_2_1)) |> 
  rename("Moderate in General" = V1,
         "Extremist in General" = V2) |> 
  tibble::rownames_to_column(var = " ")  |> 
  mutate(SD = vec_sd_2) 
#N = c(489, 504, 413, 504, 496, 504, NA))

summary_stats_2_3 <- summary_stats_2_2 |> 
  gt() |> 
  fmt_number(rows = c(1:3),
             decimals = 2) |> 
  fmt_number(rows = c(4),
             decimals = 0) 
#gtsave(summary_stats_2_3, "summary_stats_rep.png")


# Figure 2 ----
# ideo_diff is absdist and margin is margin 

# df_final$ideo_diff
df_final_v1 <- df_final |> # 0.2??
  filter(margin <= 0.2, ideo_diff > median(ideo_diff)) |> # dist big 
  mutate(rv_int = cut_interval(rv, 
                               length = 0.02,
                               right = F)) # ?? # averages of margin, not general election voteshare?

#summary(df_final_v1$rv_int) # why exactly from -0.2 to 0.2

df_final_v2 <- df_final_v1 |> 
  group_by(rv_int) |>
  summarize(mean_vote_share_bin = mean(dv, na.rm = TRUE)) # voteshare in general election
# instead of gen.voteshare.x, dv
df_final_v2 <- df_final_v1 |> 
  count(rv_int) |> 
  right_join(df_final_v2, by = "rv_int")

df_final_v2 <- df_final_v2 |> 
  mutate(rv_int = c(-10:9)/50) # as.numeric(rv_int)

# -0.2, 20 levels each is 0.02

ggplot(df_final_v1, aes(x = as.numeric(rv_int), y = mean_vote_share_bin)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(data = df_final_v2, size = 2) + # each point race, not candidates?
  geom_smooth(method = "lm", 
              aes(x = rv, y = dv, group = treat), 
              formula = y ~ x, se = FALSE) +
  geom_point(data = df_final_v1, aes(x = rv, y = dv), alpha = 0.4, size = 1) +
  labs(x = "Extreme Candidate Primary Election Winning Margin",
       y = "General Election Vote Share",
       title = "") +
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  annotate("text", x = 0.17, y = 0.15, label = "N=652")  # N=652

df_final_yr <- df_final |> 
  filter(cycle.x >= 1980, 
         cycle.x <= 2010)

# Years 1980 - 2010 ----
# Column 1 -- dv, bandwidth=5, local linear 
m1 <- lm(dv ~ treat + rv + rv_treat, data = df_final_yr |> 
           filter(ideo_diff > median(ideo_diff),
                  margin < 0.05)) # winning margin 
# Column 2 -- dv, bandwidth=_, cubic
m2 <- lm(dv ~ treat + rv + rv2 + rv3, data = df_final_yr |> 
             filter(ideo_diff > median(ideo_diff))) 
# Column 3 -- dv, IK bandwidth 
m3 <- RDestimate(dv ~ rv, data = df_final_yr |> 
                     filter(ideo_diff > median(ideo_diff)) )

est <- format(round(m3$est[1], 2), nsmall = 3)
se <- format(round(m3$se[1], 2), nsmall = 3)
obs <- format(round(m3$obs[1], 2), nsmall = 0)

# Outcome is victory in general election 
# Column 4 -- dv_win, bandwidth=5, local linear 
m4 <- lm(dv_win ~ treat + rv + rv_treat, data = df_final_yr |> 
           filter(ideo_diff > median(ideo_diff),
                  margin < 0.05)) # winning margin 

# Column 5 -- dv_win, cubic
m5 <- lm(dv_win ~ treat + rv + rv2 + rv3, data = df_final_yr |> 
           filter(ideo_diff > median(ideo_diff))) 
# Column 6 -- dv_win, IK
m6 <- RDestimate(dv_win ~ rv, data = df_final_yr |> 
                     filter(ideo_diff > median(ideo_diff))) 

est1 <- format(round(m6$est[1], 2), nsmall = 3)
se1 <- format(round(m6$se[1], 2), nsmall = 3)
obs1 <- format(round(m6$obs[1], 2), nsmall = 0)

summary_rows <- tribble(~ term, ~ m1, ~ m3, ~m2, 
                        ~ m4, ~ m6, ~ m5,
                        "RDD bandwidth", "5", "1.99", "-", 
                        "5", "2.81", "-",
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

# All Years ----
# Main regression 
# Column 1 -- dv, bandwidth=5, local linear 
m1 <- lm(dv ~ treat + rv + rv_treat, data = df_final |> 
           filter(ideo_diff > median(ideo_diff),
                  margin < 0.05)) # winning margin 

# Column 2 -- dv, bandwidth=_, cubic
m2 <- lm(dv ~ treat + rv + rv2 + rv3, data = df_final  |> 
           filter(ideo_diff > median(ideo_diff)))

# Column 3 -- dv, bandwidth IK
m3 <- RDestimate(dv ~ rv, data = df_final  |> 
                   filter(ideo_diff > median(ideo_diff))) 

est <- format(round(m3$est[1], 2), nsmall = 3)
se <- format(round(m3$se[1], 2), nsmall = 3)
obs <- format(round(m3$obs[1], 2), nsmall = 0)

# outcome is victory in general election ----
# column 4 -- dv_win, bandwidth=5, local linear 
m4 <- lm(dv_win ~ treat + rv + rv_treat, data = df_final |> 
           filter(ideo_diff > median(ideo_diff),
                  margin < 0.05)) # winning margin 

# Column 5 -- dv_win, bandwidth=_, cubic
m5 <- lm(dv_win ~ treat + rv + rv2 + rv3, data = df_final |>
           filter(ideo_diff > median(ideo_diff)))

# Column 6 -- dv, bandwidth=9.68, IK
m6 <- RDestimate(dv_win ~ rv, data = df_final |>
                   filter(ideo_diff > median(ideo_diff)))

est1 <- format(round(m6$est[1], 2), nsmall = 3)
se1 <- format(round(m6$se[1], 2), nsmall = 3)
obs1 <- format(round(m6$obs[1], 2), nsmall = 0)

summary_rows <- tribble(~ term, ~ m1, ~ m3, ~m2, 
                        ~ m4, ~ m6, ~ m5,
                        "RDD bandwidth", "5", "1.81", "-", 
                        "5", "2.28", "-",
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

