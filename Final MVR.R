# Core compositional & time-series packages
library(dplyr)            # data wrangling
library(tidyr)
library(lubridate)
library(compositions)     # acomp, ilr, variation, clr biplot
library(robCompositions)  # zero replacement
library(ggtern)           # ternary plots
library(vars)             # VAR modelling
library(urca)             # unit-root tests
library(tseries)          # ACF/PACF helpers
library(fable)            # forecasting in tidy framework
library(feasts)           # STL, ACF, decomposition
library(tibble)
library(forecast)
library(tsibble)          # tidy time index
library(future)           # optional parallel forecasting
library(zCompositions)
library(coda.plot)
library(broom)
library(purrr)
library(tictoc)
library(MuMIn)


df_raw <- read.csv("df_fuel_ckan.csv", stringsAsFactors = FALSE)

df <- df_raw %>% 
  # adjust to your header
  mutate(datetime = ymd_hms(DATETIME)) %>% 
  arrange(datetime) %>% 
  transmute(datetime,
            fossil   = GAS + COAL,
            renewable = WIND + HYDRO + SOLAR + BIOMASS,
            other     = NUCLEAR + IMPORTS + STORAGE + OTHER) %>% 
  mutate(total_mw = fossil + renewable + other,
         # now closed to 1
         across(fossil:other, ~ .x / total_mw)) 

df_parts <- dplyr::select(df, fossil:other)

V <- variation(acomp(df_parts))
aitch_var <- sum(V)/2

ILR_basis <- matrix(
  c(sqrt(2/3), 0,
    -sqrt(2/3)/2, sqrt(1/2),
    -sqrt(2/3)/2, -sqrt(1/2)),
  nrow = 3, byrow=TRUE,
  dimnames = list(c("fossil", "renewable", "other"),
                  c("b1", "b2"))
)

Z_ilr <- ilr(acomp(df_parts), V=ILR_basis)
df_ilr <- bind_cols(df["datetime"], as_tibble(Z_ilr, .name_repair = "unique"))
names(df_ilr)[2:3] <- c("z1", "z2")

df_ilr <- df_ilr |>
  mutate(t = row_number())

df_ts <- df_ilr %>% 
  as_tsibble(index = datetime) %>% 
  mutate(hour = yearmonth(datetime))    # or yearmonth for monthly analysis

# periods for seasonality
P <- c(day = 48, week = 336, year = 17520)
K <- c(day = 2, week = 2, year = 2)

# -- add this once to your session or a helper file -----------------
logLik.mlm <- function(object, ...) {
  E  <- residuals(object)         # n × m matrix of residuals
  n  <- nrow(E)                   # number of observations
  m  <- ncol(E)                   # number of responses
  Σ  <- crossprod(E) / n          # Σ̂  = E'E / n
  ll <- - n*m/2 * (1 + log(2*pi)) - n/2 * log(det(Σ))   # plug-in log-likelihood
  
  # count free parameters:   (m × p regression coefs)  +  (m(m+1)/2 in Σ)
  p  <- nrow(object$coefficients) * m + m*(m+1)/2
  
  attr(ll, "df") <- p            # required by AIC()
  attr(ll, "nobs") <- n
  class(ll)       <- "logLik"
  ll
}


fourier_multi <- function(t, P, K){
  bind_cols(lapply(names(P), \(tag){
    mats <- fourier(ts(rep(0, length(t)), frequency = P[tag]), K[tag])
    colnames(mats) <- paste0(tag, "_", colnames(mats))
    as_tibble(mats)
  }))
}

X_seas <- fourier_multi(df_ilr$t, P, K)
names(X_seas) <- make.names(names(X_seas), unique = TRUE)

df_mod <- bind_cols(df_ilr, as_tibble(X_seas)) |>
  mutate(t = row_number())

# Models - Baseline
fit0 <- lm(cbind(z1, z2) ~ 1, data=df_mod)
plot(fit0)
summary(fit0)

# # Linear Trend
fitT <- lm(cbind(z1, z2) ~ t, data=df_mod) 
summary(fitT)
plot(fitT)

# Purely Seasonal
fml_S <- reformulate(names(X_seas), response = "cbind(z1, z2)")
fitS <- lm(fml_S, data = df_mod)
summary(fitS)

fml_TS <- update(fml_S, . ~ . + t)
fitTS <- lm(fml_TS, data = df_mod)
BIC(fitTS)
summary(fitTS)

anova(fit0, fitT, fitS, fitTS)
AIC(fit0, fitT, fitS, fitTS)

# Extracting and tidying


models <- list(M0 = fit0, MT = fitT, MS = fitS, MTS = fitTS)
coef_tbl <- map_df(models, tidy, conf.int = TRUE, component = "response") |>
  dplyr::mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       t = "Time",
                       .default = term))

fit_tbl <- map_df(models, glance, .id = "model") |>
  dplyr::select(model, df.residual, r.squared, adj.r.squared,
                logLik, AIC, BIC)



res_df <- as_tibble(resid(fitTS)) %>%
  rename(r_z1 = z1, r_z2 = z2) %>%
  mutate(f_z1 = fitted(fitTS)[, 1],
         f_z2 = fitted(fitTS)[, 2]) %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "coord"),
               names_pattern = "(.+)_(z\\d)")

set.seed(42)
n_show <- 5000
res_sub <- res_df %>%
  slice_sample(n = n_show)

ggplot(res_sub, aes(x = f, y = r)) +
  geom_point(alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~coord, scales = "free") +
  labs(title = "Residuals vs fitted values (random 5000 obs)",
       x = "Fitted values", y = "Residuals")

ggplot(res_sub, aes(sample = r)) +
  stat_qq() +
  stat_qq_line() + 
  facet_wrap(~coord, scales = "free") +
  labs(title = "Normal Q-Q plots of residuals")

#####################
# --- 0.  prerequisites --------------------------------------------------------
library(tidyverse)
library(ggtern)     # devtools::install_github("drsimonj/ggtern")
library(MASS)       # for mvrnorm()
library(furrr)

# --- 1.  choose a scenario ----------------------------------------------------
# e.g.  14:00,  1 July 2024 (half-hour index `row_id`)

fit_z1 <- lm(z1 ~ t + ., data = df_mod[, c("z1", names(X_seas), "t")])
fit_z2 <- lm(z2 ~ t + ., data = df_mod[, c("z2", names(X_seas), "t")])

set.seed(123)
m <- 4999
n_obs <- nrow(df_mod)
tpl <- list(z1 = fit_z1, z2 = fit_z2)

# Helper
one_sim <- function(.seed) {
  set.seed(.seed)
  df_sim <- df_mod %>%
    mutate(
      z1 = simulate(tpl$z1, nsim = 1)[[1]],
      z2 = simulate(tpl$z2, nsim = 1)[[1]]
    )
  fit1  <- lm(z1 ~ t + ., data = df_sim[, c("z1", names(X_seas), "t")])
  fit2  <- lm(z2 ~ t + ., data = df_sim[, c("z2", names(X_seas), "t")])
  c(predict(fit1)[row_id],
    predict(fit2)[row_id])
}

plan(multisession, workers = availableCores())
b_boot <- future_map_dfr(1:m, one_sim, .progress = TRUE) |> as.matrix()
plan(sequential)                       

row_id <- which(df_mod$Datetime == as.POSIXct("2025-07-01 14:00:00"))

