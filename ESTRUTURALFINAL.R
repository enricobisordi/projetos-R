library(BETS)
library(ipeadatar)
library(dplyr)
library(janitor)
library(rbcb)
library(vars)
library(forecast)
library(lubridate)
library(mFilter)
library(rbcb)
library(tidyverse)
library(xts)
library(zoo)
library(readxl)
library(GetBCBData)
library(BETS)
library(lavaan)
library(tempdisagg)
library(lubridate)
library(semPlot)
library(stringr)
library(mFilter)
library(lavaan)
library(dynlm)
library(systemfit)
library(forecast)
library(bimets)
library(ipeadatar)
library(janitor)
library(vars)
library(forecast)

rm(list = ls())

acum_p <- function(data,n){
  factor <- (1+(data/100))
  prod <- RcppRoll::roll_prodr(factor, n=n)
  final <- (prod-1)*100
  
  return(final)
}

IPCA = BETSget(433, data.frame = TRUE) 
selic = ipeadata('GM366_TJOVER366', language = "br")
IBC = ipeadata('SGS12_IBCBRDESSAZ12', language = "br")
swap = ipeadata('BMF12_SWAPDI36012', language = "br")
ipca_exp <-
  get_monthly_market_expectations("IPCA") %>%
  clean_names() %>%
  dplyr::select(date, median) %>%
  arrange(date) %>%
  group_by(year = year(date), month = month(date)) %>%
  filter(date == first(date)) %>%
  ungroup() %>%
  distinct(date, .keep_all = TRUE) %>%
  mutate(date = floor_date(date, unit = "month")) %>%
  dplyr::select(date, median)

ipca_exp_12 <-
  get_market_expectations(
    type = "inflation-12-months",
    ind = "IPCA") %>%
  clean_names() %>%
  dplyr::select(date = data, ipca = mediana) %>%
  arrange(date) %>%
  group_by(year = year(date), month = month(date)) %>%
  filter(date == first(date)) %>%
  ungroup() %>%
  distinct(date, .keep_all = TRUE) %>%
  mutate(date = floor_date(date, unit = "month")) %>%
  dplyr::select(date, ipca)

selic <- selic %>%
  mutate(mes = format(date, "%Y-%m")) %>%
  group_by(mes) %>%
  summarise(selic_media = mean(value),
            date = first(date))

IBC <- subset(IBC, select = -tcode)
IBC <- subset(IBC, select = -uname)
IBC <- subset(IBC, select = -code)
swap <- subset(swap, select = -tcode)
swap <- subset(swap, select = -uname)
swap <- subset(swap, select = -code)
selic <- subset(selic, select = -mes)

IBC <- IBC %>%
  rename(IBC = value)
ipca_exp <- ipca_exp %>%
  rename(ipca_exp = median)
IPCA <- IPCA %>%
  rename(IPCA = value)
swap <- swap %>%
  rename(swap = value)
selic <- selic %>%
  rename(selic = selic_media)

IPCA <- IPCA %>%
  mutate(IPCA_lag = lag(IPCA, 1))

check_na <- function(df) {
  df %>%
    summarise_all(anyNA)
}


hp <- hpfilter(IBC$IBC, freq = 12)
IBC$tendencia <- hp$trend
IBC$hiato <- hp$cycle

rm(hp)

IPCA$ipcaanual <- acum_p(IPCA$IPCA,12)

IPCA <- IPCA %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-04-01"))

IBC <- IBC %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-04-01"))

ipca_exp <- ipca_exp %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-04-01"))

ipca_exp_12 <- ipca_exp_12 %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-04-01"))

selic <- selic %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-04-01"))

swap <- swap %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-04-01"))


selic$juro_real = ((1 + (selic$selic/100))/(1 + (IPCA$ipcaanual/100)) - 1)*100
selic$juro_real_lag = lag(selic$juro_real)

IBC$hiato_lag = lag(IBC$hiato)

#####

IPCA_estimar <- IPCA %>%
  filter(date >= as.Date("2023-04-03") & date <= as.Date("2024-04-01"))

IBC_estimar <- IBC %>%
  filter(date >= as.Date("2023-04-03") & date <= as.Date("2024-04-01"))

ipca_exp_estimar <- ipca_exp %>%
  filter(date >= as.Date("2023-04-03") & date <= as.Date("2024-04-01"))

ipca_exp_12_estimar <- ipca_exp_12 %>%
  filter(date >= as.Date("2023-04-03") & date <= as.Date("2024-04-01"))

selic_estimar <- selic %>%
  filter(date >= as.Date("2023-04-04") & date <= as.Date("2024-04-01"))

swap_estimar <- swap %>%
  filter(date >= as.Date("2023-04-03") & date <= as.Date("2024-04-01"))

#####

IPCA_base <- IPCA %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2023-04-01"))

IBC_base <- IBC %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2023-04-01"))

ipca_exp_base <- ipca_exp %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2023-04-01"))

ipca_exp_12_base <- ipca_exp_12 %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2023-04-01"))

selic_base <- selic %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2023-04-05"))

swap_base <- swap %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2023-04-01"))

#rm(IPCA, IBC, ipca_exp, ipca_exp_12, selic, swap)

check_na(IBC_base)
check_na(IPCA_base)
check_na(ipca_exp_base)
check_na(swap_base)
check_na(ipca_exp_12_base)
check_na(IBC_estimar)
check_na(IPCA_estimar)
check_na(ipca_exp_estimar)
check_na(swap_estimar)
check_na(ipca_exp_12_estimar)
check_na(selic_base)
check_na(selic_estimar)


merged_estimar <- merge(swap_estimar, IPCA_estimar, by = "date")
merged_estimar <- merge(merged_estimar, ipca_exp_estimar, by = "date")
merged_estimar <- merge(merged_estimar, ipca_exp_12_estimar, by = "date")
merged_estimar <- merged_estimar %>%
  mutate(month_year = format(date, "%Y-%m"))  # Criar coluna com formato ano-mês

selic_estimar <- selic_estimar %>%
  mutate(month_year = format(date, "%Y-%m"))

merged_estimar <- merge(merged_estimar, selic_estimar, by = "month_year")
IBC_estimar <- IBC_estimar %>%
  rename(date.x = date)
merged_estimar <- merge(merged_estimar, IBC_estimar, by = "date.x")

merged_base <- merge(swap_base, IPCA_base, by = "date")
merged_base <- merge(merged_base, ipca_exp_base, by = "date")
merged_base <- merge(merged_base, ipca_exp_12_base, by = "date")

merged_base <- merged_base %>%
  mutate(month_year = format(date, "%Y-%m"))  # Criar coluna com formato ano-mês

selic_base <- selic_base %>%
  mutate(month_year = format(date, "%Y-%m"))

merged_base <- merge(merged_base, selic_base, by = "month_year")
IBC_base <- IBC_base %>%
  rename(date.x = date)
merged_base <- merge(merged_base, IBC_base, by = "date.x")


phillips = merged_base$IPCA ~ merged_base$IPCA_lag + merged_base$hiato_lag + merged_base$ipca_exp
is = merged_base$hiato ~ merged_base$hiato_lag + merged_base$juro_real_lag
fischer = merged_base$juro_real ~ merged_base$ipca + merged_base$swap

system <- list(phillips = phillips, is = is, fischer = fischer)
fit_ols = systemfit(system, data = merged_base)
a = predict(fit_ols)

previsoes_ipca = a$phillips.pred
plot_inflacao <- data.frame(Observado = merged_base$IPCA, Previsao = previsoes_ipca)
plot_inflacao <- mutate(plot_inflacao, Observado_shifted = lag(Observado))
plot_inflacao<- tail(plot_inflacao, -1)
y_limits <- range(plot_inflacao$Observado, na.rm = TRUE) 
plot(plot_inflacao$Observado_shifted, type = "l", col = "blue", lwd = 2,
     ylim = y_limits,
     main = "Observado vs. Previsão de IPCA",
     xlab = "Tempo", ylab = "IPCA")
lines(plot_inflacao$Previsao, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Observado", "Previsto"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

previsoes_hiato = a$is.pred
plot_hiato <- data.frame(Observado = merged_base$hiato, Previsao = previsoes_hiato)
plot_hiato <- mutate(plot_hiato, Observado_shifted = lag(Observado))
plot_hiato <- tail(plot_hiato, -1)
y_limits <- range(plot_hiato$Observado, na.rm = TRUE) 
y_limits <- range(plot_hiato$Observado_shifted, na.rm = TRUE) 
plot(plot_hiato$Observado_shifted, type = "l", col = "blue", lwd = 2,
     ylim = y_limits,
     main = "Observado vs. Previsão de hiato",
     xlab = "Tempo", ylab = "Hiato")
lines(plot_hiato$Previsao, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Observado", "Previsto"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

previsoes_real = a$fischer.pred
plot_real <- data.frame(Observado = merged_base$juro_real, Previsao = previsoes_real)
plot_real <- mutate(plot_real, Observado_shifted = lag(Observado))
plot_real <- tail(plot_real, -1)
y_limits <- range(plot_real$Observado, na.rm = TRUE) 
y_limits <- range(plot_real$Observado, na.rm = TRUE) 
plot(plot_real$Observado, type = "l", col = "blue", lwd = 2,
     ylim = y_limits,
     main = "Observado vs. Previsão de juro real",
     xlab = "Tempo", ylab = "Selic")
lines(plot_real$Previsao, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Observado", "Previsto"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

check_na(merged_estimar)
predict(fit_ols, newdata = merged_estimar)

#ARIMA

IPCA_base_ts <- zoo(merged_base$IPCA, order.by = as.yearmon(merged_base$date.x))
ipca_exp_base_ts <- zoo(merged_base$ipca_exp, order.by = as.yearmon(merged_base$date.x))
swap_base_ts <- zoo(merged_base$swap, order.by = as.yearmon(merged_base$date.x))
ipca_exp_12_base_ts <- zoo(merged_base$ipca, order.by = as.yearmon(merged_base$date.x))
juro_real_base_ts <- zoo(merged_base$juro_real, order.by = as.yearmon(merged_base$date.x))
hiato_base_ts = zoo(merged_base$hiato, order.by = as.yearmon(merged_base$date.x))
IPCA_ts <- zoo(IPCA$IPCA, order.by = as.yearmon(IPCA$date))

modelo_autoarima <- auto.arima(IPCA_ts)
summary(modelo_autoarima)
plot(forecast(modelo_autoarima, h = 12))

# VAR

cbind = cbind(IPCA_base_ts, ipca_exp_base_ts, ipca_exp_12_base_ts, swap_base_ts,juro_real_base_ts,hiato_base_ts)
lag = VARselect(cbind, lag.max = 10)
lag$selection
VAR = VAR(cbind, p = 10, type = 'const', season = NULL)
forecast = predict(VAR, n.ahead = 12, ci = 0.9)
fanchart(forecast, name = 'IPCA_ts')



dados_base <- merged_base[, c("IPCA", "IPCA_lag", "swap", "ipca_exp", "ipca", "juro_real", "juro_real_lag", "hiato", "hiato_lag")]
dados_estimar <- merged_estimar[, c("IPCA", "IPCA_lag", "swap", "ipca_exp", "ipca", "juro_real", "juro_real_lag", "hiato", "hiato_lag")]
predict(fit_ols, newdata = dados_estimar)
