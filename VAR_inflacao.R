rm(list = ls())

install.packages('BETS')
install.packages('GetBCBData')
install.packages("ipeadatar")
library(rbcb)
library(dplyr)
library(GetBCBData)
library(BETS)
library(ipeadatar)
library(vars)


inflacao = gbcbd_get_series(433)
exp_inflacao = get_monthly_market_expectations('IPCA', start_date = '2000-01-01')
meta_inflacao = gbcbd_get_series(13521)

proxy_pib = BETSget(24364)
detach("package:BETS", unload = TRUE)

cambio = get_currency('USD', start = '2000-01-01', end_date = '2024-07-08')
selic = gbcbd_get_series(432)
swap = ipeadata(code = 'BMF12_SWAPDI360F12', language = 'br')

selic$ref.date <- as.Date(selic$ref.date)
selic$month <- format(selic$ref.date, "%Y-%m")
selic_mensal <- selic %>%
  group_by(month) %>%
  summarise(selic_media_mensal = mean(value, na.rm = TRUE)) %>%
  ungroup()

cambio$date <- as.Date(cambio$date)
cambio$month <- format(cambio$date, "%Y-%m")
cambio_mensal <- cambio %>%
  group_by(month) %>%
  summarise(cambio_media_mensal = mean(bid, na.rm = TRUE)) %>%
  ungroup()

cambio_mensal$month <- as.Date(paste0(cambio_mensal$month, "-01"))
start_year <- as.numeric(format(min(cambio_mensal$month), "%Y"))
start_month <- as.numeric(format(min(cambio_mensal$month), "%m"))
end_year <- as.numeric(format(max(cambio_mensal$month), "%Y"))
end_month <- as.numeric(format(max(cambio_mensal$month), "%m"))

start_month
cambio_ts <- ts(cambio_mensal$cambio_media_mensal, 
                start = c(2000, 1), 
                end = c(2024, 7), 
                frequency = 12)

plot(cambio_ts)



cambio_ts = ts(cambio_mensal$cambio_media_mensal, start = c(2014, 7), end = c(2024,7), frequency = 12)
selic_ts = ts(selic_mensal$selic_media_mensal, start = c(2014, 7), end = c(2024,7), frequency = 12)
ipca = ts(inflacao$value, start = c(2014, 7), end = c(2024,7), frequency = 12)
proxy = ts(proxy_pib$value, start = c(2014, 7), end = c(2024,7),frequency = 12)

autoplot(cbind(ipca,proxy,selic_ts,cambio_ts))

ts = cbind(ipca,proxy,selic_ts,cambio_ts)

lagselect = VARselect(ts, lag.max = 10, type = 'both')
lagselect$selection

VAR1 = VAR(ts, p = 4, type = 'both')
summary(VAR1)

predict(VAR1, n.ahead = 4, ci = 0.95)
plot(predict(VAR1, n.ahead = 6, ci = 0.95))
