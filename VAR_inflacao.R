rm(list = ls())

install.packages('BETS')
install.packages('GetBCBData')
install.packages("ipeadatar")
library(GetBCBData)
library(BETS)
library(ipeadatar)
library(vars)
detach("package:BETS", unload = TRUE)


inflacao = gbcbd_get_series(433)
exp_inflacao = get_monthly_market_expectations('IPCA', start_date = '2000-01-01')
meta_inflacao = gbcbd_get_series(13521)
proxy_pib = BETSget(24364)
cambio = get_currency('USD', start = '2000-01-01', end_date = '2024-07-08')
selic = gbcbd_get_series(432)
swap = ipeadata(code = 'BMF12_SWAPDI360F12', language = 'br')

selic$ref.date <- as.Date(selic$ref.date)

selic$month <- format(selic$ref.date, "%Y-%m")

selic_mensal <- selic %>%
  group_by(month) %>%
  summarise(selic_ultimo_valor = last(value)) %>%
  ungroup()

selic <- merge(selic, selic_mensal, by = "month")

selic_ts = ts(selic_mensal$selic_ultimo_valor, start = c(2014, 7), end = c(2024,7), frequency = 12)
ipca = ts(inflacao$value, start = c(2014, 7), end = c(2024,7), frequency = 12)
proxy = ts(proxy_pib$value, start = c(2014, 7), end = c(2024,7),frequency = 12)

autoplot(cbind(ipca,proxy,selic_ts))

ts = cbind(ipca,proxy,selic_ts)

lagselect = VARselect(ts, lag.max = 10, type = 'both')
lagselect$selection

VAR1 = VAR(ts, p = 4, type = 'both')
summary(VAR1)

predict(VAR1, n.ahead = 4, ci = 0.95)
plot(predict(VAR1, n.ahead = 6, ci = 0.95))
