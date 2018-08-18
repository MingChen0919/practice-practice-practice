# install.packages('forecast')
# install.packages('fpp2')

library(forecast)
library(fpp2)


autoplot(AirPassengers)
head(arrivals)

autoplot(arrivals)
autoplot(arrivals, facets = TRUE)


autoplot(arrivals, facets = TRUE) +
  geom_smooth() +
  labs(title="International arrivals to Australia",
       y = "Arrivals (in thousands)",
       x = NULL)

japan <- arrivals[, "Japan"]
summary(japan)

autoplot(fpp2::qcement)

japan
