# pacotes necessarios

library(tidyverse)
theme_set(theme_void())
library(lubridate)

# importando os dados
# fonte: https://data.world/sportsvizsunday/june-2020-nba-shots-1997-2019

nba <- read_csv(file = "data/NBAShotLocations19972020.csv")

# limpando os nomes das colunas

nba <- janitor::clean_names(nba)

# criando a variavel temporada

nba <- nba %>%
	mutate(game_date = ymd(game_date))

nba <- nba %>% 
	mutate(season = case_when(game_date >= ymd("1997-09-30") & game_date <= ymd("1998-09-01") ~ "1997-98",
														game_date >= ymd("1998-09-30") & game_date <= ymd("1999-09-01") ~ "1998-99",
														game_date >= ymd("1999-09-30") & game_date <= ymd("2000-09-01") ~ "1999-00",
														game_date >= ymd("2000-09-30") & game_date <= ymd("2001-09-01") ~ "2000-01",
														game_date >= ymd("2001-09-30") & game_date <= ymd("2002-09-01") ~ "2001-02",
														game_date >= ymd("2002-09-30") & game_date <= ymd("2003-09-01") ~ "2002-03",
														game_date >= ymd("2003-09-30") & game_date <= ymd("2004-09-01") ~ "2003-04",
														game_date >= ymd("2004-09-30") & game_date <= ymd("2005-09-01") ~ "2004-05",
														game_date >= ymd("2005-09-30") & game_date <= ymd("2006-09-01") ~ "2005-06",
														game_date >= ymd("2006-09-30") & game_date <= ymd("2007-09-01") ~ "2006-07",
														game_date >= ymd("2007-09-30") & game_date <= ymd("2008-09-01") ~ "2007-08",
														game_date >= ymd("2008-09-30") & game_date <= ymd("2009-09-01") ~ "2008-09",
														game_date >= ymd("2009-09-30") & game_date <= ymd("2010-09-01") ~ "2009-10",
														game_date >= ymd("2010-09-30") & game_date <= ymd("2011-09-01") ~ "2010-11",
														game_date >= ymd("2011-09-30") & game_date <= ymd("2012-09-01") ~ "2011-12",
														game_date >= ymd("2012-09-30") & game_date <= ymd("2013-09-01") ~ "2012-13",
														game_date >= ymd("2013-09-30") & game_date <= ymd("2014-09-01") ~ "2013-14",
														game_date >= ymd("2014-09-30") & game_date <= ymd("2015-09-01") ~ "2014-15",
														game_date >= ymd("2015-09-30") & game_date <= ymd("2016-09-01") ~ "2015-16",
														game_date >= ymd("2016-09-30") & game_date <= ymd("2017-09-01") ~ "2016-17",
														game_date >= ymd("2017-09-30") & game_date <= ymd("2018-09-01") ~ "2017-18",
														game_date >= ymd("2018-09-30") & game_date <= ymd("2019-09-01") ~ "2018-19",
														game_date >= ymd("2019-09-30") & game_date <= ymd("2020-09-01") ~ "2019-20"
														))

# calculando o numero de arremessos por localizacao

nba_tally <- nba %>%
	group_by(x_location, y_location) %>%
	count()

# plotando de forma discreta

ggplot(nba_tally, aes(x = x_location, y = y_location, fill = n)) +
	geom_tile() +
	coord_equal() +
	theme(legend.position = "none") +
	scale_fill_viridis_c()
	
# mapa de calor
	
nba_tally <- nba_tally %>%
	arrange(desc(n))

head(nba_tally)

# retirando os tres outliers

nba_tally <- tail(nba_tally, -3)

# mapa de calor

ggplot(nba_tally, aes(x = x_location, y = y_location, fill = log(n))) +
	geom_tile() +
	coord_equal() +
	theme(legend.position = "none") +
	scale_fill_viridis_c()

# mapa de calor por ano

nba_tally_season <- nba %>%
	group_by(x_location, y_location, season) %>%
	count()

nba_tally_season %>%
	group_by(season) %>%
	top_n(n = 5) %>%
	arrange(desc(n)) %>%
	print(n = Inf)

# retirar as duas primeiras e a ultima temporada

'%ni%' <- Negate('%in%')

nba_tally_season <- nba_tally_season %>%
	filter(!(x_location == 0 && y_location == 0)) %>%
	filter(season %ni% c("1997-98", "1998-99", "2019-20")) %>%
	group_by(season) %>%
	mutate(n_normalized = n/max(n))

ggplot(nba_tally_season, aes(x = x_location, y = y_location, fill = log(1000*n_normalized))) +
	geom_tile() +
	coord_equal() +
	theme(legend.position = "none") +
	facet_wrap(~ season) +
	scale_fill_viridis_c() +
	theme(strip.background = element_rect(fill = "grey"),
				panel.spacing = unit(0, "lines"),
				panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
