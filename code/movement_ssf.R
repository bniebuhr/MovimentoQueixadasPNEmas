#' ---
#' title: 'White-lipped peccary movement in PN Emas - resource and step selection functions'
#' author: Bernardo Niebuhr, Ennio Painkow, Ronaldo Morato - CENAP/ICMBio
#' 
#' abstract: "In this document we read land use data and perform habitat selection analyses
#' for four white-lipped peccaries (**Tayassu pecari**) moving around the Emas National Park,
#' in the Brazilian Cerrado."
#' ---

#' # Loading movement and background data
#' 
#' First of all, we load the data that was already organized and cleaned in another step.
#' We're working here with data from four white-lipped peccary individuals 
#' (from four different groups) from the Parque Nacional
#' das Emmas, within the Brazilian Cerrado.
#' We also load land use maps (that will be used here for the habitat selection analysis), 
#' rasterize and merge them in two rasters: one with land use classes, and another with
#' the date when the sugarcane and corn plantations were croped.

# --------------- label=load_packages, warning=FALSE, message=FALSE, echo=FALSE

# Load packages
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load('ezknitr', 'knitr')

# Print options for this document
opts_knit$set(root.dir = '..') # root project folder
# opts_chunk$set(error = F, message = F, warning = F, cache = F, echo = T, results = T)

# --------------- label=setup, warning=FALSE, message=FALSE
# Set up 

# Clean everything before beginning
rm(list = ls())

# Load packages
install.load::install_load('tidyverse', 'ggridges', 'lubridate')
install.load::install_load('sf', 'fasterize', 'raster')
install.load::install_load('broom', 'bbmle')
install.load::install_load('amt')

# --------------- label=load_data

#' ## Load movement data
#' 

# Load movement data
load('data/movement_data_loaded.RData')

#' ## Load spatial data
#' 

# land use classes
lu.classes <- c('Campo Seco', 'Campo Úmido', 'Cana-de-Açúcar',
                'Cerrado', 'Mata de Galeria', 'Milho - Safrinha', 
                'Solo Exposto', 'Não Disponível - Vala')

# alto formoso
map.alto.formoso <- sf::st_read(dsn = 'maps/Alto_Formoso', layer = 'alto_formoso') %>% 
  dplyr::rename(uso_terra = CLASS_D_19) %>% 
  dplyr::mutate(data.colheita = lubridate::ymd(DATA_COLHE) + days(1),
                julian = julian(data.colheita),
                name = 'Alto Formoso',
                classe = factor(uso_terra, levels = lu.classes),
                classe.num = factor(classe, levels = lu.classes, labels = 1:length(lu.classes)) %>% as.numeric) %>% 
  dplyr::select(-DATA_COLHE)

# Check                
map.alto.formoso$uso_terra
length(levels(map.alto.formoso$uso_terra))
length(levels(map.alto.formoso$classe))
map.alto.formoso$data.colheita[!is.na(map.alto.formoso$data.colheita)]
map.alto.formoso$julian[!is.na(map.alto.formoso$julian)]
str(map.alto.formoso)

# olhos leste
map.olhos.leste <- sf::st_read(dsn = 'maps/Olhos_Leste', layer = 'Olhos_leste') %>% 
  dplyr::rename(uso_terra = CLASS_C_19) %>% 
  dplyr::mutate(data.colheita = lubridate::ymd(DATA_COLHE) + days(1),
                julian = julian(data.colheita),
                name = 'Olhos Leste',
                classe = factor(uso_terra, levels = lu.classes),
                classe.num = factor(classe, levels = lu.classes, labels = 1:length(lu.classes)) %>% as.numeric)

# Check                
map.olhos.leste$uso_terra
length(levels(map.olhos.leste$uso_terra))
length(levels(map.olhos.leste$classe))
map.olhos.leste$data.colheita[!is.na(map.olhos.leste$data.colheita)]
str(map.olhos.leste)

# olhos oeste
map.olhos.oeste <- sf::st_read(dsn = 'maps/Olhos_Oeste', layer = 'olhos_oeste') %>% 
  dplyr::rename(uso_terra = CLASS_B_19) %>% 
  dplyr::mutate(data.colheita = lubridate::ymd(NA) + days(1),
                julian = julian(data.colheita),
                name = 'Olhos Oeste',
                classe = factor(uso_terra, levels = lu.classes),
                classe.num = factor(classe, levels = lu.classes, labels = 1:length(lu.classes)) %>% as.numeric)

# Check                
map.olhos.oeste$uso_terra
length(levels(map.olhos.oeste$uso_terra))
length(levels(map.olhos.oeste$classe))
map.olhos.oeste$data.colheita[!is.na(map.olhos.oeste$data.colheita)]
str(map.olhos.oeste)

# pontal
map.pontal <- sf::st_read(dsn = 'maps/Pontal', layer = 'pontal') %>% 
  dplyr::rename(uso_terra = CLASS_A_19) %>% 
  dplyr::mutate(data.colheita = lubridate::ymd(DATA_COLHE) + days(1),
                julian = julian(data.colheita),
                name = 'Pontal',
                classe = factor(uso_terra, levels = lu.classes),
                classe.num = factor(classe, levels = lu.classes, labels = 1:length(lu.classes)) %>% as.numeric)

# Check                
map.pontal$uso_terra
length(levels(map.pontal$uso_terra))
length(levels(map.pontal$classe))
map.pontal$data.colheita[!is.na(map.pontal$data.colheita)]
str(map.pontal)

# rasterize

# alto formoso
r.af <- raster(map.alto.formoso, res = 10)
rast.alto.formoso.class <- fasterize::fasterize(sf = map.alto.formoso, raster = r.af, field = 'classe.num')
raster::plot(rast.alto.formoso.class)
rast.alto.formoso.julian <- fasterize::fasterize(sf = map.alto.formoso, raster = r.af, field = 'julian')
raster::plot(rast.alto.formoso.julian)

# olhos leste
r.ol <- raster(map.olhos.leste, res = 10)
rast.olhos.leste.class <- fasterize::fasterize(sf = map.olhos.leste, raster = r.ol, field = 'classe.num')
raster::plot(rast.olhos.leste.class)
rast.olhos.leste.julian <- fasterize::fasterize(sf = map.olhos.leste, raster = r.ol, field = 'julian')
raster::plot(rast.olhos.leste.julian)

# olhos oeste
r.oo <- raster(map.olhos.oeste, res = 10)
rast.olhos.oeste.class <- fasterize::fasterize(sf = map.olhos.oeste, raster = r.oo, field = 'classe.num')
raster::plot(rast.olhos.oeste.class)
rast.olhos.oeste.julian <- fasterize::fasterize(sf = map.olhos.oeste, raster = r.oo, field = 'julian')
raster::plot(rast.olhos.oeste.julian)

# pontal
r.po <- raster(map.pontal, res = 10)
rast.pontal.class <- fasterize::fasterize(sf = map.pontal, raster = r.po, field = 'classe.num')
raster::plot(rast.pontal.class)
rast.pontal.julian <- fasterize::fasterize(sf = map.pontal, raster = r.po, field = 'julian')
raster::plot(rast.pontal.julian)

# merge raster
origin(rast.alto.formoso.class)
origin(rast.olhos.oeste.class)
origin(rast.olhos.leste.class)
origin(rast.pontal.class)

# class
rast.classes <- raster::merge(rast.alto.formoso.class, rast.olhos.oeste.class, 
                              rast.olhos.leste.class, rast.pontal.class, tolerance = 4)
raster::plot(rast.classes)

# julian
rast.julian <- raster::merge(rast.alto.formoso.julian, rast.olhos.oeste.julian, 
                             rast.olhos.leste.julian, rast.pontal.julian, tolerance = 4)
raster::plot(rast.julian)

# maps
maps <- raster::stack(rast.classes, rast.julian)
names(maps) <- c('classes', 'julian.date')
raster::plot(maps)

#' # Step-selection functions for a single individual
#' 
#' Now we perform a step selection analysis for a single individual, just to get some 
#' understanding of how it is responding to the habitat covariates. Then we do the same
#' for all individuals 

# --------------- label=ssf_one_individual

coord.system <- as.character(maps@crs)

# select individual
mov.track.1 <- mov.track %>% 
  amt::transform_coords(crs_to = coord.system) %>% 
  dplyr::filter(name == 'Alto Formoso')

# check regularity
mov.track.1 %>% 
  amt::summarize_sampling_rate()

# resample
stps <- mov.track.1 %>% 
  amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>% 
  amt::filter_min_n_burst(min_n = 3) %>% 
  amt::steps_by_burst()

# explore covariates
eda1 <- stps %>% 
  extract_covariates(maps, where = "start") %>% 
  mutate(classes.lev = factor(classes, levels = 1:length(lu.classes), labels = lu.classes),
         dttm.harvest = ymd('1970-01-01') + days(julian.date),
         classes.corr = ifelse((classes.lev == lu.classes[3]) & (t1_ > dttm.harvest) & (t1_ < dttm.harvest + days(3*30)) & !is.na(dttm.harvest), 
                               lu.classes[7], as.character(classes.lev)),
         classes.corr = as.factor(ifelse((classes.corr == lu.classes[6]) & (t1_ > dttm.harvest) & !is.na(dttm.harvest), 
                                         lu.classes[7], as.character(classes.corr))),
         classes.corr = relevel(classes.corr, lu.classes[4])) %>% 
  dplyr::filter(!is.na(classes.corr)) 

###
## Possibility: classificar o milho de maneira diferente apos a colheita; pode ser uma nova classe (durante um mes, e solo exposto apos)

# Some plots to explore

# step length
eda1 %>% 
  ggplot() +
  geom_boxplot(aes(x = classes.corr, y = sl_))

g1 <- ggplot(data = eda1, aes(x = sl_, y = classes.corr, fill = classes.corr)) +
  ggridges::geom_density_ridges(alpha = 0.5) +
  theme_bw() +
  labs(x = '30-min Step length (km)', y = '',
       title = 'Distance traveled in 30 min') +
  theme(legend.position = "none")
g1

# turning angle
eda1 %>% 
  ggplot() +
  geom_boxplot(aes(x = classes.corr, y = ta_))
g2 <- ggplot(data = eda1, aes(x = ta_ * 180 / pi, y = classes.corr, fill = classes.corr)) +
  ggridges::geom_density_ridges(alpha = 0.5) +
  theme_bw() +
  labs(x = '30-min Step length (km)', y = '',
       title = 'Turning angle') +
  theme(legend.position = "none")
g2

#------------------------
# ssf

# create random steps and extracting land use covariates
d1 <- stps %>% 
  amt::random_steps(n = 9) %>% 
  extract_covariates(maps, where = "end") %>% 
  mutate(classes.lev = factor(classes, levels = 1:length(lu.classes), labels = lu.classes),
         dttm.harvest = ymd('1970-01-01') + days(julian.date),
         classes.corr = ifelse((classes.lev == lu.classes[3]) & (t2_ > dttm.harvest) & (t2_ < dttm.harvest + days(3*30)) & !is.na(dttm.harvest), 
                                         lu.classes[7], as.character(classes.lev)),
         classes.corr = as.factor(ifelse((classes.corr == lu.classes[6]) & (t2_ > dttm.harvest) & !is.na(dttm.harvest), 
                                         lu.classes[7], as.character(classes.corr))),
         classes.corr = relevel(classes.corr, lu.classes[4])) %>% 
  dplyr::filter(!is.na(classes.corr)) %>%
  mutate(log_sl_ = log(sl_))

# plot
par(mfrow = c(2,1))
# used
plot(maps$classes)
points(stps$x2_, stps$y2_, pch = 19, col = grey(0.1, 0.5))
plot(maps$classes)
points(d1$x2_, d1$y2_, pch = 19, col = grey(0.1, 0.5))
par(mfrow = c(1,1))

# ckeck steps that go outside the range area
which(is.na(d1$x2_) | is.na(d1$y2_))
# no one, in this case!

# check if dates and classes are correctly classified
d1 %>% 
  dplyr::filter(!is.na(julian.date)) %>% 
  .[5500:6500,] %>% 
  print(width = Inf, n = 1000)

d1 %>% 
  dplyr::filter(!is.na(julian.date)) %>% 
  .[6500:7500,] %>% 
  print(width = Inf, n = 1000)

d1 %>% 
  dplyr::filter(as.character(classes.lev) != as.character(classes.corr)) %>% 
  print(n = 1000)

# run models
m1 <- d1 %>% 
  amt::fit_clogit(case_ ~ classes.corr + strata(step_id_))
m1

# m2 <- d1 %>% 
#   amt::fit_clogit(case_ ~ classes.corr + log_sl_ + strata(step_id_))
# m2

#install.load::install_load('ResourceSelection')
# m1 <- ResourceSelection::rspf(case_ ~ classes.corr, data =  d1, m = ifelse(d1$case_, 1, 2))
# m1

AIC(m1$model)
summary(m1)
s <- summary(m1$model)$coefficients
s
print(knitr::kable(s, digits = 4))

# plot
broom::tidy(m1$model) %>% 
  dplyr::mutate(term = stringr::str_remove(term, "classes.corr"))

m1.plot <- broom::tidy(m1$model) %>% 
  dplyr::mutate(term = stringr::str_remove(term, "classes.corr")) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Relative Selection Strength", x = "") +
  theme_light() + 
  coord_flip()
m1.plot

# m2.plot <- broom::tidy(m2$model) %>% 
#   ggplot(aes(x = term, y = estimate)) +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
#                   position = position_dodge(width = 0.7), size = 0.8) +
#   geom_hline(yintercept = 0, lty = 2) +
#   labs(y = "Relative Selection Strength") +
#   theme_light() + 
#   coord_flip()
# m2.plot


# --------------- label=all_inds

# maps coordinate system
coord.system <- as.character(maps@crs)

# calculate sampling rate statstics for all individuals
mov.track %>% 
  tidyr::nest(-name) %>% 
  dplyr::mutate(samp_rate = map(data, function(x) amt::summarize_sampling_rate(x))) %>% 
  dplyr::select(name, samp_rate) %>% 
  tidyr::unnest()

#------------
# rarefy and calculate exploratory analysis on used step length and angle vs land use class

mov.track.lu <- mov.track %>% 
  tidyr::nest(-name) %>% 
  dplyr::mutate(trk = map(data, function(d) {
    d %>% 
      amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>% 
      amt::transform_coords(crs_to = coord.system) %>% 
      amt::steps_by_burst() %>% 
      amt::extract_covariates(maps, where = "start") %>%
      dplyr::mutate(classes.lev = factor(classes, levels = 1:length(lu.classes), labels = lu.classes),
                    dttm.harvest = ymd('1970-01-01') + days(julian.date),
                    classes.corr = ifelse((classes.lev == lu.classes[3]) & (t2_ > dttm.harvest) & (t2_ < dttm.harvest + days(3*30)) & !is.na(dttm.harvest), 
                                          lu.classes[7], as.character(classes.lev)),
                    classes.corr = as.factor(ifelse((classes.corr == lu.classes[6]) & (t2_ > dttm.harvest) & !is.na(dttm.harvest), 
                                                    lu.classes[7], as.character(classes.corr))),
                    classes.corr = relevel(classes.corr, lu.classes[4])) %>% 
      dplyr::filter(!is.na(classes.corr), classes.corr != "Não Disponível - Vala") %>%
      dplyr::mutate(log_sl_ = log(sl_))
  }),
  trk = map(trk, function(d) d %>% dplyr::mutate(classes.corr = factor(classes.corr, levels = sort(unique(classes.corr)))))
  ) %>% 
  dplyr::select(-data) %>% 
  unnest(cols = trk)

# plot step length

# general boxplot
mov.track.lu %>% 
  ggplot() +
  geom_boxplot(aes(x = classes.corr, y = sl_))

# density plot for each individual
g1.sl <- mov.track.lu %>% 
  ggplot(aes(x = sl_, y = classes.corr, fill = classes.corr)) +
  ggridges::geom_density_ridges(alpha = 0.5) +
  facet_wrap(~name) +
  theme_bw() +
  labs(x = '30-min Step length (km)', y = '',
       title = 'Distance traveled in 30 min') +
  theme(legend.position = "none")
g1.sl

# calculating averages and confidence intervals for each individual
# and for the whole population
mov.track.lu.summ <- mov.track.lu %>% 
  group_by(name, classes.corr) %>% 
  summarise(n = n(),
            avg.sl = mean(sl_, na.rm = T),
            sd.sl = sd(sl_, na.rm = T),
            CI.sl.lower = avg.sl - 1.96 * sd(sl_, na.rm = T)/sqrt(n-1),
            CI.sl.higher = avg.sl + 1.96 * sd(sl_, na.rm = T)/sqrt(n-1),
            avg.ta = mean(ta_, na.rm = T),
            sd.ta = sd(ta_, na.rm = T),
            CI.ta.lower = avg.ta - 1.96 * sd(ta_, na.rm = T)/sqrt(n-1),
            CI.ta.higher = avg.ta + 1.96 * sd(ta_, na.rm = T)/sqrt(n-1))

pop.lu.summ <- mov.track.lu %>% 
  group_by(classes.corr) %>% 
  summarise(n = n(),
            avg.sl = mean(sl_, na.rm = T),
            sd.sl = sd(sl_, na.rm = T),
            CI.sl.lower = avg.sl - 1.96 * sd(sl_, na.rm = T)/sqrt(n-1),
            CI.sl.higher = avg.sl + 1.96 * sd(sl_, na.rm = T)/sqrt(n-1),
            avg.ta = mean(ta_, na.rm = T),
            sd.ta = sd(ta_, na.rm = T),
            CI.ta.lower = avg.ta - 1.96 * sd(ta_, na.rm = T)/sqrt(n-1),
            CI.ta.higher = avg.ta + 1.96 * sd(ta_, na.rm = T)/sqrt(n-1)) %>% 
  dplyr::mutate(x = 1:n())

# if we used weighted average
# pop.lu.summ.weighted <- mov.track.lu.summ %>% 
#   group_by(classes.corr) %>% 
#   summarise(n.ind = n(),
#             avg.sl.pop = weighted.mean(avg.sl, w = c(1,2,3,4), na.rm = T),
#             CI.sl.lower = avg.sl.pop - 1.96 * sd(avg.sl, na.rm = T)/sqrt(n.ind-1),
#             CI.sl.higher = avg.sl.pop + 1.96 * sd(avg.sl, na.rm = T)/sqrt(n.ind-1),
#             avg.ta.pop = mean(avg.ta, na.rm = T),
#             CI.ta.lower = avg.ta.pop - 1.96 * sd(avg.ta, na.rm = T)/sqrt(n.ind-1),
#             CI.ta.higher = avg.ta.pop + 1.96 * sd(avg.ta, na.rm = T)/sqrt(n.ind-1)) %>% 
#   dplyr::mutate(x = 1:n())

# plot all estimates in a single plot
g2.sl <- mov.track.lu.summ %>% 
  ggplot(., aes(x = as.numeric(classes.corr), y = avg.sl, group = name, col = name), ) + 
  geom_rect(mapping = aes(xmin = x - .4, xmax = x + .4, ymin = CI.sl.lower,
                          ymax = CI.sl.higher), data = pop.lu.summ, inherit.aes = FALSE, fill = "grey90") + 
  geom_segment(mapping = aes(x = x - .4, xend = x + .4,
                             y = avg.sl, yend = avg.sl), data = pop.lu.summ, inherit.aes = FALSE, size = 1) + 
  geom_pointrange(aes( ymin = CI.sl.lower, ymax = CI.sl.higher),
                  position = position_dodge(width = 0.7), size = 0.8) +
  geom_text(data = pop.lu.summ, aes(x = x, label = classes.corr), y = -100, inherit.aes = FALSE) +
  labs(x = '', y = 'Step length (m)',
       col = 'Group',
       title = 'Distance traveled in 30 min') + 
  ylim(0, NA) + 
  scale_x_discrete(breaks = pop.lu.summ$x, label = rep("", length(pop.lu.summ$x))) +
  coord_flip(clip = "off") +
  theme_light() + 
  theme(plot.margin = unit(c(1,1,1,6), "lines"))
g2.sl

ggsave(filename = "sl_one_figure.png", plot = g2.sl, path = "results", 
       width = 20, height = 17, units = "cm", dpi = 300)

# plot only populational step length values
g3.sl.pop <- pop.lu.summ %>% 
  ggplot(aes(x = classes.corr, y = avg.sl)) +
  geom_pointrange(aes(ymin = CI.sl.lower, ymax = CI.sl.higher),
                  position = position_dodge(width = 0.7), size = 0.8) +
  labs(x = "", y = "Step length (m)",
       title = "Population level distance traveled in 30 min") +
  theme_light() + 
  coord_flip()
g3.sl.pop

# plot step length for each individual
g3.sl.ind <- mov.track.lu.summ %>% 
  ggplot(aes(x = classes.corr, y = avg.sl)) +
  geom_pointrange(aes(ymin = CI.sl.lower, ymax = CI.sl.higher),
                  position = position_dodge(width = 0.7), size = 0.8) +
  facet_wrap(~name) +
  labs(x = "", y = "Step length (m)",
       title = "Group level distance traveled in 30 min") +
  theme_light() + 
  coord_flip()
g3.sl.ind

#---- 
# other possibility, using a Gamma distribution for step length

# parameterization
# mu = shape * scale -> scale = mu/shape
# shape
dgamma2 = function(x, mu, shape, ...) dgamma(x, shape = shape, scale = mu/shape, ...)

LLgamma <- function(x, mu, shape) {
  -sum(dgamma2(x, mu = mu, shape = shape, log = T))
}
# mwei <- mle2(LLgamma, start = list(mu = mean(x$sl_), shape = mean(x$sl_)**2/var(x$sl_)), data = x)

# calculate average and 95%CI for all individuals
mov.track.lu.summ.gamma <- mov.track.lu %>% 
  dplyr::mutate(sl_ = sl_ + 1) %>% 
  tidyr::nest_legacy(-c(name, classes.corr)) %>% 
  dplyr::mutate(fit = purrr::map(data, amt::fit_sl_dist, x = sl_),
                fit.bbmle = purrr::map(data, function(x) {
                  mle2(LLgamma, start = list(mu = mean(x$sl_), shape = mean(x$sl_)**2/var(x$sl_)), data = list(x = x$sl_))}),
                conf.int = purrr::map(fit.bbmle, confint),
                coef = map(fit.bbmle, ~ broom::tidy(., conf.int = T))) %>% 
  tidyr::unnest_legacy(coef) %>% 
  dplyr::filter(term == "mu")

# plot - it is almost the same thing!
g2.sl +
  geom_pointrange(data = mov.track.lu.summ.gamma,
                  aes(x = as.numeric(classes.corr), y = estimate, 
                      ymin = conf.low, ymax = conf.high,
                      group = name, col = name), 
                  position = position_dodge(width = 0.7), size = 0.8, inherit.aes = FALSE)

# plot turning angle

# general boxplot
mov.track.lu %>% 
  ggplot() +
  geom_boxplot(aes(x = classes.corr, y = ta_))

# density plot for each individual
g1.ta <- mov.track.lu %>% 
  ggplot(aes(x = 180/pi*ta_, y = classes.corr, fill = classes.corr)) +
  ggridges::geom_density_ridges(alpha = 0.5) +
  facet_wrap(~name) +
  theme_bw() +
  labs(x = 'Turning angle', y = '',
       title = 'Turning angle in 30-min steps') +
  theme(legend.position = "none")
g1.ta

ggsave(filename = "ta_one_figure_distribution.png", plot = g1.ta, path = "results", 
       width = 20, height = 17, units = "cm", dpi = 300)

# plot all estimates in a single plot
g2.ta <- mov.track.lu.summ %>% 
  ggplot(., aes(x = as.numeric(classes.corr), y = 180/pi*avg.ta, group = name, col = name)) + 
  geom_rect(mapping = aes(xmin = x - .4, xmax = x + .4, 
                          ymin = 180/pi*CI.ta.lower, ymax = 180/pi*CI.ta.higher), 
            data = pop.lu.summ, inherit.aes = FALSE, fill = "grey90") + 
  geom_segment(mapping = aes(x = x - .4, xend = x + .4,
                             y = 180/pi*avg.ta, yend = 180/pi*avg.ta), 
               data = pop.lu.summ, inherit.aes = FALSE, size = 1) + 
  geom_pointrange(aes(x = as.numeric(classes.corr), ymin = 180/pi*CI.ta.lower, ymax = 180/pi*CI.ta.higher), 
                  position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_text(data = pop.lu.summ, aes(x = x, label = classes.corr), y = -150, inherit.aes = FALSE) +
  labs(x = '', y = 'Turning angle',
       col = 'Group',
       title = 'Turning angle in 30-min steps') + 
  scale_x_discrete(breaks = pop.lu.summ$x, label = rep("", length(pop.lu.summ$x))) +
  coord_flip(clip = "off") +
  theme_light() + 
  theme(plot.margin = unit(c(1,1,1,6), "lines"))
g2.ta

ggsave(filename = "ta_one_figure.png", plot = g2.ta, path = "results", 
       width = 20, height = 17, units = "cm", dpi = 300)

# plot only populational step length values
g3.ta.pop <- pop.lu.summ %>% 
  ggplot(aes(x = classes.corr, y = 180/pi*avg.ta)) +
  geom_pointrange(aes(ymin = 180/pi*CI.ta.lower, ymax = 180/pi*CI.ta.higher),
                  position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = "", y = "Turning angle",
       title = "Population turning angle in 30-min steps") +
  theme_light() + 
  coord_flip()
g3.ta.pop

# plot step length for each individual
g3.ta.ind <- mov.track.lu.summ %>% 
  ggplot(aes(x = classes.corr, y = 180/pi*avg.ta)) +
  # geom_pointrange(aes(ymin = 180/pi*(avg.ta - sd.ta), ymax = 180/pi*(avg.ta + sd.ta)),
  geom_pointrange(aes(ymin = 180/pi*CI.ta.lower, ymax = 180/pi*CI.ta.higher),
                  position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) + 
  facet_wrap(~name) +
  labs(x = "", y = "Turning angle",
       title = "Group level turning angle in 30-min steps") +
  theme_light() + 
  coord_flip()
g3.ta.ind

# fitting von mises
# dgamma2 = function(x, mu, shape, ...) dgamma(x, shape = shape, scale = mu/shape, ...)
# 
# LLgamma <- function(x, mu, shape) {
#   -sum(dgamma2(x, mu = mu, shape = shape, log = T))
# }
# mwei <- mle2(LLgamma, start = list(mu = mean(x$sl_), shape = mean(x$sl_)**2/var(x$sl_)), data = x)

# calculate average and 95%CI for all individuals
# mov.track.lu.summ.vonmises <- mov.track.lu %>%
#   tidyr::nest_legacy(-c(name, classes.corr)) %>% 
#   dplyr::mutate(fit = purrr::map(data, amt::fit_ta_dist, x = ta_),
#                 # fit.bbmle = purrr::map(data, function(x) {
#                 #   mle2(LLgamma, start = list(mu = mean(x$sl_), shape = mean(x$sl_)**2/var(x$sl_)), data = list(x = x$sl_))}),
#                 # conf.int = purrr::map(fit.bbmle, confint),
#                 coef = map(fit, ~ broom::tidy(.$fit, conf.int = T))) %>% 
#   tidyr::unnest_legacy(coef) %>% 
#   dplyr::filter(term == "mu")

#-------------------------
# resample trajectory and do the analysis

# resample the trajectories for 30 min, filter paths with minimum 3 steps, transform coordinates to 
# the same CRS as the background maps, create random steps, calculate steps and turning angles by burst,
# calculate the correct class taking into account the plantation and harvest dates
mov.track.rsp.all <- mov.track %>% 
  tidyr::nest_legacy(-name) %>% 
  dplyr::mutate(trk = map(data, function(d) {
    d %>% 
      amt::track_resample(rate = minutes(30), tolerance = minutes(10)) %>%
      amt::filter_min_n_burst(min_n = 3) %>% 
      amt::transform_coords(crs_to = coord.system) %>% 
      amt::steps_by_burst() %>% 
      amt::random_steps(n = 9) %>% 
      amt::extract_covariates(maps) %>%
      dplyr::mutate(classes.lev = factor(classes, levels = 1:length(lu.classes), labels = lu.classes),
                    dttm.harvest = ymd('1970-01-01') + days(julian.date),
                    classes.corr = ifelse((classes.lev == lu.classes[3]) & (t2_ > dttm.harvest) & (t2_ < dttm.harvest + days(3*30)) & !is.na(dttm.harvest), 
                                   lu.classes[7], as.character(classes.lev)),
                    classes.corr = as.factor(ifelse((classes.corr == lu.classes[6]) & (t2_ > dttm.harvest) & !is.na(dttm.harvest), 
                                             lu.classes[7], as.character(classes.corr))),
                    classes.corr = relevel(classes.corr, lu.classes[4])) %>% 
      dplyr::filter(!is.na(classes.corr)) %>%
      dplyr::mutate(log_sl_ = log(sl_))
  }))

# check
mov.track.rsp.all %>% 
  dplyr::select(-data) %>% 
  tidyr::unnest_legacy(cols = trk)

mov.track.rsp.all %>% 
  dplyr::select(-data) %>% 
  tidyr::unnest_legacy(cols = trk) %>% 
  dplyr::filter(as.character(classes.lev) != as.character(classes.corr)) %>% 
  print(n = 1000)

# get all steps (used or available) that go outside the mapped area (or to the "Vala" class that we're going to omit)
# we also remove steps for which the group Olhos D'Agua Leste available steps were Sugarcane, since they are too few (< 10)
# and the animal did not use this class even a single time
outside.area <- mov.track.rsp.all %>%
  dplyr::select(-data) %>%
  tidyr::unnest_legacy(cols = trk) %>%
  dplyr::mutate(row.number = row_number(),
         outside.area = is.na(classes.corr),
         identifier = paste(name, burst_, step_id_, sep = "_")) %>%
  dplyr::filter(outside.area == T | classes.corr == "Não Disponível - Vala" | 
                  (name == "Olhos D'Agua Leste" & (classes.corr == lu.classes[3] | classes.corr == lu.classes[7])))

outside.area$classes.corr # ok

# filtering these lines and re-nesting
mov.track.rsp.all.nonNA <- mov.track.rsp.all %>%
  dplyr::select(-data) %>%
  tidyr::unnest_legacy(cols = trk) %>%
  dplyr::mutate(identifier = paste(name, burst_, step_id_, sep = "_")) %>%
  dplyr::filter(!(identifier %in% outside.area$identifier)) %>%
  # dplyr::filter(!(name == "Olhos D'Agua Leste" & classes.corr == lu.classes[3])) %>%
  tidyr::nest_legacy(-name)
  
m1.all <- mov.track.rsp.all.nonNA %>%
  mutate(trk = map(data, function(d) {
    d %>%
      dplyr::mutate(classes.corr = factor(classes.corr, levels = sort(unique(classes.corr))) %>% relevel(lu.classes[4]))
    }),
    fit = map(trk, ~ amt::fit_clogit(., case_ ~ classes.corr + strata(step_id_))))

# m1.all <- mov.track.rsp.all %>%
#   dplyr::mutate(
#     trk.class.ok = map(trk, function(d) {
#       d %>% 
#         dplyr::mutate(classes.corr = factor(classes.corr, levels = sort(unique(classes.corr))))
#       }),
#     fit = map(trk.class.ok, ~ amt::fit_clogit(., case_ ~ classes.corr + strata(step_id_))))

# table with coefficients
d2 <- m1.all %>% 
  dplyr::mutate(coef = map(fit, ~ broom::tidy(.x$model))) %>% 
  dplyr::select(name, coef) %>% 
  tidyr::unnest_legacy() %>% 
  dplyr::mutate(name = factor(name),
         term = factor(term))

d2 %>% 
  print(n = 50)

# result for Olhos D'agua Oeste for sugarcane cannot be trusted. Let's ignore this class for this group.
d2 <- d2 %>% 
  dplyr::mutate(term = stringr::str_remove(term, "classes.corr") %>% as.factor())

d2

# summarize results for the whole population by averaging
d3 <- d2 %>% 
  dplyr::group_by(term) %>% 
  dplyr::summarize(n = n(),
                   mean = mean(estimate),
                   ymin = mean - 1.96 * sd(estimate)/sqrt(n-1),
                   ymax = mean + 1.96 * sd(estimate)/sqrt(n-1)) %>% 
  dplyr::mutate(x = 1:n())

# plot results for all individuals and the population
p1 <- d2 %>% 
  ggplot(., aes(x = as.numeric(term), y = estimate, group = name, col = name)) + 
  geom_rect(mapping = aes(xmin = x - .4, xmax = x + .4, ymin = ymin,
                          ymax = ymax), data = d3, inherit.aes = FALSE, fill = "grey90") + 
  geom_segment(mapping = aes(x = x - .4, xend = x + .4,
                             y = mean, yend = mean), data = d3, inherit.aes = FALSE, size = 1) + 
  geom_pointrange(aes(x = as.numeric(term), ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_text(data = d3, aes(x = x, label = term), y = -2.3, inherit.aes = FALSE) +
  labs(x = "", col = 'Group', y = "Relative Selection Strength",
       title = 'Habitat selection') + 
  ylim(-1.2, NA) +
  scale_x_discrete(breaks = pop.lu.summ$x, label = rep("", length(pop.lu.summ$x))) +
  coord_flip(clip = "off") +
  theme_light() + 
  theme(plot.margin = unit(c(1,1,1,6), "lines"))
p1

# save
ggsave(filename = "ssf_all_individuals.png", plot = p1, path = "results", 
       width = 20, height = 17, units = "cm", dpi = 300)

# for for the population only
p2 <- d3 %>% 
  ggplot(., aes(x = term, y = mean)) + 
  geom_pointrange(aes(ymin = ymin, ymax = ymax), position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "", y = "Relative Selection Strength") + 
  theme_light() + 
  coord_flip()
p2

# for each individual
m.each <- d2 %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~name) +
  labs(y = "Relative Selection Strength") +
  theme_light() +
  coord_flip()

m.each

ggsave(filename = "ssf_each_individual.png", plot = m.each, path = "results", 
       width = 20, height = 20, units = "cm", dpi = 300)

#----------------
# plot 

used.available <- mov.track.rsp.all.nonNA %>%
  dplyr::mutate(trk = map(data, function(d) {
    d %>%
      dplyr::mutate(classes.corr = factor(classes.corr, levels = sort(unique(classes.corr))) %>% relevel(lu.classes[4]))
  })) %>% 
  dplyr::select(-data) %>% 
  tidyr::unnest_legacy() %>% 
  dplyr::mutate(use.ava = factor(case_, levels = c(F, T), labels = c("Available", "Used")),
                name = as.factor(name), 
                classes.corr = as.factor(classes.corr))

total <- used.available %>% 
  dplyr::group_by(name, use.ava) %>% 
  dplyr::summarise(total = n())

prop <- used.available %>% 
  dplyr::mutate(use.ava = factor(case_, levels = c(F, T), labels = c("Available", "Used")),
                name = as.factor(name), 
                classes.corr = as.factor(classes.corr)) %>% 
  dplyr::group_by(name, use.ava, classes.corr) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::left_join(total, by = c("name", "use.ava")) %>% 
  dplyr::mutate(proportion = n/total)

# export table
prop %>% 
  readr::write_csv("results/table_proportion_use_availability.csv")

# plot
prop.plot <- prop %>%  
  ggplot() +
  geom_bar(aes(x = classes.corr, y = proportion, fill = use.ava), 
           stat = "identity", position = position_dodge()) +
  facet_wrap(~name) +
  coord_flip() +
  labs(x = "", y = "Proportion of use/availability", fill = "") +
  scale_y_continuous(labels=scales::percent)
  
ggsave(filename = "proportion_use_available.png", plot = prop.plot, path = "results", 
       width = 20, height = 20, units = "cm", dpi = 300)


#----------------
# perform RSF considering the whole range of each individual


#----------------
# include variation along the day

# Plot the functions
curve(cos(x*2*pi), from = -2, to = 2)
curve(cos(x*4*pi), lty = 2, add = T)
curve(0.5*sin(x*2*pi), col = 2, add = T)
curve(0.5*sin(x*4*pi), col = 2, lty = 2, add = T)
