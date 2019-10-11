#' ---
#' title: 'White-lipped peccary movement in PN Emas - resource and step selection functions'
#' author: Bernardo Niebuhr, Ennio Painkow, Ronaldo Morato - CENAP/ICMBio
#' ---

#' # Loading and organizing data
#' 
#' First of all, we'll load and organize data, to prepare them as input to the
#' different movement analysis packages. We're working here with data from four
#' white-lipped peccary individuals (from four different groups) from the Parque Nacional
#' das Emmas, within the Brazilian Cerrado.
#' 

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
install.load::install_load('broom')
install.load::install_load('amt')

# --------------- label=load_data

#' ## Load data
#' 

# Load movement data
load('data/movement_data_loaded.RData')

# Load spatial data

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

# olhos pontal
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
r.af <- raster(map.alto.formoso, res = 10)
rast.alto.formoso.class <- fasterize::fasterize(sf = map.alto.formoso, raster = r.af, field = 'classe.num')
raster::plot(rast.alto.formoso.class)
rast.alto.formoso.julian <- fasterize::fasterize(sf = map.alto.formoso, raster = r.af, field = 'julian')
raster::plot(rast.alto.formoso.julian)

r.ol <- raster(map.olhos.leste, res = 10)
rast.olhos.leste.class <- fasterize::fasterize(sf = map.olhos.leste, raster = r.ol, field = 'classe.num')
raster::plot(rast.olhos.leste.class)
rast.olhos.leste.julian <- fasterize::fasterize(sf = map.olhos.leste, raster = r.ol, field = 'julian')
raster::plot(rast.olhos.leste.julian)

r.oo <- raster(map.olhos.oeste, res = 10)
rast.olhos.oeste.class <- fasterize::fasterize(sf = map.olhos.oeste, raster = r.oo, field = 'classe.num')
raster::plot(rast.olhos.oeste.class)
rast.olhos.oeste.julian <- fasterize::fasterize(sf = map.olhos.oeste, raster = r.oo, field = 'julian')
raster::plot(rast.olhos.oeste.julian)

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
         julian.pos = julian(t1_),
         classes.corr = as.factor(ifelse((classes.lev == lu.classes[3]) & (julian.pos > julian.date) & (julian.pos < julian.date + 3*30), 
                                         lu.classes[7], as.character(classes.lev))),
         classes.corr = as.factor(ifelse((classes.lev == lu.classes[6]) & (julian.pos > julian.date), 
                                         lu.classes[7], as.character(classes.lev)))) %>% 
  dplyr::filter(!is.na(classes.corr))

###
## Possibility: classificar o milho de maneira diferente apos a colheita; pode ser uma nova classe (durante um mes, e solo exposto apos)

# Check
eda1 %>% 
  dplyr::filter(!is.na(julian.date)) %>% 
  print(n = 200)

eda1 %>% 
  dplyr::filter(as.character(classes.lev) != as.character(classes.corr)) %>% 
  print(n = 1000)

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
         julian.pos = julian(t2_),
         classes.corr = as.factor(ifelse((classes.lev == lu.classes[3]) & (julian.pos > julian.date) & (julian.pos < julian.date + 3*30), 
                                         lu.classes[7], as.character(classes.lev))),
         classes.corr = as.factor(ifelse((classes.lev == lu.classes[6]) & (julian.pos > julian.date), 
                                         lu.classes[7], as.character(classes.lev)))) %>% 
  dplyr::filter(!is.na(classes.corr)) %>% 
  mutate(log_sl_ = log(sl_))

# Check
eda1 %>% 
  dplyr::filter(!is.na(julian.date)) %>% 
  print(n = 200)

d1 %>% 
  dplyr::filter(as.character(classes.lev) != as.character(classes.corr)) %>% 
  print(n = 1000)

################### !!!!!!!!!!!verificar

# run models
m1 <- d1 %>% 
  amt::fit_clogit(case_ ~ classes.corr + strata(step_id_))
m1

AIC(m1$model)
summary(m1)
s <- summary(m1$model)$coefficients
s
print(knitr::kable(s, digits = 4))

# plot
broom::tidy(m1$model)

m1.plot <- broom::tidy(m1$model) %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Relative Selection Strength") +
  theme_light() + 
  coord_flip()
m1.plot







mod.names <- c('m1', 'm2', 'm3', 'm4', 'm4.2', 'm5', 'm5.2', 'm6', 'm6.2', 'm7', 'm7.2')
mods <- list(m1, m2, m3, m4, m4.2, m5, m5.2, m6, m6.2, m7, m7.2)
(aic.100 <- tibble(model = mod.names,
                   AIC = lapply(mods, function(x) x[[1]]) %>%
                     lapply(., AIC) %>%
                     unlist
) %>%
    dplyr::arrange(AIC) %>% 
    mutate(dAIC = AIC - AIC[1],
           wAIC = exp(-0.5*dAIC)/sum(exp(-0.5*dAIC))))