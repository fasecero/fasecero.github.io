#load libraries
library(readxl);
library(ggplot2);
library(patchwork);
library(scales);
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#donwload file
url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx";
destfile <- "COVID_19_geographic_disbtribution_worldwide.xlsx";
download.file(url, destfile);
datoscovidok <- read_excel(destfile);

#options
paises <- list('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'United_States_of_America', 'Germany',  'Italy', 'South_Korea', 'China', 'Iran', 'Turkey');
paises2 <- list('Argentina', 'Brazil', 'Chile', 'Colombia', 'Germany', 'Italy', 'United_States_of_America', 'China', 'Mexico');
#paisesAmericaDelSur <- list('Argentina', 'Chile', 'Uruguay', 'Bolivia', 'Brazil', 'Paraguay', 'Ecuador', 'Colombia', 'Venezuela', 'Peru');
paisesAmericaDelSur <- list('Argentina', 'Chile', 'Brazil',  'Ecuador', 'Colombia', 'Peru', 'Mexico', 'Paraguay', 'Bolivia');
paises4 <- list('Argentina', 'Brazil', 'Chile', 'Colombia', 'Germany', 'Italy', 'United_States_of_America', 'Mexico');
theme_set(theme_bw());
caption = 'Elaboarción propia en base a datos del ECDC.';
grUnits <- "in";
grDevice <- 'png';
grDpi <- 120;
grPixelWidth <- 1600;
grPixelHeight <- 1200;
grHeight <- grPixelHeight / grDpi;
grWidth <- grPixelWidth / grDpi;
directorioInteractivo <- '~/covid/publica/fasecero.github.io'; 
archivoInteractivo <- 'index.html';
directorioGraficos <- 'graficos';
pathGraficos <- paste(directorioInteractivo, '/', directorioGraficos, sep ="");
#colores para los graficos de continentes: se mezclaban mucho el azul y el turquesa, el 4 es europa
coloresContinente <- gg_color_hue(7);
coloresContinente[4] <- 'red';


#pre-process file

#grafico10
datoscovidok$realDate <- as.Date(sprintf("%d/%2d/%d", datoscovidok$day,datoscovidok$month, datoscovidok$year), format = "%d/%m/%Y");
datoscovidok$casesAcum <- apply(datoscovidok, 1, function(x) sum(subset(datoscovidok, (geoId == x['geoId'] & realDate <= x['realDate']), select = 'cases')));
datoscovidok$casesAcumSemana <- apply(datoscovidok, 1, function(x) sum(subset(datoscovidok, (geoId == x['geoId'] & realDate <= x['realDate'] & realDate >= (as.Date(x['realDate'])-6) ), select = 'cases')));
datoscovidok$tasaDiaria <- ifelse(
  datoscovidok$casesAcumSemana < 0,
  NA,
  ifelse(
    datoscovidok$casesAcum - datoscovidok$casesAcumSemana == 0,
    1,
    with(datoscovidok, (casesAcum/(casesAcum - casesAcumSemana))^(1/7)-1)));
datoscovidok$diasDuplicar <- ifelse(datoscovidok$tasaDiaria == 0, NA, with(datoscovidok, log(2, tasaDiaria+1)));

#grafico mortalidad
datoscovidok$deathsAcum <- apply(datoscovidok, 1, function(x) sum(subset(datoscovidok, (geoId == x['geoId'] & realDate <= x['realDate']), select = 'deaths')));
datoscovidok$mortalidad <- datoscovidok$deathsAcum / datoscovidok$casesAcum;

#comparacion de tasa actuales
ultimaFecha <- max(datoscovidok$realDate);
caption <- paste(caption, ' Actualizado al ', ultimaFecha);
tasasActuales <- datoscovidok[datoscovidok$realDate == as.Date(ultimaFecha),];
#tasasActuales$tasaDiaria <- with(tasasActuales, (casesAcum/(casesAcum - casesAcumSemana))^(1/7)-1);

#comparacio de tasas anteriores
casosABuscar = max(datoscovidok[datoscovidok$countriesAndTerritories == 'Argentina', 'casesAcum']);
todosMayores = datoscovidok[datoscovidok$casesAcum >= casosABuscar,];
tasasAnteriores <- merge(aggregate(casesAcum ~ countriesAndTerritories, data = todosMayores, FUN = min), todosMayores);
#tasasAnteriores$tasaDiaria <- with(tasasAnteriores, (casesAcum/(casesAcum - casesAcumSemana))^(1/7)-1);
tasasAnteriores <- aggregate(tasasAnteriores, by = list(tasasAnteriores$countriesAndTerritories), FUN = min);

#continentes
norte <- list('United_States_of_America', 'Canada', 'Mexico');
datoscovidok$subContinente <- ifelse(datoscovidok$countriesAndTerritories %in% norte, 'North America (US, Canada, Mexico)', datoscovidok$continentExp);
datoscovidok$subContinente <- ifelse(datoscovidok$subContinente == 'America', 'America Latina y Caribe', datoscovidok$subContinente);
continente <- aggregate(datoscovidok[, c('cases', 'deaths', 'casesAcum', 'casesAcumSemana', 'deathsAcum')], by = list(datoscovidok$realDate, datoscovidok$subContinente), FUN = sum)
#aggregate(casesAcum ~ countriesAndTerritories, data = todosMayores, FUN = min)
poblacion <- aggregate(popData2019 ~ subContinente, data = datoscovidok[datoscovidok$realDate == as.Date('2020-04-27'),], FUN = sum);
continente <- merge(continente, poblacion, by.x = 'Group.2', by.y ="subContinente", all.x = TRUE);
continente$casosAcumCada100Mil <- continente$casesAcum / continente$popData2019 * 100000;
continente$casosAcumSemanaCada100Mil <- continente$casesAcumSemana / continente$popData2019 * 100000;

colores <- gg_color_hue(7);

#dias duplicacion y proyecccion argentina
datosargentina <- datoscovidok[datoscovidok$countriesAndTerritories == 'Argentina',];
datosargentina <- datosargentina[rev(order(datosargentina$realDate)), ];
ultimosDatos <- head(datosargentina, n= 15);
ultimoDato <- head(ultimosDatos, n= 1);
primerDato <- tail(ultimosDatos, n= 1);
model <- lm(diasDuplicar ~ realDate, data = ultimosDatos);
nuevosDias <- NULL;
nuevosDias$realDate <- seq(max(ultimosDatos$realDate)+1,by = 'day', length.out = 30);
nuevosDias$diasDuplicar <- predict(model, newdata = nuevosDias);
df <- data.frame(realDate = nuevosDias$realDate, diasDuplicar = nuevosDias$diasDuplicar, countriesAndTerritories = rep('Argentina (proyección)', 30), continentExp = rep('America', 30));

df2 <- rbind(datoscovidok[,c('realDate', 'diasDuplicar', 'countriesAndTerritories', 'continentExp')], df);

#------------------------------------------------------------------------------------------------

  #grafico10
  gra10 <- ggplot(
    #data = datoscovidok[datoscovidok$countriesAndTerritories %in% paises & datoscovidok$casesAcum > 100,],
    data = datoscovidok[datoscovidok$countriesAndTerritories %in% paises & datoscovidok$casesAcum > 100 & datoscovidok$countriesAndTerritories != 'Ecuador',],
    mapping = aes( 
      x = casesAcum, 
      y = casesAcumSemana, 
      color = countriesAndTerritories)) + 
    geom_line(aes(linetype = continentExp)) + 
    scale_x_log10(labels = scales::comma_format()) + 
    scale_y_log10(labels = scales::comma_format()) + 
    theme(legend.position="bottom", legend.title  = element_blank()) +
    labs(x = 'Casos confirmados', y = 'Casos semanales', caption = caption);
  
  ggsave(paste(pathGraficos, '/', 'casossemanacovid', ultimaFecha, ".png", sep = ""), plot = gra10, width = grWidth, height = grHeight, dpi = grDpi, units = grUnits, device= grDevice);
         
  #grafico tasas actuales
  grTasasActuales <- ggplot(data = tasasActuales[tasasActuales$countriesAndTerritories %in% paises,], 
                   mapping = aes(
                     x=tasaDiaria, 
                     y=countriesAndTerritories, 
                     fill=countriesAndTerritories)) +
    geom_col() + 
    coord_flip() + 
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) +  
    labs(
      title=paste("Tasa diaria de crecimiento al", ultimaFecha), 
      x ="tasa diaria (acumulativa 7 dias)", 
      y = "",
      caption = caption) + 
    geom_text(aes(label = countriesAndTerritories), nudge_x = -0.002);
  
  #ggsave(paste('tasasactualescovid', ultimaFecha, ".png"), plot = grTasasActuales, width = grWidth, height = grHeight, dpi = grDpi, units = grUnits, device= grDevice);
  
  #grafico tasas anteirores
  grTasasAnteriores <- ggplot(data = tasasAnteriores[tasasAnteriores$countriesAndTerritories %in% paises,], 
         mapping = aes(
           x=tasaDiaria, 
           y=countriesAndTerritories, 
           fill=countriesAndTerritories)) + 
    geom_col() + 
    coord_flip() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +  
    theme(
      legend.position = "none", 
      axis.title.x = element_blank(), 
      axis.text.x = element_blank()) +  
    labs(
      title=paste("Tasa diaria de cremiento a los", format(casosABuscar, big.mark = ","), "casos acumulados."), 
      x ="tasa diaria (acumulativa 7 dias)", 
      y = "",
      caption = caption) + 
    geom_text(aes(label = countriesAndTerritories), nudge_x = -0.01);
  
  #ggsave(paste('tasasanteriorescovid', ultimaFecha, ".png"), plot = grTasasAnteriores, width = grWidth, height = grHeight, dpi = grDpi, units = grUnits, device= grDevice);
  grTasasActuales <- grTasasActuales + labs(caption = element_blank());
  grTasasTotal <- grTasasActuales + grTasasAnteriores + plot_annotation(title = 'Comparacion de tasas');
  ggsave(paste(pathGraficos, '/', 'tasastotal', ultimaFecha, ".png", sep = ""), plot = grTasasTotal, width = grWidth, height = grHeight*2/3, dpi = grDpi, units = grUnits, device= grDevice);
  
  #grafico de mortalidad
  grMortalidad <- ggplot(data = datoscovidok[datoscovidok$countriesAndTerritories %in% paises2 & datoscovidok$casesAcum > 100,],
         mapping = aes(
           x = realDate, 
           y = mortalidad, 
           color = countriesAndTerritories)) + 
    geom_line(
      size = 0.7, 
      aes(linetype = continentExp)) +  
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title="Mortalidad", 
      x ="", 
      y = "Muertes/Casos confirmados",
      caption = caption) + 
    theme(
      legend.position="bottom", 
      legend.title  = element_blank()) + 
    coord_cartesian(
      xlim = c(as.Date('2020-03-01'), max(datoscovidok$realDate)));
  
  ggsave(paste(pathGraficos, '/', 'mortalidadcovid', ultimaFecha, ".png", sep = ""), plot = grMortalidad, width = grWidth, height = grHeight, dpi = grDpi, units = grUnits, device= grDevice);
  
#interactivo para la web
  gra10paraWeb <- ggplot(
    data = datoscovidok[datoscovidok$casesAcum > 100 & datoscovidok$popData2019 > 10000000,], 
    mapping = aes( 
      x = casesAcum, 
      y = casesAcumSemana, 
      color = countriesAndTerritories)) + 
    geom_line(aes(linetype = continentExp)) + 
    scale_x_log10(labels = scales::comma_format()) + 
    scale_y_log10(labels = scales::comma_format()) + 
    theme(legend.position="bottom", legend.title  = element_blank()) +
    labs(x = 'Casos confirmados', y = 'Casos semanales', caption = caption);
  
  library(plotly);
  gr10Interactivo <- ggplotly(gra10paraWeb);
  pathInteractivo <- paste(directorioInteractivo, '/', archivoInteractivo, sep = ""); 
  if(file.exists(pathInteractivo)) unlink(pathInteractivo);
  htmlwidgets::saveWidget(gr10Interactivo, archivoInteractivo, title = 'CoronaLea 2.0');
  if(file.copy(archivoInteractivo, directorioInteractivo)) unlink(archivoInteractivo);

#graficos continentes

grafico11 <- ggplot(
  data = continente[continente$casesAcum > 1000, ], 
  mapping = aes( 
    x = casesAcum, 
    y = casesAcumSemana, 
    color = Group.2)) + 
  geom_line() + 
  scale_x_log10(labels = scales::comma_format()) + 
  scale_y_log10(labels = scales::comma_format()) + 
  theme(legend.position="bottom", legend.title  = element_blank()) +
  labs(x = 'Casos confirmados', y = 'Casos semanales', caption = caption) + 
  coord_cartesian(ylim = c(1000, max(continente$casesAcumSemana)));

grafico13 <- ggplot(
  data = continente[continente$casesAcum > 1000, ], 
  mapping = aes( 
    x = casosAcumCada100Mil, 
    y = casosAcumSemanaCada100Mil, 
    color = Group.2)) + 
  geom_line() + 
  scale_x_log10(labels = scales::comma_format()) + 
  scale_y_log10(labels = scales::comma_format()) + 
  theme(legend.position="bottom", legend.title  = element_blank()) +
  labs(x = 'Casos confirmados cada 100.000 habitantes', y = 'Casos semanales cada 100.000 habitantes', caption = caption);

grafico12 <- ggplot(
  data = continente[continente$Group.1 < (as.Date(max(continente$Group.1)) - 2),], 
  mapping = aes( 
    x = Group.1, 
    y = deathsAcum, 
    color = Group.2)) + 
  geom_line() +
  #geom_point() +
  #scale_x_log10(labels =   scales::comma_format()) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position="bottom", legend.title  = element_blank()) +
  labs(x = element_blank(), y = 'Muertes', caption = caption) + 
  coord_cartesian(xlim = c(as.Date('2020-03-01'), as.Date(max(continente$Group.1))));

grafico14 <- ggplot(
  data = continente[continente$Group.1 < (as.Date(max(continente$Group.1)) - 2),], 
  mapping = aes( 
    x = Group.1, 
    y = deathsAcum / popData2019 * 100000, 
    color = Group.2)) + 
  geom_line() +
  #geom_point() +
  #scale_x_log10(labels =   scales::comma_format()) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  theme(legend.position="bottom", legend.title  = element_blank()) +
  labs(x = element_blank(), y = 'Muertes cada 100.000 habitantes', caption = caption) + 
  coord_cartesian(xlim = c(as.Date('2020-03-01'), as.Date(max(continente$Group.1))));


grafico12 <- grafico12 + scale_colour_manual(values = coloresContinente);
grafico11 <- grafico11 + scale_colour_manual(values = coloresContinente);
grafico13 <- grafico13 + scale_colour_manual(values = coloresContinente);
grafico14 <- grafico14 + scale_colour_manual(values = coloresContinente);

grafico12 <- grafico12 + labs(caption = element_blank());
grafico15 <- grafico12 + grafico14;


grafico12 <- grafico11 + labs(caption = element_blank());
grafico16 <- grafico11 + grafico13;

ggsave(paste(pathGraficos, '/', 'muertesPorContinente', ultimaFecha, ".png", sep = ""), plot = grafico15, width = grWidth, height = grHeight*2/3, dpi = grDpi, units = grUnits, device= grDevice);
ggsave(paste(pathGraficos, '/', 'casosPorContinente', ultimaFecha, ".png", sep = ""), plot = grafico16, width = grWidth, height = grHeight*2/3, dpi = grDpi, units = grUnits, device= grDevice);


#dias dup
graficoProyeccion <- ggplot(
  data = df2[df2$countriesAndTerritories %in% paises2 | df2$countriesAndTerritories == 'Argentina (proyección)',],
  mapping = aes(x=realDate, y=diasDuplicar, color = countriesAndTerritories)) +
  geom_line(aes(linetype = continentExp)) +
  #geom_smooth(aes(color=countriesAndTerritories, linetype = continentExp), se = FALSE) +
  #geom_point() +
  geom_hline(aes(yintercept = 25), color = 'red', linetype = 'dashed', show.legend = FALSE) +
  geom_hline(aes(yintercept = 15), color = 'red', linetype = 'dashed', show.legend = FALSE) +
  geom_hline(aes(yintercept = 5), color = 'red', linetype = 'dashed', show.legend = FALSE) +
  geom_text(aes(as.Date('2020-03-15'), 25, label="Fase: Reapertura progresiva", vjust = -1, hjust = 0) , color = 'grey', size= 4) +
  geom_text(aes(as.Date('2020-03-15'), 15, label="Fase: Segmentacion geografica", vjust = -1, hjust = 0) , color = 'grey', size= 4) +
  geom_text(aes(as.Date('2020-03-15'), 5, label="Fase: Aislamiento administrado", vjust = -1, hjust = 0) , color = 'grey', size= 4) +
  theme(legend.position="bottom") +
  coord_cartesian(ylim = c(0,40), xlim = c(as.Date('2020-03-15'), max(df2$realDate))) +
  labs(y = 'Dias para duplicar', x = element_blank());

colores <- gg_color_hue(10);
colores[2] <- colores[1];

graficoProyeccion <- graficoProyeccion + scale_colour_manual(values = colores);

ggsave(paste(pathGraficos, '/', 'proyeccionArgentina', ultimaFecha, ".png", sep = ""), plot = graficoProyeccion, width = grWidth, height = grHeight, dpi = grDpi, units = grUnits, device= grDevice);


#dias dup

graficoProyeccion2 <- ggplot(
  data = df2[df2$countriesAndTerritories %in% paises4 | df2$countriesAndTerritories == 'Argentina (proyección)',],
  mapping = aes(x=realDate, y=diasDuplicar, color = countriesAndTerritories)) +
  #geom_line(aes(linetype = continentExp)) +
  geom_smooth(aes(color=countriesAndTerritories, linetype = continentExp), se = FALSE) +
  #geom_point() +
  geom_hline(aes(yintercept = 25), color = 'red', linetype = 'dashed', show.legend = FALSE) +
  geom_hline(aes(yintercept = 15), color = 'red', linetype = 'dashed', show.legend = FALSE) +
  geom_hline(aes(yintercept = 5), color = 'red', linetype = 'dashed', show.legend = FALSE) +
  geom_text(aes(as.Date('2020-03-15'), 25, label="Fase: Reapertura progresiva", vjust = -1, hjust = 0) , color = 'grey', size= 4) +
  geom_text(aes(as.Date('2020-03-15'), 15, label="Fase: Segmentacion geografica", vjust = -1, hjust = 0) , color = 'grey', size= 4) +
  geom_text(aes(as.Date('2020-03-15'), 5, label="Fase: Aislamiento administrado", vjust = -1, hjust = 0) , color = 'grey', size= 4) +
  theme(legend.position="bottom") +
  coord_cartesian(ylim = c(0,60), xlim = c(as.Date('2020-03-15'), max(df2$realDate))) +
  labs(y = 'Dias para duplicar', x = element_blank(), caption = caption);

colores <- gg_color_hue(10);
colores[2] <- colores[1];

graficoProyeccion2 <- graficoProyeccion2 + scale_colour_manual(values = colores);

ggsave(paste(pathGraficos, '/', 'smooth', ultimaFecha, ".png", sep = ""), plot = graficoProyeccion2, width = grWidth, height = grHeight, dpi = grDpi, units = grUnits, device= grDevice);


#graficos latam
grLatam1 <- ggplot(
  data = datoscovidok[datoscovidok$countriesAndTerritories %in% paisesAmericaDelSur, ],
  mapping = aes( 
    x = realDate, 
    y = deathsAcum, 
    color = countriesAndTerritories)) + 
  geom_line() +
  #geom_point() +
  #scale_x_log10(labels =   scales::comma_format()) + 
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_date(breaks = pretty_breaks(10)) +
  theme(legend.position="bottom", legend.title  = element_blank()) +
  labs(x = element_blank(), y = 'Muertes', caption = caption)  +
  coord_cartesian(xlim = c(as.Date('2020-04-01'), as.Date(ultimaFecha)));

grLatam2 <- ggplot(
  data = datoscovidok[datoscovidok$countriesAndTerritories %in% paisesAmericaDelSur, ],
  mapping = aes( 
    x = realDate, 
    y = deathsAcum / popData2019 * 100000, 
    color = countriesAndTerritories)) + 
  geom_line() +
  #geom_point() +
  #scale_x_log10(labels =   scales::comma_format()) + 
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_date(breaks = pretty_breaks(10)) +
  theme(legend.position="bottom", legend.title  = element_blank()) +
  labs(x = element_blank(), y = 'Muertes cada 100.000 habitantes', caption = caption) + 
  coord_cartesian(xlim = c(as.Date('2020-04-01'), as.Date(ultimaFecha)));

grLatam1 <- grLatam1 + labs(caption = element_blank());
grMuertesLA <- grLatam1 + grLatam2 + plot_annotation(
  title = 'Comparación cantidad de muertes por Covid-19 en países de latinoamérica'
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  #caption = 'Disclaimer: None of these plots are insightful'
);

ggsave(paste(pathGraficos, '/', 'muertesLA', ultimaFecha, ".png", sep = ""), plot = grMuertesLA, width = grWidth, height = grHeight*2/3, dpi = grDpi, units = grUnits, device= grDevice);

#grafico argentina EJE SECUNDARIO

factorSecundario <- 5000000;

grArg <- ggplot(
  data = datoscovidok[datoscovidok$countriesAndTerritories == 'Argentina',],
  mapping = aes(
    x = realDate
  )) +
  geom_line(aes(y = casesAcum, colour = 'Casos')) +
  geom_line(aes(y = tasaDiaria*factorSecundario, colour = 'Tasa diaria acumulariva ultimos 7 días')) +
  geom_point(aes(y=casesAcum, colour = 'Casos')) +
  geom_point(aes(y = tasaDiaria*factorSecundario, colour = 'Tasa diaria acumulariva ultimos 7 días')) +
  scale_y_continuous(sec.axis = sec_axis(~./factorSecundario, name = "Tasa", labels = scales::percent_format(accuracy = 0.1)), labels = scales::comma_format()) + 
  theme(
    legend.position="bottom", 
    legend.title  = element_blank()) +
  labs(y = 'Casos confirmados', x = element_blank(), title = "Casos confirmados y tasa diara de crecimiento Argentina", caption = caption) +
  coord_cartesian(xlim = c(as.Date('2020-03-15'), max(ultimaFecha)), ylim = c(0, max(datoscovidok[datoscovidok$countriesAndTerritories == 'Argentina', c('casesAcum')])));

ggsave(paste(pathGraficos, '/', 'Arg', ultimaFecha, ".png", sep = ""), plot = grArg, width = grWidth, height = grHeight, dpi = grDpi, units = grUnits, device= grDevice);




setwd(directorioInteractivo)
system('git add .');
system(paste('git commit -m\'Automatica ', ultimaFecha, '\'', sep = ""));
system('git push origin master');


