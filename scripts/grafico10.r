#load libraries
library(readxl);
library(ggplot2);

#donwload file
url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx";
destfile <- "COVID_19_geographic_disbtribution_worldwide.xlsx";
download.file(url, destfile);
datoscovidok <- read_excel(destfile);

#options
paises <- list('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'United_States_of_America', 'Germany',  'Italy', 'South_Korea', 'China', 'Iran', 'Turkey');
paises2 <- list('Argentina', 'Brazil', 'Chile', 'Colombia', 'Germany', 'Italy', 'United_States_of_America', 'China');
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
pathGraficos <- paste(directorioInteractivo, '/', directorioGraficos, sep ="")

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


  #grafico10
  gra10 <- ggplot(
    data = datoscovidok[datoscovidok$countriesAndTerritories %in% paises & datoscovidok$casesAcum > 100,], 
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
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
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
  library(patchwork);
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
    data = datoscovidok[datoscovidok$casesAcum > 100 & datoscovidok$popData2018 > 10000000,], 
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
  htmlwidgets::saveWidget(gr10Interactivo, archivoInteractivo, title = 'CoronaLea 2.0');
  if(file.exists(pathInteractivo)) unlink(pathInteractivo);
  file.copy(archivoInteractivo, directorioInteractivo);

system('cd ~/covid/publica/fasecero.github.io');
system('git add .');
system(paste('git commit -m\'Automatica ', ultimaFecha, '\'', sep = ""));
system('git push origin master');

