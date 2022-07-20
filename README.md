# Fireworks disturbance across bird communities
Code repository for paper (to be submitted). See [this website](https://barthoekstra.github.io/fireworks) for a knitted bookdown version of all RMarkdown analysis notebooks.

## Abstract
Fireworks are important parts of celebrations globally, but little remains known about their effect on wildlife. The synchronized and extraordinary use of fireworks on New Year's Eve causes a strong flight response in birds. We use weather radar and systematic bird counts to quantify how flight response differs across bird communities and determine the distance-dependence of this relationship. On average, approximately 1000 times more birds were in flight during New Year’s Eve than on regular nights. We found disturbance from fireworks decreases with distance, most strongly in the first 5 km, but overall flight activity remained elevated tenfold up to about 10 km. We found that communities of large-bodied species respond more strongly than those of smaller birds. Given the pervasive nature of this disturbance, mitigation should be achieved by establishing large firework-free zones or concentrating fireworks in urban centers. Conservation action should prioritize the most disturbance-prone, larger-bodied, bird communities.

## Authors

Bart Hoekstra<sup>1</sup>, Willem Bouten<sup>1</sup>, Adriaan Dokter<sup>1,2</sup>, Hans van Gasteren<sup>1,3</sup>, Chris van Turnhout<sup>4,5</sup>, Bart Kranstauber<sup>1</sup>, Emiel van Loon<sup>1</sup>, Hidde Leijnse<sup>6,7</sup>, Judy Shamoun-Baranes<sup>1</sup>

<sup>1</sup> Institute for Biodiversity and Ecosystem Dynamics, University of Amsterdam, P.O. Box 94240, 1090 GE Amsterdam, The Netherlands<br />
<sup>2</sup> Cornell Lab of Ornithology, Cornell University, 159 Sapsucker Woods Rd, Ithaca, NY 14850, United States of America<br />
<sup>3</sup> Royal Netherlands Air Force, P.O Box 8762, 4820 BB Breda, The Netherlands<br />
<sup>4</sup> Sovon Dutch Centre for Field Ornithology, P.O. Box 6521, 6503 GA Nijmegen, The Netherlands<br />
<sup>5</sup> Department of Animal Ecology & Physiology, Institute for Biological and Environmental Sciences (RIBES), Radboud University, P.O. Box 9010, 6500 GL Nijmegen, The Netherlands<br />
<sup>6</sup> R&D Observations and Data Technology, Royal Netherlands Meteorological Institute, De Bilt, The Netherlands<br />
<sup>7</sup> Hydrology and Quantitative Water Management Group, Wageningen University & Research, Wageningen, The Netherlands<br />

## Data
This repository contains all raw bird count data from Sovon. Other data should be downloaded separately using the instructions below. In addition, the analysis notebooks clearly show how files should be structured.

### Weather radar
Weather radar data can be acquired through the [KNMI Data Platform](https://dataplatform.knmi.nl). All files used in our analysis are listed in .txt files in folders in `data/raw/pvol/` and should be stored in their respective folders to run the scripts. Filenames in the .txt files are unique identifiers and can be used to directly query the KNMI API. See here for [Herwijnen](https://dataplatform.knmi.nl/dataset/radar-volume-full-herwijnen-1-0) and [Den Helder](https://dataplatform.knmi.nl/dataset/radar-volume-denhelder-2-0) radar data respectively.

### Land use
Land use data can be acquired via the Copernicus portal for the [CORINE Land Cover 2018](https://land.copernicus.eu/pan-european/corine-land-cover/clc2018) dataset. Contents of the CLC2018 archive should be extracted into the `data/raw/landuse/clc2018_clc2018_v2018_20_raster100m/` folder.

### Bird bodymasses
Life-history characteristics for birds can be downloaded from [Storchová & Hořák 2018](https://doi.org/10.1111/geb.12709) and should be extracted directly into the `data/raw/life-history-characteristics/` folder.

### Population density
We have tested several proxies for disturbance caused by fireworks, including human population density. This data can be acquired from [Statistics Netherlands](https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/kaart-van-500-meter-bij-500-meter-met-statistieken) ([direct link to file](https://www.cbs.nl/-/media/cbs/dossiers/nederland-regionaal/vierkanten/500/2022-cbs_vk500_2019_vol.zip)) and should be extracted in `data/raw/population-density/`.
