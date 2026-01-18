# lejos-al-desierto
# Away to the wilderness!
## A collaborative effort with jamiesanderlin

The purpose of the repository is to describe basic steps used to select field site locations in the US Sky Islands and the Mogollon Rim/Plateau for the 2023 field season. 
The code is not repeatable without access to datasets that we input and generated during processing.

Funding credit: Southwest Climate Adaptation Center swcasc.arizona.edu

# Algorithm
## Candidate points
1. Read in the data, transform to common crs and subset to the study region. 
2. Identify all potential candidate sample locations by first extracting center points from all raster cells using forward climate velocity layer as a template.
3. Join the ownership, climate and fire data with the candidate points. 
## Previously sampled points
1. Read in the data for point locations sampled previously in the United States and Mexico.
2. Join with fire, climate and ownership data.
4. Compare the distribution of candidate points (above) to identify gaps in the data, i.e., climatic conditions and fire history that are missing from the previous sample.
## Last steps: 
1. Select five sets of candidate points from each of three subregions: northern Arizona, New Mexico, and the Sky Islands (United States).
2. Subset the points so the samples are in proportion to the total number of candidates in each subregion and represent a spatally balanced set of points.
 
## Datasets:
Forward climate velocity in km/yr for ensemble RCP8.5 projection 1995-2085: https://adaptwest.databasin.org/datasets/e841145986b54efa882e94d1a7beb3ac/

Fire data for Sky Islands, United States and Mexico (1985-2017) published version (data used here have minor edits) : https://doi.org/10.5066/P9BB5TIO

Fire data for Arizona and New Mexico (1985-2021) original polygon data from https://mtbs.gov/ were subset to the study region

Fire data for Arizona and New Mexico, Wildland Fire Perimeters Southwestern Region (2022) https://www.fs.usda.gov/detailfull/r3/landmanagement/gis/?cid=stelprdb5201889&width=full

Federal land ownership: https://www.sciencebase.gov/catalog/item/5d150464e4b0941bde5b7653

## Notes on other data sources:
PCA 1 and PCA 2: principal components for the three ecoregions were derived using bioclim variables from  https://adaptwest.databasin.org/pages/adaptwest-climatena

Study area boundary for the three ecoregions was derived from two sources: 1) Terrestrial Ecoregions: Level III http://www.cec.org/north-american-environmental-atlas/terrestrial-ecoregions-level-iii/ 
and 2) original Sky Island polygons available here: https://skyisland.maps.arcgis.com/home/item.html?id=6797fbaf9e524cae836925c5de6a186a

## Code

### Code to output a set of potential sample locations across a climate gradient
1.	dataprep_pt.selection.Rmd: putting all the above steps for producing candidate points together in report form including steps in the algorithm, above, e.g., analysis across climate gradients, fire history, and screening for accessibility.
2.	Graphical outputs from the report will soon be added.

### Code to generate a spatially balanced sample from locations output above
1.	load_packages.R:  load needed packages for selecting study sites (check if installed and if not install them)
2.	priority.site.select.master. R: apply the generalized random tessellation stratified (GRTS) sample to provide a spatially balanced sample of CASC sites. This is based on stevens and Olsen (2004) algorithm
3.	sample_functions.R: contains all the sampling functions for sampling field sites.

Stevens, D. L., & Olsen, A. R. (2004). Spatially Balanced Sampling of Natural Resources. Journal of the American Statistical Association, 99(465), 262–278. https://doi.org/10.1198/016214504000000250
	



