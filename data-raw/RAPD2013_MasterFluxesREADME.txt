Introductory information

For each filename, a short description of what data it contains

This data table includes the measured soil efflux for each COLLAR within each plot relevant to the 2013 analysis.
Data were extracted from LICOR6400 raw files
The High Park Flux files are located here: 
Niwot and Fraser (FEF)
libname FFNT13 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\03.Data_and_notes\01.FEFNWT_plot efflux data and code\2013';
High Park
libname HiPark13 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\00.MasterDataFolder\03.HiP.Fluxes_RAW\EmilyLib';
****note these should be moved to iPlant****



Format of the file if not obvious from the file name

Obs	PLOTID	site	PlotCODE_ID	QUADRANT	YOD	VisualFireSeverity	Fire_CODE	BarkBeetle	Beetle_CODE	Date	YEAR	MONTH	HHMMSS	EFFLUX	RHcmbr	Tsoil_C	Tair	LICOR



If the data set includes multiple files that relate to one another, the relationship between the files or a description of the file structure that holds them (possible terminology might include "dataset" or "study" or "data package")
Name/institution/address/email information for
Principal investigator (or person responsible for collecting the data)
Associate or co-investigators
Contact person for questions
PI: David Moore davidjpmoore@email.arizona.edu
Co-I: Nicole Trahn nicole.trahan@gmail.com

Date of data collection (can be a single date, or a range)
Summer 2013

Information about geographic location of data collection
Efflux data recorded for plots at Niwot, Fraser, Stove Praire Ranch, United Methodist Church and Lory State Park)

Date that the file was created
1/23/2015

Date(s) that the file(s) was updated and the nature of the update(s), if applicable

Keywords used to describe the data topic
soil efflux, CO2 flux, soil respiration, li6400, licor 6400, High Park Fire, NSF RAPID

Language information
English

Methodological information
Method description, links or references to publications or other documentation containing experimental design or protocols used in data collection
Any instrument-specific information needed to understand or interpret the data
Standards and calibration information, if appropriate
Describe any quality-assurance procedures performed on the data
Definitions of codes or symbols used to note or characterize low quality/questionable/outliers that people should be aware of
People involved with sample collection, processing, analysis and/or submission

At each location we measured soil CO2 Efflux using a Li6400 infra-red gas analyzer. Soil collars were installed and remained in the field for 48 hours before analysis. 

Data-specific information

Full names and definitions (spell out abbreviated words) of column headings  for tabular data
Units of measurement
Definitions for codes or symbols used to record missing data
Specialized formats or abbreviations used

It is merged with the disturbance history of each plot based on field observations and analysis of tissue beneath the 
bark of trees, including the use of increment cores to estimate the year that trees ceased radial growth.
site='NWT: Niwot Ridge Ameriflux, FEF: Fraser Experimental Forest, SPR: Stove Praire Ranch (High Park Fire), UMC: United Methodist Church (High Park Fire), LSP: Lory State Park (High Park Fire)';
Fire_CODE='Coded Visual Estimates of burn intentsity Trahan/Dynes| 0: Unburned, 1: Light Burn, 2: Moderate Burn, 3: Severe Burn';
VisualFireSeverity='Visual Estimates of burn intentsity Trahan/Dynes| UNBUR: Unburned, LIGHT: Light Burn, MOD: Moderate Burn, SEVER: Severe Burn';
BarkBeetle='Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| YES: Evidence present, NO: Evidence Absent';
Beetle_CODE='Coded Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| 1: Evidence present, 0: Evidence Absent';
PLOTID='Unique identifier for each plot| combination of site||PlotCODE_ID||Beetle_CODE||Fire_CODE';
PlotCODE_ID='Three Letter code for within site indentification';
YOD='Year of Death (Dynes):Average year trees in the plot stopped growing estimated using tree ring analysis for Beetle Damaged Plots (Dynes MSc thesis) or Year of fire for dead trees without beetle damage, 0 indicates live trees'; 
EFFLUX='Rate of CO2 efflux from Soil | mmol CO2 per m^2 per second  measured by Li6400'; 
Tsoil_C='Temperature (Celsius) in soil measured by Li6400'; 
Tair='Temperature (Celsius) in air measured by Li6400'; 
LICOR='Serial number of name of Li6400'; 
QUADRANT='Quandrant of plot/location where the soil collar was placed| FEF & NWT collars are arranged on cardinal axes North, South, East or West | All High Park location have collars arranged on intercardinal axes'; 
HHMMSS ='Timestamp from Licor';
Date ='Date of measurement in format YYYYMMDD e.g. 20130723 = July 23rd 2013';

Sharing/Access information
Feb 2014 - sharable between project members

Licenses or restrictions placed on the data
Links to publications that cite or use the data
Links to other publicly accessible locations of the data
Recommended citation for the data
Information about funding sources that supported the collection of the data

*******MASTERFLUXES was processed using the following SAS code to standardise dates, variable names and labels

Folders for RAW DATA 
libname FFNT13 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\03.Data_and_notes\01.FEFNWT_plot efflux data and code\2013';
*enzyme analysis 2013;
libname microbe 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\03.Data_and_notes\SASlib\Enzymes';
*HiParkBGC;
libname HiPark13 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\00.MasterDataFolder\03.HiP.Fluxes_RAW\EmilyLib';
*FEFNWT2013;
libname BB2013 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\03.Data_and_notes\01.FEFNWT_plot efflux data and code\2013';
*MASTER FILE library;
libname MASTER 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\03.Data_and_notes\SASlib\MASTER';


*** CODE to merge files *****

****GRAB FEF & NWT ****; 
proc datasets lib=FFNT13;run;
proc contents data = FFNT13.RSOILFEF2013RAW;run;

data Funk1; set FFNT13.RSOILFEF2013RAW;
YEAR =substr(DATE,5,4)*1;
MONTH = substr(DATE,9,2)*1;
PLOT = plot;
SITE = SITE;
run;


data Nunk1; set FFNT13.RSOILNWT2013RAW;
YEAR =substr(DATE,5,4)*1;
MONTH = substr(DATE,9,2)*1;
PLOT = plot;
SITE = SITE;
run;

data Hunk1; set hiPark13.HPFLUX_SITE_MERGED;;
YEAR =substr(DATE,5,4)*1;
MONTH = substr(DATE,9,2)*1;
PLOT = plot;
SITE = SITE;
run;


data FluxesMerged1; SET FUNK1 NUNK1 HUNK1;
keep Date Site HHMMSS Collar ID Plot TSD Treatment EFFLUX RHcmbr Tsoil_C Tair LICOR YEAR MONTH collarID ;
run;

Data FluxesMerged2; set FluxesMerged1;
plotCODE = plot + 1000;
Junk1 = put(plotCODE, 4.) ; 
PlotCODE_ID = substr(Junk1, 2, 3);
drop Junk1 plotCODE;


CID = upcase(ID);
*make ID uppercase; 
Letter='Q';
CID2 = CATS(Letter,CID);
COLLARQ = collarID;
if site = 'FEF' then COLLARQ = CID2;
if site = 'NWT' then COLLARQ = CID2;
QUADRANT = COLLARQ;
KEEP Date Site HHMMSS EFFLUX RHcmbr Tsoil_C Tair LICOR YEAR MONTH PlotCODE_ID QUADRANT ;
*to merge by site PlotCODE_ID;
run;


*need to merge with MASTER.MASTERPLOT_DISTURBANCE_INFO


Proc sort data = FluxesMerged2; by site PlotCODE_ID; 
run;

Proc sort data = MASTER.MASTERPLOT_DISTURBANCE_INFO; by site PlotCODE_ID; 
run;

data RAPID2013_MasterFluxes; merge MASTER.MASTERPLOT_DISTURBANCE_INFO FluxesMerged2; by site PlotCODE_ID; if site = 'UMCz' then delete; run;

data MASTER.RAPID2013_MasterFluxes; set RAPID2013_MasterFluxes;
Label site='NWT: Niwot Ridge Ameriflux, FEF: Fraser Experimental Forest, SPR: Stove Praire Ranch (High Park Fire), UMC: United Methodist Church (High Park Fire), LSP: Lory State Park (High Park Fire)';
Label Fire_CODE='Coded Visual Estimates of burn intentsity Trahan/Dynes| 0: Unburned, 1: Light Burn, 2: Moderate Burn, 3: Severe Burn';
Label VisualFireSeverity='Visual Estimates of burn intentsity Trahan/Dynes| UNBUR: Unburned, LIGHT: Light Burn, MOD: Moderate Burn, SEVER: Severe Burn';
Label BarkBeetle='Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| YES: Evidence present, NO: Evidence Absent';
Label Beetle_CODE='Coded Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| 1: Evidence present, 0: Evidence Absent';
Label PLOTID='Unique identifier for each plot| combination of site||PlotCODE_ID||Beetle_CODE||Fire_CODE';
label PlotCODE_ID='Three Letter code for within site indentification';
Label YOD='Year of Death (Dynes):Average year trees in the plot stopped growing estimated using tree ring analysis for Beetle Damaged Plots (Dynes MSc thesis) or Year of fire for dead trees without beetle damage, 0 indicates live trees'; 
Label EFFLUX='Rate of CO2 efflux from Soil | mmol CO2 per m^2 per second  measured by Li6400'; 
Label Tsoil_C='Temperature (Celsius) in soil measured by Li6400'; 
Label Tair='Temperature (Celsius) in air measured by Li6400'; 
Label LICOR='Serial number of name of Li6400'; 
Label QUADRANT='Quandrant of plot/location where the soil collar was placed| FEF & NWT collars are arranged on cardinal axes North, South, East or West | All High Park location have collars arranged on intercardinal axes'; 
label HHMMSS ='Timestamp from Licor';
label Date ='Date of measurement in format YYYYMMDD e.g. 20130723 = July 23rd 2013';
run;



   ods csv body="D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\00.MasterDataFolder\00.MasterDataTablesAndREADMEs\RAPID2013_MasterFluxes.csv";
   proc print data=MASTER.RAPID2013_MasterFluxes;
   var PLOTID site PlotCODE_ID QUADRANT YOD VisualFireSeverity Fire_CODE BarkBeetle Beetle_CODE Date YEAR MONTH HHMMSS EFFLUX RHcmbr Tsoil_C Tair LICOR;
   run;
   ods csv close;
