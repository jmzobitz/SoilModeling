FILENAME: 

For each filename, a short description of what data it contains |Format of the file if not obvious from the file name | If the data set includes multiple files that relate to one another, the relationship between the files or a description of the file structure that holds them (possible terminology might include "dataset" or "study" or "data package")
********

Obs	PLOTID	site	PlotCODE_ID	YOD	VisualFireSeverity	Fire_CODE	BarkBeetle	Beetle_CODE

These variables provide the disturbance information for each plot within the 2013 Summer campaign | High Park, Fraser and Niwot Ridge

********
Name/institution/address/email information for Principal investigator (or person responsible for collecting the data) | Associate or co-investigators | Contact person for questions
********
PI: David Moore davidjpmoore@email.arizona.edu
Co-I: Nicole Trahan Nicole.trahan@gmail.com
Collected by: Samples collected by Nicole Trahan & Emily Dynes 
Lab analysis:
********
Date of data collection (can be a single date, or a range) 


Information about geographic location of data collection
******
Niwot Ridge CO
Fraser Experimental Forest CO 
(Stove Praire Ranch, United Methodist Church and Lory State Park) High Park CO

******
Date that the file was created
*
Date(s) that the file(s) was updated and the nature of the update(s), if applicable
*

Keywords used to describe the data topic
********
NSF RAPID, High Park, Disturbance, code, indices

********
Language information
*

Methodological information
Method description, links or references to publications or other documentation containing experimental design or protocols used in data collection | Any instrument-specific information needed to understand or interpret the data | Standards and calibration information, if appropriate | Describe any quality-assurance procedures performed on the data | Definitions of codes or symbols used to note or characterize low quality/questionable/outliers that people should be aware of | People involved with sample collection, processing, analysis and/or submission | 
*********
The file defines the PLOTID the unique identifier for each plot| combination of site||PlotCODE_ID||Beetle_CODE||Fire_CODE
The first 3 digits are SITE, next 3 digits are the PlotCODE_ID, next digit is Beetle_CODE and the last digit is the Fire_CODE
Site||PlotCODE_ID||Beetle_CODE||Fire_CODE
(See below)

*********
Data-specific information
Full names and definitions (spell out abbreviated words) of column headings  for tabular data | ******* Units of measurement ******** | Definitions for codes or symbols used to record missing data | Specialized formats or abbreviations used 
******** 
PLEASE EXPLAIN HEADER CODES

site 'NWT: Niwot Ridge Ameriflux, FEF: Fraser Experimental Forest, SPR: Stove Praire Ranch (High Park Fire), UMC: United Methodist Church (High Park Fire), LSP: Lory State Park (High Park Fire)';
Fire_CODE 'Coded Visual Estimates of burn intentsity Trahan/Dynes| 0: Unburned, 1: Light Burn, 2: Moderate Burn, 3: Severe Burn';
VisualFireSeverity 'Visual Estimates of burn intentsity Trahan/Dynes| UNBUR: Unburned, LIGHT: Light Burn, MOD: Moderate Burn, SEVER: Severe Burn';
BarkBeetle 'Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| YES: Evidence present, NO: Evidence Absent';
BeetleCode 'Coded Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| 1: Evidence present, 0: Evidence Absent';
PLOTID 'Unique identifier for each plot| combination of site||PlotCODE_ID||Beetle_CODE||Fire_CODE';
YOD 'Year of Death (Dynes):Average year trees in the plot stopped growing estimated using tree ring analysis for Beetle Damaged Plots (Dynes MSc thesis) or Year of fire for dead trees without beetle damage, 0 indicates live trees'; 

********

Sharing/Access information
Licenses or restrictions placed on the data | Links to publications that cite or use the data | Links to other publicly accessible locations of the data | Recommended citation for the data |Information about funding sources that supported the collection of the data
*****
2/26/2015 - data can be shared within the research team

*****


/*
Author: Dave Moore
Date: 11/22/2014
Purpose:Organize disturbance information for plots at Niwot, Fraser, Stove Praire Ranch, United Methodist Church and Lory State Park)

File:MASTERPLOT_DISTURBANCE_INFO.csv
This data table denotes the unique identifier for each plot relevant to the 2013 analysis of fluxes, enzymes and biogeochemistry.
It provides information about the disturbance history of each plot based on field observations and analysis of tissue beneath the 
bark of trees, including the use of increment cores to estimate the year that trees ceased radial growth.


Notes: 
site 'NWT: Niwot Ridge Ameriflux, FEF: Fraser Experimental Forest, SPR: Stove Praire Ranch (High Park Fire), UMC: United Methodist Church (High Park Fire), LSP: Lory State Park (High Park Fire)';
Fire_CODE 'Coded Visual Estimates of burn intentsity Trahan/Dynes| 0: Unburned, 1: Light Burn, 2: Moderate Burn, 3: Severe Burn';
VisualFireSeverity 'Visual Estimates of burn intentsity Trahan/Dynes| UNBUR: Unburned, LIGHT: Light Burn, MOD: Moderate Burn, SEVER: Severe Burn';
BarkBeetle 'Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| YES: Evidence present, NO: Evidence Absent';
BeetleCode 'Coded Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| 1: Evidence present, 0: Evidence Absent';
PLOTID 'Unique identifier for each plot| combination of site||PlotCODE_ID||Beetle_CODE||Fire_CODE';
YOD 'Year of Death (Dynes):Average year trees in the plot stopped growing estimated using tree ring analysis for Beetle Damaged Plots (Dynes MSc thesis) or Year of fire for dead trees without beetle damage, 0 indicates live trees'; 
*/

*SASCODE to generate the files;
libname hipark 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\03.Data_and_notes\SASlib\hipark';
libname Mpark 'D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\03.Data_and_notes\SASlib\MASTER';

proc datasets lib=Mpark;run;
proc contents data =Mpark.MASTERPLOTINFO; ;run;
proc contents data = Mpark.NWTPLOTYOD; run;
proc print data =  Mpark.MASTERPLOTINFO; ;run;

Data MasterTreatments; set Mpark.MASTERPLOTINFO;
plotCODE = plot + 1000;
Junk1 = put(plotCODE, 4.) ; 
PlotCODE_ID = substr(Junk1, 2, 3);
drop Junk1 plotCODE;
if site = "UMCz" then delete;
If BB = "YES" then Beetle_CODE = 1;
If BB = "NO" then Beetle_CODE = 0;
If BURN = "UNBUR" then Fire_CODE=0;
If BURN = "LIGHT" then Fire_CODE=1;
If BURN = "MOD" then Fire_CODE=2;
If BURN = "SEVER" then Fire_CODE=3;
rename BB=BarkBeetle;
rename BURN=VisualFireSeverity;
PLOTID = cats(site,PlotCODE_ID,Beetle_CODE,Fire_CODE);
run;
data MasterTreatments; set MasterTreatments;
Label site='NWT: Niwot Ridge Ameriflux, FEF: Fraser Experimental Forest, SPR: Stove Praire Ranch (High Park Fire), UMC: United Methodist Church (High Park Fire), LSP: Lory State Park (High Park Fire)';
Label Fire_CODE='Coded Visual Estimates of burn intentsity Trahan/Dynes| 0: Unburned, 1: Light Burn, 2: Moderate Burn, 3: Severe Burn';
Label VisualFireSeverity='Visual Estimates of burn intentsity Trahan/Dynes| UNBUR: Unburned, LIGHT: Light Burn, MOD: Moderate Burn, SEVER: Severe Burn';
Label BarkBeetle='Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| YES: Evidence present, NO: Evidence Absent';
Label Beetle_CODE='Coded Evidence of Bark Beetle Damage (tree rings and field estimates Trahan/Dynes)| 1: Evidence present, 0: Evidence Absent';
Label PLOTID='Unique identifier for each plot| combination of site||PlotCODE_ID||Beetle_CODE||Fire_CODE';
Label YOD='Year of Death (Dynes):Average year trees in the plot stopped growing estimated using tree ring analysis for Beetle Damaged Plots (Dynes MSc thesis) or Year of fire for dead trees without beetle damage, 0 indicates live trees'; 
run;
proc print;run;
   ods csv body="D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\00.MasterDataFolder\00.MasterDataTablesAndREADMEs\MASTERPLOT_DISTURBANCE_INFO.csv";
   proc print data=work.MasterTreatments;
   var PLOTID site PlotCODE_ID YOD VisualFireSeverity Fire_CODE BarkBeetle Beetle_CODE;
   run;
   ods csv close;
