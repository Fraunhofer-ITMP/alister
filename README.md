<h1 align = "center">
 
 <img src="https://raw.githubusercontent.com/Fraunhofer-ITMP/alister/main/www/alister_akronym.png" style ="width:30%">
 
[![](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://itmp.shinyapps.io/alister/)
 
</h1>

<h2>
 Introduction
</h2>

ALISTER is a web-app containing scientific information on pre-analytical blood sample stability in metabolomics and lipidomics. With a user-friendly interface and easy navigation, the app provides you with accurate and current information, essential for any researcher or scientist working in these fields. Current app contents are limited to the influences of time delay and temperature during processing of (EDTA) plasma and serum samples, which have been identified to be among the major pre-analytical pitfalls. Within four different menus, the app aims to answer pre-analytical questions regarding two general scenarios. In prospective scnarios the app aims to advice on sampling protocols, that ensure stability of analytes of interest. In retrospective assessment of samples, that ALISTER assesses analytes stability based on pre-analytical sample information provided by the user. Detailed information on the scientific motivation for ALISTER can be found in its publication[^1].
The following document gives an overview over the app functionalities.

<h2>
 Plasma or serum?
</h2>

ALISTER can provide data for plasma or serum samples. In the first step you need to decide which matrix you are working with.

<h3 align = "center">
 <img src="https://raw.githubusercontent.com/Fraunhofer-ITMP/alister/main/www/tut/2023%2005%2003%20Tutorial/plasma_or_serum.PNG">
</h3>

<h2>
 Search modes
</h2>

As described previously ALISTER is able to answer questions regarding pro- as well as retrospective pre-analytical scenarios. Retrospective assessment of samples in respect to their pre-analytical stability can be made using: 
* Sample search
* Analyte search
* Data filtering mode
Prospective planning of sampling can be done using:
* Protocol search
* Analayte search

<h3 align = "center">
 Sample search
</h3>

> Research question: "I have a set of lipids and/or polar metabolites that have been measured from blood samples (e.g. biobank samples). I want to gain information on whether results should be treated with caution due to the mode of sample collection."

Sample search is used in the following way:
1. 

<h3 align = "center">
 <img src="https://user-images.githubusercontent.com/105213394/235858420-99191f0c-2ebc-48e1-81e0-7a7427d94fec.png">
</h3>

<h3 align = "center">
 Protocol search
</h3>

> Research question: "I want to take blood sample in order to analyze a certain set of lipids and/or polar metabolites. What is the optimal way of sampling in order to assure stability it most analytes of interest? What is the optimal way in order to ensure stability in all of them?"

Protocol search is currently only available for plasma samples. It is used in the following way:
1. 

<h3 align = "center">
 <img src="https://raw.githubusercontent.com/Fraunhofer-ITMP/alister/main/www/tut/2023%2005%2003%20Tutorial/Prot.PNG">
</h3>

<h3 align = "center">
 Analyte search
</h3>

> Research question: "I am interested in the stability of a very specific analyte. I need detailed information in how to take sample properly, as well as in how stable the analyte is under certain pre-analytical conditions."

Analyte search is used in the following way:
1. 

<h3 align = "center">
 <img src="https://raw.githubusercontent.com/Fraunhofer-ITMP/alister/main/www/tut/2023%2005%2003%20Tutorial/An.PNG">
</h3>

<h3 align = "center">
 Data filtering mode
</h3>

> Research question: "I have a set of measurements of various specific analytes. I do know the conditions the samples were taken under. I want an indiciation of which analytes might be dubious due to pre-analytical conditions. I also want to filter them out of my dataset preemptively."

Sample search is used in the following way:
1. 

<h3 align = "center">
 <img src="https://user-images.githubusercontent.com/105213394/235858925-df8a7aef-db77-4779-ad27-9cdf30764b3a.png">
</h3>

[^1]: Future reference
