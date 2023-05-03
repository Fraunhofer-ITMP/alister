<h1 align = "center">
 
 <img src="https://raw.githubusercontent.com/Fraunhofer-ITMP/alister/main/www/alister_akronym.png" style ="width:30%">
 
[![](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://itmp.shinyapps.io/alister/)
 
</h1>

<h1>
 Introduction
</h1>

ALISTER is a web-app containing scientific information on pre-analytical blood sample stability in metabolomics and lipidomics. With a user-friendly interface and easy navigation, the app provides you with accurate and current information, essential for any researcher or scientist working in these fields. Current app contents are limited to the influences of time delay and temperature during processing of (EDTA) plasma and serum samples, which have been identified to be among the major pre-analytical pitfalls. Within four different menus, the app aims to answer pre-analytical questions regarding two general scenarios. In prospective scnarios the app aims to advice on sampling protocols, that ensure stability of analytes of interest. In retrospective assessment of samples, that ALISTER assesses analytes stability based on pre-analytical sample information provided by the user. Detailed information on the scientific motivation for ALISTER can be found in its publication[^1].
The following document gives an overview over the app functionalities.

<h1>
 Plasma or serum?
</h1>

ALISTER can provide data for plasma or serum samples. In the first step you need to decide which matrix you are working with.

<h2 align = "center">
 <img src="https://raw.githubusercontent.com/Fraunhofer-ITMP/alister/main/www/tut/2023%2005%2003%20Tutorial/plasma_or_serum.PNG">
</h2>

<h1>
 Search modes
</h1>

As described previously ALISTER is able to answer questions regarding pro- as well as retrospective pre-analytical scenarios. Retrospective assessment of samples in respect to their pre-analytical stability can be made using: 
* Sample search
* Analyte search
* Data filtering mode
Prospective planning of sampling can be done using:
* Protocol search
* Analayte search

<h2 align = "center">
 Sample search
</h2>

> Research question: "I have a set of lipids and/or polar metabolites that have been measured from blood samples (e.g. biobank samples). I want to gain information on whether results should be treated with caution due to the mode of sample collection."

Sample search is used in the following way:
1. Enter compound classes of interest. Analytes are categorized by their main lipid class (when lipids) or their main class according to the RefMet database (when polar metabolites). You can choose to 'Select all' groups at once.
2. Choose the temperature your samples were processed under. If the temperature varied before and after centrifugation, check 'Other temperature after centrifugation' and add the additional information. 
3. Experiments have shown, that for specific analytes or analyte classes it does matter, whether time delay occurs before or after centrifugation. Enter the respective processing delays.
4. By default the critical threshold for assessing instability is set to 20% fold change due to specific pre-analytical conditions. When conditions are combined (e.g. added change due to delay before and after centrifugation) the threshold is expanded to 30%. You can choose to readjust this upper and lower bound by chkecking 'Define stability thresholds'.
5. This is the result of your query. In some cases the database underlying ALISTER will not exactly match your conditions. In these cases generalization needs to happen. Hover your mouse over each analyte in order to see how your input got interpreted. Click on 'Citation' in order to see relevant literature on the stability of the shown analytes.
6. A pie chart shows the overall influence of your input conditions on the set of selected analytes.
7. You can download a .csv-file containing your query, as well as the output and all relevant references. The tab 'Variable explanation' will answer all your questions regarding interpreation of the download.

<h2 align = "center">
 <img src="https://user-images.githubusercontent.com/105213394/235858420-99191f0c-2ebc-48e1-81e0-7a7427d94fec.png">
</h2>

<h2 align = "center">
 Protocol search
</h2>

> Research question: "I want to take blood sample in order to analyze a certain set of lipids and/or polar metabolites. What is the optimal way of sampling in order to assure stability it most analytes of interest? What is the optimal way in order to ensure stability in all of them?"

Protocol search is currently only available for plasma samples. It is used in the following way:
1. Like done previously, you can choose specific analyte classes of interest. 
2. If you want to be recommended the sampling protocol that is recommended in most cases, choose 'Majority Vote' (default). In other cases you might want to choose the most strict protocol in order to ensure stability for the maximum of selected analytes. For this, choose 'Maximize stable analytes'.
3. The output is straight forward. Based on its inherenc pre-analytical stability each analyte is recommended a sampling protocol. The overall protocol is recommended above the table. An icon next to the analyte indicates, whether the respective analyte is expected to be stable under the recommended protocol (this is mostly relevant when working in 'Majority Vote'.
4. You can investigate the recommended sampling protocol in form of a comprehensible flow chart.
5. A pie chart summarizes the expected stability of all selected analytes under the selected conditions.
6. Like previously, you can adjust the stability thresholds to be more or less strict.
7. Besides downloading a .csv-file containing your query, as well as the ouput, you are able to download a PDF, that depicts all possible sampling protocols in straight forward flowcharts. The tab 'Variable explanation' will answer all your questions regarding interpreation of the download.

<h2 align = "center">
 <img src="https://raw.githubusercontent.com/Fraunhofer-ITMP/alister/main/www/tut/2023%2005%2003%20Tutorial/Prot.PNG">
</h2>

<h2 align = "center">
 Analyte search
</h2>

> Research question: "I am interested in the stability of a very specific analyte. I need detailed information in how to take sample properly, as well as in how stable the analyte is under certain pre-analytical conditions."

Analyte search is used in the following way:
1. As analyte search provides a more detailed investigation of analytes, you can select them individually in the drop down menu. Fell free to type in the name of your analyte of interest, so you dont have to scroll through all of them.
2. Like in sample search you now can add you pre-analytic conditions, starting with temperature during processing. Click 'Other temperature after centrifugation' if the processing temperature varied before and after centrifugation.
3. Processing delay before and after centrifugation might affect the analyte in different ways. Put in the processing time of your samples here.
4. Like previously: If you want to define more or less strict stability thresholds, you can do that here.
5. Analyte search combines functions previously introduced in 'Sample search' and 'Protocol search'. Here, the protocol recommendation, as well as the visual output introduced in 'Protocol search' are displayed.
6. Separately, 'Analyte search' assesses your own pre-analytical input conditions. When clicking on 'Details' you can see an abstraction on the literature based  information ALISTER bases its decision on, indicating a generalized form of your input and whether the set individual threshold of fold changes due to pre-analytical stability was exceeded and in what direction.
7. All data you put in your query and the respective output can be downloaded. The tab 'Variable explanation' will answer all your questions regarding interpreation of the download.

<h2 align = "center">
 <img src="https://raw.githubusercontent.com/Fraunhofer-ITMP/alister/main/www/tut/2023%2005%2003%20Tutorial/An.PNG">
</h2>

<h2 align = "center">
 Data filtering mode
</h2>

> Research question: "I have a set of measurements of various specific analytes. I do know the conditions the samples were taken under. I want an indiciation of which analytes might be dubious due to pre-analytical conditions. I also want to filter them out of my dataset preemptively."

The data filtering mode is used in the following way:
1. First up you need to specify a file you load into the app environment. Despite being displayed in a transposed way, the file expects names rows and columns, with analytes in columns. Your data will not be touched. You could even load in an empty table, that just contains all the analyte names relevant to you. In the background your analyte names will be matched with the ones in ALISTER´s underlying database. Lipids are named after the lipid shorthand nomenclature[^2], while polar metabolites are named after their RefMet nomenclature[^3].
2. As in 'Sample search' you can specify the temperature during processing. Click 'Other temperature after centrifugation' if you want to put in different temperatures for pre- and post-centrifugation processing.
3. Put in processing delays before and after centrifugation. In most pre-analytic experiments their influence is tested separately, but will both be taken into consideration. Like in all search modes discussed prior to this, you can adjust the thresholds for pre-analytical stability individually.
4. This is your data. Each analyte is accompanied by a visual indicator giving you a hint on whether the analyte can be expected to be stable under your input conditions based on experimental data. Hover your mouse over each analyte in order to see how your input conditions were approximated by available experimental data. Click 'Citation' if you want to see the sources relevant to assessing the stability of your input.
5. A pie chart indicates the percentages of each stability category among your analytes under the given conditions.
6. The download is a bit different from previous search modes. Despite still giving you the possibility to save your input conditions, you are presented with a annotated version of your dataset. All analytes, that were evaluated instable based on experimental stability data and your input conditions are flagged as such in this variant of your dataset.

<h2 align = "center">
 <img src="https://user-images.githubusercontent.com/105213394/235858925-df8a7aef-db77-4779-ad27-9cdf30764b3a.png">
</h2>

<h1>
 Call for contribution
</h1>

ALISTER aims to aggregate analyte stability data and faciliate low-threshold access for the assessment of data and blood sample quality. This data-driven appproach off cause necessitates large amounts of experimental data. If you own data you think could fit ALISTER or know a study that you wish would be included in ALISTER´s database, please consider contacting us via rischke@med.uni-frankfurt.de

<h1>
 References
</h1>

[^1]: Future reference
[^2]: Lipid shorthand nomenclature
[^3]: [RefMet Database](https://metabolomicsworkbench.shinyapps.io/refmet_name_search/)
