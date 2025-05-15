# A Concerning Estrangement ? The Case of Philosophy of Biology And The Biological Sciences 
## What is this repo about?
This Github repo is to keep track of the computational work I did during my master thesis. In it, I looked at the estrangement between philosophy of biology and biology. Specifically, I used tools aminly topic modeling (see below) and citation analysis to assess the current relationship of the two endeavors. This relationship was considered under two aspects. The first one was thematic diversity, i.e. 'are philosopher interested in the biological thematics that interest biologists?'. The second one was citational, 'how are philosopher citing biology?', 'are they able to keep themselves up to date with the contemporary production in biology?'. I also looked at the visibility of both disciplines through time, that is what is the evolution of the citations going from one discipline to the other.

## HTML 
For those who want to look at the various html files where the principal results are, the simplest way is to click on the following link : 
- [FULL](https://htmlpreview.github.io/github.com/?https://github.com/JacobHamelMottiez/Estrangement/blob/main/Notebooks/Appendix_master.html)
- [CHP1](https://htmlpreview.github.io/?https://github.com/JacobHamelMottiez/Estrangement/blob/main/Notebooks/chapters_appendix/CHP1_appendix.html)
- [CHP2](https://htmlpreview.github.io/?https://github.com/JacobHamelMottiez/Estrangement/blob/main/Notebooks/chapters_appendix/CHP2_appendix.html)
- [CHP3](https://htmlpreview.github.io/?https://github.com/JacobHamelMottiez/Estrangement/blob/main/Notebooks/chapters_appendix/CHP3_appendix.html)
  
## Structure
Here you will find : 
- **Pipeline** : various files (mostly .R), that I used for data cleaning or to test ideas such as cocitation graphs, wordclouds or citation ratio through time.
- **Notebook** : This is where you find all the results I have produced. There is a mix of .Rmd, .Qmd and .ipynb. The diversity of these files reflect my learning of different resources
- through the project.
- **Archive** : Pretty straithfoward what this folder do. Note that sometimes, archives are in specific folders. I have not come accross a satisfying way to deal with my archives to this point.
- **Visualisation** : This folder is where I store .png, .svg, .jpeg and the like. These are mainly figures that I use in my thesis and that have been generated in notebooks.

## Data
Note that on my local repository, there is a folder named `Data`. For propretary issues, I cannot share the data I used here since it comes from Scopus.
However, it is noteworthy to mention that OpenAlex is becoming a very appealing data base for the kind of analysis I conducted here. You can adapt my code to make it work with OpenAlex,
just be sure to adapt the column names according to OpenAlex typology since it is not the same as Scopus (e.g. respectively title vs sourcetitle).

![](Visualisation/CHP3/map_philo_bio_update.png)


