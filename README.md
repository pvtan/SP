# SP

# Project Title

This project is called MoviSenti, which is a sentiment analyzer for movie reviews from Twitter. This project is created in compliance to CMSC 190 or the Undergraduate Special Problem course.

## Usage: 

1. This project requires the following
* R version 3.4.4
* RStudio version 1.1.442

2. To install the required packages, run the script *installPackages.R*.

3. Additional Step: Since the create_matrix function of RTextTools has a problem, this additional step is added. 

Type the following command in the R console of RStudio
```
trace(RTextTools::create_matrix, edit=TRUE)
```
Then in Line 49, replace "Acronym" to "acronym".

4. To run the application, run the script *server.R*.
