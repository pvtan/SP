# SP

# Project Title

This project is called MoviSenti, which is a sentiment analyzer for movie reviews from Twitter. This project is created in compliance to CMSC 190 or the Undergraduate Special Problem course.

## Usage: 

1. This project requires the following
* R version 3.4.4
* RStudio version 1.1.442
* Twitter account for the Twitter API calls. Contact the developer for this prerequisite.

2. To install the required packages, run the script *installPackages.R*.
```
source('installPackages.R')
```

3. Additional Step: Since the create_matrix function of RTextTools has a problem, this additional step is added. 

Type the following command in the R console of RStudio
```
trace(RTextTools::create_matrix, edit=TRUE)
```
Then in Line 49, replace "Acronym" to "acronym".

4. To run the application, run the script *server.R*.
```
source('server.R')
```

5. To use the application, just type a movie title or a movie hashtag in the textbox.
