# DM-Reuters21578

The assignment practices text classification, clustering and topic models for Reuters-21578 data set.

Note: all of the generated datasets for each task are stored in the folder as .csv files.

Task 1: Explore the data and undertake any cleaning/pre-processing

The implementation of task 1 utilizes six libraries: NLP, openNLP, openNLPmodelss.en, tm, tau and koRpus, most of which can be easily installed by the command install.packages(“package_name”). The point to note is the package ‘openNLPmodels.en’ was removed from the CRAN repository, but an alternative way to install is typing the following command at R console: install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz", repos = NULL, type = "source")

The procedure of tagging part-of-speech and lemmatisation for text requires a local treetag library, which can be retrieved from http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/ Another alternative way downloading the package where necessary environment has been set up is to retrieve the package from my Dropbox: https://www.dropbox.com/sh/jwu96dspzol1ows/AAB0VbZ6cDTVBliqgny4QaDka?dl=0 Then place the package into R working directory in order to efficiently use TreeTagger functions.

Task 2: Obtain feature representations of the documents/news articles. Try a version using topic models as features. Try topic models on their own as well as in conjunction with other features.

This task requires three libraries, namely: NLP, tm and topicmodels. The value of TFIDF each feature is calculated in the first place; and then topic models are obtained by LDA algorithm. Moreover, the features selected by TFIDF are combined with the features from LDA to produce a new version of features.

Task 3: Build classifiers to predict the TOPICS tags for documents. Focus on the 10 most populous classes, namely: (earn, acquisitions, money-fx, grain, crude, trade, interest, ship, wheat, corn).

In order to classify multiclass data, the duplicates of the document with a different class assignment are created. SVM, Naive Bayes and Random Forest classifier are applied to classify the data set. Furthermore, a k-fold cross-validation function is built to further evaluate the classifications.

Task 4: Consider all the data and use the best performing features to represent the documents and apply three clustering algorithms.

This task requires fpc library. Distance based Hierarchical clustering, distance based predictive clustering K-means and density based DBSCAN clustering algorithms are applied to cluster the features.

