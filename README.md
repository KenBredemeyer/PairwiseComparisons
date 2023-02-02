# PairwiseComparisons
PairwiseComparisons provides maximum likelihood estimation (MLE) and other tools for pairwise comparisons data. For a complete pairwise comparisons
project, generate pairs with my pair.generator package, then use a judging interface for making comparisons (not included in my packages) and then estimate
object locations with PairwiseComparisons. Note that high performance algorithms have not been implemented at this time, so estimation times can be slow.

## Background
Pariwise comparisons is a method for forming a measurement scale of a set of objects, based on Thursotne's law of comparative judgement (1927). These 
objects can be student performances for assessment, or any other objects which are intended to be compared. Objects are firstly assigned to a set of pairs.
It is not necessary to pair each object with every other object. The pairs are typically presented with two objects side by side (one presented on the left 
and one on the right), however PairwiseComparisons does not include an interface for presenting the objects of comparison. Judges are then tasked with
choosing which of the pair is better or more according to some specified criterion.

## Estimation
A scale is formed from MLE of parameters of the Bradley-Terry-Luce model (BTL), when using `estimate_BTL`. Object locations on the scale can also be 
estimated using a pairwise comparisons version of the Logistic Measurement Function (LMF) by Humphry (2012), using `judge_lmf`. The LMF is similar to the 
two parameter logistic model (2pl; see Birnbaum) in item response theory except that 1) discrimination is not considered to be only a property of items, 
but is more general, and 2) the discrimination parameter is restricted to take the same value within sets (in the case of pairwise comparisons, sets of 
objects have the same discrimination value). `judge_lmf` computes the discrimination parameter for each judge. That is, the value of the discrimination 
parameter is equal for all comparisons made by a single judge. The benifit of `judge_lmf` is to provide an index of how well each judge can discriminate 
between the quality of each object in a pair.

## Installation
Clone `PairwiseComparisons` from GitHub, or install using 
```
remotes::install_github("KenBredemeyer/PairwiseComparisons")
```

## Usage
Import data as a `data.frame`, with the variables `judge, Criteria, Item, Item.1, Selected`. `Item` and `Item.1` are the objects paired (Left and Right).
`Seelected` is the object judges to be of higher quality according to the criterion specified in the pairwise design, one of `Item`, or `Item.1`.
Alternatively, import the data matrices, in which the columns and rows represent objects (`colnames == rownames`).

The imported data.frame can be converted to a data matrix using
```
dm <- pairs_format(df)
```
Then, estimate using 
```
estimate_BTL(dm)
```

## References

Birnbaum, A. (1968) Some Latent Trait Models and Their Use in Inferring an Examinee’s Ability. In: Lord, F.M. and Novick, M.R., Eds., 
Statistical Theories of Mental Test Scores, Addison-Wesley, Reading, 397-479.

Bradley, R. A., and Terry, M. 1952. The rank analysis of incomplete block designs: I. the method of paired comparisons. Biometrika, 39(3):324–345.

Humphry, S. M. (2012). Item Set Discrimination and the Unit in the Rasch Model. Journal of Applied Measurement, 13(2), 165-180.

Humphry, S. M. (2005). Maintaining a common arbitrary unit in social measurement. Doctoral thesis: 
https://librarysearch.murdoch.edu.au/discovery/delivery?vid=61MUN_INST:ResearchRepository&repId=12129673560007891#13130840450007891

Luce, R.D. (1959). Individual choice behavior. New York: Wiley.

Thurstone, L.L. (1927). A law of comparative judgement. Psychological Review, 34, 278-286.
