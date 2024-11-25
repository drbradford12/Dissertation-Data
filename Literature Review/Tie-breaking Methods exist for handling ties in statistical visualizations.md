---
output:
  pdf_document: default
  html_document: default
---
### Overall Summary of the Document

This document looks at several other ways to break ties, including the Mid-Rank Method, dense ranking, Monte Carlo rank correction, pairwise comparisons, and sequential tie-breaking based on a secondary criterion. By changing ranks to avoid bias, these methods ensure that rankings are fair and accurate. They can be used in various situations, such as hypothesis testing, regression models, and machine learning. This document also discusses the problems with methods like random assignment and first-come, first-served, which are not generally suggested because they are not deterministic and need robust statistics.

The document also talks about different ways to show tied data, like jittering, bee swarm plots, and scatterplots. These methods change where the data points are placed so they don't overlap. This makes the data distribution more transparent, especially for large datasets. Some examples are using scatterplot matrices and time-segmented scatterplots to show data that is either very complex or changes over time and using techniques like jittering and Voronoi partitioning to make the connections between the points seen more clear. Advanced statistical methods, such as kernel density estimation and Bayesian inference, are discussed in terms of how they can predict how users will interact with linked data in visualizations.

### Tie-breaking methods exist to handle ties in statistical visualizations.

1. Critical Examination of Length-of-Stay Comparisons ([Reference](https://pubmed.ncbi.nlm.nih.gov/28712630/)): Discusses statistical methods for comparing distributions with ties, emphasizing the importance of rank adjustments to ensure fair comparisons.  
2. Kruskal-Wallis Test Application ([Reference](https://www.scientific.net/AMM.611.115)): A non-parametric method that accounts for ties by ranking the data and adjusting the test statistic accordingly.  
3. Unsupervised Machine Learning for Anomaly Detection (Reference): This paper introduces methods for ranking anomalies in unsupervised learning, which help break ties in large datasets.  
4. Kendall's Tau and Spearman’s Rank Correlation ([Reference](https://doi.org/10.1080/01621459.2015.1016226)): Provides rank-based statistical techniques that correct for ties, ensuring that visualized data maintains proper relational order.  
5. Monte Carlo Rank Correction ([Reference](https://arxiv.org/abs/1912.12880)): Discusses the use of Monte Carlo methods to simulate ranks for tie-breaking in statistical visualizations.  
6. Kruskal-Wallis Variations ([Reference](https://link.springer.com/content/pdf/10.1186/s12874-021-01410-x.pdf)): Variations of the Kruskal-Wallis test that explicitly handle ties through corrected rank-sum procedures.  
7. Wilcoxon Rank-Sum Test ([Reference](https://pubmed.ncbi.nlm.nih.gov/22006667/)): Discusses a variant of the Wilcoxon test, which adjusts for ties in rank comparisons to avoid bias in the visualized data.  
8. Statistical Regression Techniques (Reference): This section explores how regression models incorporate rank adjustments to handle ties, improving the clarity of regression-based visualizations.  
9. Permutation Testing with Rank Adjustments (Reference): This method uses permutations to correct for ties in visualizations based on ranked data.  
10. Generalized Jonckheere Test ([Reference](https://arxiv.org/pdf/1205.0534)): This test is designed for ordinal data and handles tied observations by adjusting ranking methods.  
11. Rank Regression Techniques ([Reference](https://www.mdpi.com/1996-1073/15/24/9397/pdf)): A regression approach that handles ties by modifying rank orders in datasets with overlapping values.

### Overview of Tie-Breaking Methods Used on Numerical Data

1. Average Ranks (Mid-Rank Method)
  
    * Description: In this method, when values are tied, their ranks are replaced by the average of the ranks they would have received without ties.  
    * Use Case: Commonly used in non-parametric tests such as Kruskal-Wallis and Wilcoxon rank-sum tests.  
    * Reference: Rafael Meléndez Surmay, \*Kruskal-Wallis Test for Functional Data Based on Ranks\* (2024).

2. Dense Ranking
  
    * Description: Tied observations receive the same rank in dense ranking, but the following distinct observation gets the subsequent rank without gaps. For example, if two items tie for 2nd place, the following unique item is ranked 3rd.  
    * Use Case: Useful in leaderboard rankings or ordinal data where all ranks must remain tightly packed.  
    * Reference: Iwona Dorota Czechowska, \*INFORMATION AS A CONSUMER PROTECTION INSTRUMENT\* (2024).

3. Random Assignment

    * Description: Tied values are randomly assigned ranks, with each tied value having an equal chance of receiving any possible ranks.  
    * Use Case: This method can be used when ties are rare, and breaking them randomly won’t affect the overall analysis. However, it lacks statistical robustness.  
    * Reference: Not commonly recommended in academic references due to its non-deterministic nature

4. First-Come, First-Served
  
    * Description: This method assigns the lower rank to the first tied value(s) encountered, followed by the next tied value(s).  
    * Use Case: Suitable when the sequence of data entry or occurrence matters, such as in queueing systems or time-based analysis.  
    * Reference: This approach is often implemented in systems handling sequential data but needs to be more widely cited in formal academic literature.

5. Statistical Significance-Based Tie Breaking

    * Description: Involves breaking ties by considering additional statistical metrics, such as p-values, confidence intervals, or effect sizes, which provide a more refined ranking.  
    * Use Case: Useful in hypothesis testing, clinical trials, and research where statistical certainty is required.  
    * Reference: Alireza Ansariyar, \*Accident Response Analysis of Six Different Types of Vehicle Collisions\* (2023).

6. Pairwise Comparisons

    * Description: Tied observations are compared directly to one another in a head-to-head comparison, and the one with a higher “win” count gets a higher rank.  
    * Use Case: Widely used in decision-making scenarios like tournaments or preference rankings.  
    * Reference: Iwona Dorota Czechowska, \*INFORMATION AS A CONSUMER PROTECTION INSTRUMENT\* (2024)

7. Monte Carlo Rank Correction
  
    * Description: This method uses Monte Carlo simulations to generate random samples of rankings. Ties are broken probabilistically by generating multiple possible rank orders.  
    * Use Case: Suitable for large datasets where ties occur frequently and precision is needed.  
    * Reference: Xuejian Wang, \*Research on 3D Visualization of Real Scene in Urban Subway Engineering\* (2023).

8. Sequential Tie-Breaking (Based on a Secondary Criterion)
  
    * Description: When ties occur, a secondary variable is used to differentiate between the tied values (e.g., in sports, two teams may be tied in points but are differentiated by goal difference).  
    * Use Case: Useful in multi-criteria ranking systems like sports or composite indexes.  
    * Reference: It is often used in real-world ranking systems like sports but rarely discussed in statistical literature.

9. Lexicographical Ordering

    * Description: Breaks ties using a lexicographical order (alphabetical or based on another secondary variable), often used when multiple variables are ranked in order.  
    * Use Case: Suitable for datasets where a secondary variable (e.g., name, timestamp) is available for use in tie-breaking.  
    * Reference: Often referenced in sorting algorithms and optimization literature but not commonly used in statistical tests.

10. Penalty Functions

    * Description: Adds a minor penalty to one or more tied values to break the tie artificially. The penalty is often chosen arbitrarily but ensures that tied observations are ranked differently.  
    * Use Case: Used in regression models or machine learning algorithms where exact ties can cause model overfitting.  
    * Reference: Used more in machine learning contexts than traditional statistical methods.

The most statistically robust methods are the Mid-Rank Method, Statistical Significance-Based tie-breaking, and Pairwise Comparisons. These methods ensure the data's underlying structure is respected. Monte Carlo Simulations also provide high precision, especially in large datasets with frequent ties.

The **Mid-Rank Method** is widely accepted and reliable if you are dealing with non-parametric tests. **Pairwise Comparisons** or **Sequential Tie-Breaking** using a secondary criterion can offer the most logical and fair results for decision-making processes. When computational resources are available, Monte Carlo Rank Correction is ideal for larger datasets.

### Statistical Visualizations for handling ties

Several visualization methods can handle tied data effectively by representing overlapping or identical data points. Here are some common approaches:

1. **Jittering**: Slightly shifting tied data points randomly to prevent overlap, making the distribution more straightforward. This is commonly used in scatterplots.  
2. **Swarm Plot**: An alternative to jittering, this method arranges data points along an axis to minimize overlap while still indicating density.  
3. **Dot Plot**: Instead of stacking points directly on top of one another, dot plots displace tied points vertically (or horizontally) to show their frequencies.  
4. **Boxplots with Dot Overlay**: Combining a boxplot (which shows the central tendency and spread) with jittered or stacked individual data points overlaid can reveal ties.  
5. **Stacked Histogram or Bar Plot**: Stacking bars or histograms by frequency helps visualize tied values without overlap in categorical data visualization.  
6. **Violin Plot**: This combines density estimation and boxplots, where ties and data density are shown symmetrically, offering insights into the distribution of ties.

#### Scatter Plot Versions of Handling Ties

 1. [Visualizing Multivariate Spatial Correlation with Dynamically Linked Windows](https://www.academia.edu/download/34468636/02-t-8.pdf)

    * **Authors**: L Anselin, I Syabri, O Smirnov  
    * **Publication Info**: CSISS Workshop, 2002  
    * **Summary**: This paper focuses on visualizing multivariate spatial correlations using scatterplots. The key feature is dynamically linked windows, which allow users to interact with scatterplots to understand spatial data relationships better. The paper emphasizes how scatterplots can manage tied data points by organizing data centrally and adjusting for overlaps in a spatial context.  
    * **Statistical Methodology:** This paper employs Moran’s I, a measure used for spatial autocorrelation, to represent the relationship between spatial units in the scatterplot. Spatial correlation analysis is extended through dynamically linked views, which allow users to compare spatial scatterplots across multiple variables. To handle numerical ties in spatial data, the method adjusts the weighting of variables based on proximity and correlation strength. Ties are visualized by centralizing and changing the spatial positions of data points to avoid overlaps while preserving correlation patterns.


  
 2. [Time-Segmented Scatter Plots: A View On Time-Dependent State Relations In Discrete-Event Time Series](https://www.researchgate.net/profile/Arne-Koors/publication/301372679_Time-Segmented_Scatter_Plots_A_View_On_Time-Dependent_State_Relations_In_Discrete-Event_Time_Series/links/5a6a139f0f7e9b01f3efc55a/Time-Segmented-Scatter-Plots-A-View-On-Time-Dependent-State-Relations-In-Discrete-Event-Time-Series.pdf)

    * **Authors**: A Koors, B Page  
    * **Publication Info**: ECMS, 2015  
    * **Summary**: This research introduces a new type of scatterplot, the time-segmented scatterplot, to visualize time-dependent state relations in event-based data series. It focuses on how ties are handled across different time segments, distributing data points that would otherwise overlap by adjusting for local densities. This technique allows a clear visualization of time-segmented events where numerical ties occur.  
    * **Statistical Methodology**: The methodology here focuses on dividing data into time segments, each treated as a cross-sectional snapshot of the dataset. The scatterplots are segmented by discrete-event time series, where local densities and overlaps are handled by adjusting segment boundaries. Ties are managed by assigning equal weights to tied points within the same time window, using density-based clustering methods to group tied data and reduce overdraw in the visual representation.

 3. [Predicting Intent Behind Selections in Scatterplot Visualizations](https://journals.sagepub.com/doi/abs/10.1177/14738716211038604)

    * **Authors**: K Gadhave, J Görtler, Z Cutler, C Nobre  
    * **Publication Info**: Information Visualization, 2021  
    * **Summary**: This paper examines how scatterplot selections (clusters or outliers) are tied to specific intents. By predicting user behavior when selecting data points, the study provides insights into how scatterplot visualizations can account for numerical ties by predicting the user's intent behind selecting overlapping data. This is relevant in fields where users frequently explore scatterplots for decision-making.  
    * **Statistical Methodology**: This paper uses Bayesian inference and machine learning models to predict user intent in selecting scatterplot regions. It introduces a probabilistic model that predicts the likelihood of a user choosing specific points based on their proximity to clusters, outliers, or areas of interest. The model accounts for numerical ties by incorporating kernel density estimation (KDE). This helps smooth the distribution of tied points and ensures that user selections in dense regions are correctly captured and visualized.

 4. [Visual Exploration of Large Scatter Plot Matrices by Pattern Recommendation Based on Eye Tracking](https://www.researchgate.net/profile/Lin-Shao-16/publication/314519317_Visual_Exploration_of_Large_Scatter_Plot_Matrices_by_Pattern_Recommendation_based_on_Eye_Tracking/links/59d35822aca2721f436ca912/Visual-Exploration-of-Large-Scatter-Plot-Matrices-by-Pattern-Recommendation-based-on-Eye-Tracking.pdf)
  
    * **Authors**: L Shao, N Silva, E Eggeling, T Schreck  
    * **Publication Info**: ACM Workshop, 2017  
    * **Summary**: This paper investigates how scatter plot matrices (SPLOMs) can visualize large, high-dimensional datasets. It uses eye tracking to recommend patterns based on user exploration, allowing for detecting trends even when numerical ties or clusters occur. The eye-tracking data helps to navigate through ties in large scatterplot matrices.  
    * **Statistical Methodology**: This study uses eye-tracking data to recommend large scatterplot matrices (SPLOMs) patterns. The method applies multivariate analysis to understand where users focus their gaze, using the Gini coefficient and Shannon entropy to evaluate how well users distinguish between patterns. For numerical ties, heatmaps derived from eye-tracking data highlight regions where tied data points are clustered. These are refined using hierarchical clustering to ensure clear separation of visually dense areas.

 5. [High-Dimensional Visual Analytics: Interactive Exploration Guided by Pairwise Views of Point Distributions](https://ieeexplore.ieee.org/abstract/document/1703359/)

    * **Authors**: L Wilkinson, A Anand  
    * **Publication Info**: IEEE Transactions, 2006  
    * **Summary**: The authors present a methodology for interactive exploration of high-dimensional data through pairwise scatterplots. This paper discusses how point distributions reveal scatterplot relationships, especially when data points share identical values (ties). By correcting statistical features, the method adjusts scatterplots to deal with monotonicity and ties.  
    * **Statistical Methodology**: The authors introduce a method based on Spearman’s rank correlation (corrected for ties) to assess the monotonic relationships between variables. This approach applies pairwise scatterplots to visualize point distributions across multiple dimensions. When ties occur, the scatterplots are adjusted using tie-breaking rules based on proximity and frequency, ensuring that tied points are spread in the scatterplot while maintaining the overall statistical integrity of the distribution.

 6. [Gatherplot: A Non-Overlapping Scatterplot](https://arxiv.org/pdf/2301.10843)
  
    * **Authors**: D Park, N Elmqvist  
    * **Publication Info**: arXiv, 2023  
    * **Summary**: Gatherplot introduces a novel scatterplot design that ensures non-overlapping data points, addressing the issue of ties. This paper explores various design strategies to separate tied data points, preventing overdraw and clutter in scatterplot visualizations. It also discusses jittering techniques and alternative visual encodings to handle large datasets.  
    * **Statistical Methodology**: The Gatherplot method addresses the problem of scatterplot overdraw by introducing jittering and Voronoi partitioning techniques. It employs density-based spatial clustering to ensure that tied points (or points with the same values) are separated visually without distorting the data. The methodology uses K-means clustering to partition overlapping points and assign them to distinct visual regions while preserving the original data relationships.

 7. [Extending a Scatterplot for Displaying Group Structure in Multivariate Data: A Case Study](https://www.ajol.info/index.php/orion/article/download/34254/6252)

    * **Authors**: S Gardner, NJ Le Roux, T Rypstra, JPJ Swart  
    * **Publication Info**: ORiON, 2005  
    * **Summary**: This case study examines how scatterplots can be extended to display group structures in multivariate datasets. By addressing how small sample sizes and ties impact the visualization, the authors propose a modified scatterplot that maintains clarity even when data points overlap. They explore strategies for separating tied points in complex datasets.  
    * **Statistical Methodology**: This paper presents a modified canonical variate analysis (CVA) method to extend traditional scatterplots for visualizing group structures. When dealing with small sample sizes or ties, the authors propose using Mahalanobis distance and centroid-based clustering to separate tied points. They also suggest adjusting the scatterplot’s scale and axis dimensions to improve the visualization of data clusters and ensure that tied points are displayed clearly in multivariate data visualizations.
  
