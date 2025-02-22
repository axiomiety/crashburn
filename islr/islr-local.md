
## 9.2 Support Vector Classifiers

### 9.2.1 Overview of the Support Vector Classifier
- hyperplane classifier will necessarily perfectly classify all training observations
	- overfitting
- distance of observation to hyperplane can be seen as a measure of our confidence
- we may not want to classify everything perfectly
	- greater robustness to individual observations
	- better classifications of most training observations
- support vector classifier, a.k.a. sofrt margin classifier

### 9.2.2 Details of the Support Vector Classifier
- hyperplane vs margin
- optimisation problem
	- maximise $\beta_0,\ldots,\beta_p,\epsilon_1,\ldots,\epsilon_m,M$
	- subject to:
		- $\sum_{j=1}^p\beta_j^2=1$
		- $y_i(\beta_0+\beta_1x_{i1}+\ldots +\beta_px_{ip}) \ge M(1-\epsilon_i)$
		- $\epsilon_i \ge 0, \sum_{i=1}^n \epsilon_i \le C$
	- $C$ is a tuning param
		- budget for the amonut that the margin can be violated
	- $M$ is the width of the margin
	- $\epsilon_i$ are slack variables
- if $C\gt0$, at most $C$ observations can be on the wrong side
- only observations that lie on the margin will affect the hypderplane!
- large $C$ means low variance (many observations are support vectors)
- robust to observations that are far away from the hyperplane
	- vs LDA, but closer to logistic regression

## 9.3 Support Vector Machines

### 9.3.1 Classification with Non-Linear Decision Boundaries
- address non-linearity by enlarging the feature space
	- function of the predictors such as quadratic terms
	- from $X_1,\ldots,X_p$ to $X_1.X_1^2,\ldots,X_p,X_p^2$ for instance

### 9.3.2 The Support Vector Machine
- accommodate non-linear boundaries
- solving the optimisation problem requires inner products of the observations (vs the observations themselves)
- inner product $\langle a,b \rangle = \sum_{j=1}^r a_ib_i$ 
- $f(x)=\beta_0+\sum_{i=i}^n \alpha_i \langle x,x_i \rangle$
- $\alpha_i$ is non-zero only for support vectors
- replace the inner product with a generalisation
	- $K(x_i,x_i')$
	- $K$ is called the kernel
	- example of a linear kernel is $K(x_i,x_i')=\sum_{j=1}^px_{ij}x_{i'j}$
	- example of a polynomial kernel is $K(x_i,x_i')=(1+\sum_{j=1}^px_{ij}x_{i'j})^d$
- using $K$ the equation becomes:
	- $f(x)=\beta_0+\sum_{i\in S}^n \alpha_i K(x,x_i)$
	- where $S$ is the set of support vectors
- radial kernel:
	- $\text{exp}(-\gamma \sum_{j=a}^p(x_{ij}-x_{i'j})^2)$
	- local behaviour - only nearby training observations have an effect on the test observation
### 9.3.3. An Application to the Heart Disease Data
- heh... not sure i fully understood the comparison, ROC curves really...

## 9.4 SVMs with More than Two Classes
- extend to an arbitrary number of classes
- 2 main proposals

### 9.4.1 One-Versus-One Classification

- construct $K\choose2$ 
- assign the test obs to the class it was most frequently assigned to

### 9.4.2 One-Versus-All Classification
- fit $K$ SVMs, each time comparing one of the $K$ classes to the remaining $K-1$ classes
- $\beta_{0k},\beta_{1k},\ldots,\beta_{pk}$ - parameters from fitting an SVM comparing the $k$th class to all the others

## 9.5 
- SVM was introduced in the mid-90's
- we can rewrite the support vector classifier as something in the form of a loss and penalty
- in SVM the loss is called a hinge loss
- there's an extension of SVM for regression (so for quantitative vs qualitative response)
