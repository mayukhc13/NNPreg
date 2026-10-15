`NNPreg` is designed to compute the nuclear norm penalized (hereafter referred to as NNP) estimator $\tilde{A}_n$, defined as:

$$\begin{aligned} \tilde{A}_n &= \arg\min_{B\in\mathbf{R}^{p\times T}} \left( \Vert{}Y - XB\Vert{}_F^2 + \lambda_n \Vert{}\sigma(B)\Vert{}_1 \right) \\ &=\arg\min_{B\in\mathbf{R}^{p\times T}} \left( \sum_{i=1}^{n} \Vert{}\bm{Y}_i - \bm{X}_iB\Vert{}_2^2 + \lambda_n \Vert{}\sigma(B)\Vert{}_1 \right), \end{aligned}$$

where $\bm{Y}_i\in\mathbf{R}^{1\times T}$ and $\bm{X}_i\in\mathbf{R}^{1\times p}$ denote the $i$-th rows of $Y$ and $X$, respectively. Let $m:=\min\{p,T\}$. For any matrix $Z \in \mathbf{R}^{p\times T}$, we denote its ordered singular values as:

$$\sigma_1(Z)\geq \sigma_2(Z)\geq\cdots\geq\sigma_m(Z)\geq0.$$

The nuclear norm of a matrix $Z$ is defined as $\Vert{}Z\Vert{}_{*}=\Vert{}\sigma(Z)\Vert{}_1=\sum_{j=1}^{m}\sigma_j(Z)$, where $\lambda_n > 0$ is the regularization parameter.

## Installation

You can install the development version of `NNPreg` from GitHub using `devtools`:


# install.packages("devtools")
devtools::install_github("mayukhc13/NNPreg")
