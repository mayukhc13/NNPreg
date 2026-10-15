`NNPreg` is designed to compute the nuclear norm penalized (hereafter referred to as NNP) estimator $\tilde{A}_n$, defined as:

$$\begin{aligned} \tilde{A}_n &= \arg\min_{B\in\mathbf{R}^{p\times T}} \left( \Vert{}Y - XB\Vert{}_F^2 + \lambda_n \Vert{}\sigma(B)\Vert{}_1 \right)\end{aligned}$$

The nuclear norm of a matrix $Z$ is defined as:

$$
\|Z\|_{*} = \|\sigma(Z)\|_1 = \sum_{j=1}^{m}\sigma_j(Z),
$$

where $m:=\min\left(p,T\right)$ and $\lambda_n > 0$ is the regularization parameter.

## Installation

You can install the development version of `NNPreg` from GitHub using `devtools`:


# install.packages("devtools")
devtools::install_github("mayukhc13/NNPreg")
