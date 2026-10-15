# NNPreg

An R package for **Nuclear Norm Penalized (NNP) Regression** featuring efficient FISTA optimization and robust cross-validation.

## Estimator Definition

`NNPreg` is designed to compute the nuclear norm penalized (hereafter referred to as NNP) estimator $\tilde{A}_n$, which is defined as:

\begin{align}
\tilde{A}_n
&=
\arg\min_{B\in\mathbf{R}^{p\times T}}
\Big\{
\|Y - XB\|_F^2 + \lambda_n \|\sigma(B)\|_1
\Big\}.
\end{align}

The nuclear norm of a matrix $Z$ is defined as $\Vert{}Z\Vert{}_{*}=\Vert{}\sigma(Z)\Vert{}_1=\sum_{j=1}^{m}\sigma_j(Z)$, where $\lambda_n > 0$ is the regularization penalty parameter.

## Installation

You can install the development version of `NNPreg` from GitHub using `devtools`:


# install.packages("devtools")
devtools::install_github("mayukhc13/NNPreg")
