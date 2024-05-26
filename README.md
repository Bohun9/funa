## funa

The framework for data flow analysis of a simple functional language written for educational purposes.
User shall only specify a data type for the analyzed property and generate constraint system that will
be solved and results will be presented in the graph format.

### Usage
1. Build the project.
```
dune build
```
2. You can see the results of the analysis on the specific program using an interactive graph viewer like xdot.
```
dune exec -- src/bin/funa.exe cfg examples/id.fun 1 | xdot -
```
```
dune exec -- src/bin/funa.exe analyse examples/id.fun call_analysis 1 | xdot -
```

### Remarks

For educational purposes I find standard K-CFA algorithm not appropriate,
because it would obscure output graph with environments too much.
This [paper](https://arxiv.org/pdf/1311.4231) presents smart and easy
way to get the polynomial time and only uses call-strings contexts (but of course it is less precise).

The main idea is that there is a difference between context $\delta_1$, in which a closure was created
and a context $\delta_2+l$, in which this function will be executed. The problem arises, when a closure
remembers a value $v$ assigned to some variable $x$. When $x$ is refering to the closest variable binding
then we have $v \in \rho(x)(\delta_1)$ because of basic application constraints).
"Induction step" is the new rule they added to an application case:
$$[e_1^{l_1} e_2^{l_1}]$$
$$\forall (\lambda x. e_0, \delta_0) \in C(e_1)(\delta_2) \ \forall y \in FV(\lambda x.e_0)
\ \rho(y)(\delta_0) \subseteq \rho(y)(\delta_2+l)$$
