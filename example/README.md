# Product Mix

This example is the third project from the first semester of 23/24 of the
curricular unit Analysis and Synthesis of Algorithms (ASA) from the computer
science course at Instituto Superior Técnico.

The following problem statement is the translated version of the Portuguese
original.

The goal of the example is to use linear programming to solve a maximization
objective.

## Problem Description

Professor Natalino Caracol has been hired by the company UbiquityInc in
Rovaniemi, Lapland, to develop a program that estimates the maximum profit
achievable through the production and sale of toys during Christmas.

The company produces a set of $n$ wooden toys $\{x_1, \ldots, x_n\}$ daily,
where each toy $x_i$ has a profit $l_i$. In addition to a maximum production
limit for each toy due to assembly line constraints, the company is restricted
to a maximum total quantity of toys that can be produced per day due to
constraints in the boreal forest. Additionally, this Christmas, the company
has decided to not only sell each toy individually but also special packages
containing three distinct toys, whose profit is greater than the sum of the
individual profits of the toys it comprises.

The objective is to advise Rüdolf, CEO of UbiquityInc, on the maximum daily
profit obtainable. UbiquityInc will subsequently handle the distribution problem.

## Input

The input file contains information about the $n$ products, their profit, and
the production capacity of each, as follows:

- A line containing three integers: $t$ indicating the number of different toys that can be produced, $p$ indicating the number of special packages, and $max$ indicating the maximum number of toys that can be produced per day.
- A list of $n$ lines, where each line contains two integers $l_i$ and $c_i$, indicating the profit and production capacity of toy $i$.
- A list of $p$ lines, where each line contains four integers $i$, $j$, $k$, and $l_{ijk}$, indicating the profit $l_{ijk}$ of the special package $\{i, j, k\}$, and the names of the products $i$, $j$, and $k$ that constitute it.

Any integers in a line are separated by exactly one space, with no other
characters except the end of the line.

## Output

The program should write to the output an integer corresponding to the maximum
profit that Rüdolf can obtain daily.
