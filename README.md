# Association Rules
Association Rules and Frequent Itemsets using aprori method (arules package)

Priori algorithim is a frequent itemset mining method. 
It starts by looking at frequent patterns and assumes that any subset of a frequent itemset must be frequent. 
For example, shoes, socks and milk are frequently sold together then shoes and socks should also be sold frequently. 
That is, all itemsets whose support is greater or equal to the minimum support will be considered frequent otherwise 
they will be eliminated. The algorithm generates frequent itemsets until no new frequent itemsets are identified. 
For each frequent item, it constructs its conditional pattern-base, and then its conditional FP-tree, it repeats
the process on each newly created conditional P-tree until the FP-tree is empty or only contains one path. 
Then the algorithm does the reliability of the rules by creating candidate association rules from the frequent itemset 
and then determine which ones are strong association rules based on whether they pass the confidence test.

Notes:
a.	Support is the proportion of transactions that contain an itemset ( X, Y or XY)
b.	Confident is a measure that tell us how often items in Y itemsets appear in transactions that contain X.
c.	Lift is the likehood that having X in a transaction if Y is also present



