# Strong-Euler-Well-Composed

This repository contains the implementations of the paper entitled **Strong Euler Well-Composedness** by the authors Nicolas Boutry, Rocio Gonzalez-Diaz, Maria-Jose Jimenez and Eduardo Paluzo-Hidalgo. In that paper, a new definition of well-composedness is provided.

> **Definition** (Strong Euler Well-composedness) An nD cubical complex is Strong Euler well-composed if it has no X-critical cells.


In [Nd_strongEWC_DWC.hs](https://github.com/Cimagroup/Strong-Euler-Well-Composed/blob/main/Nd_strongEWC_DWC.hs) all the basic definitions that characterize when a configuration centered on a vertex (we used ```[0.5,...,0.5]``` as central vertex). Some of the main functions are the following:

```Haskell
dwc_aux :: Eq a => [[a]] -> Bool
dwc_aux xss = and [isthereapath (length xss) xs ys xss | ys <- xss, xs <- xss]

dwc xss =  and [dwc_aux (star 0 v xss) | v <- cell n xss]
  where
    n = length (head xss)
```
which determines if a given configuration is Digitally Well Composed or not. For example:
```Haskell
λ> dwc [[0,0,0,0],[1,1,1,1]]
False
λ> dwc [[0,0,0,0],[1,0,0,0]]
True
```

To determine if a configuration is Strong Euler Well Composed:

```Haskell
xwc xss = and [euler_star v xss == 0 | i<-[1..n], v <- cell i xss, boundary v xss]
  where
    n = length (head xss)
```

For example:
``` Haskell
λ> xwc [[0,0,0],[1,0,0]]
True
```

Then, the following lemma was checked by exhaustive computation:
> Let Q(I) be the cubical complex canonically associated to an nD picture I for n=2,3. Then Q(I) has no X-critical cells if and only if I is not Digitally Well Composed.

The code used to check this result is the following for the right implication:
```Haskell
lemma1_imp1 = [and [dwc hs && dwc (dualHcubes 2 hs)| hs <- hss1, xwc hs],and [dwc hs && dwc (dualHcubes 3 hs)| hs <- hss2, xwc hs ]] 
 where
   hss1 = all_conf [0.5,0.5] -- It is a list of all possible configurations centered in the vertex.
   hss2 = all_conf [0.5,0.5,0.5]
```
And the following for the left implication:
```Haskell
lemma1_imp2 =   [and [xwc hs  | hs <- hss1, dwc hs, dwc (dualHcubes 2 hs)], and [xwc hs  | hs <- hss2, dwc hs, dwc (dualHcubes 3 hs)]] 
 where
   hss1 = all_conf [0.5,0.5]
   hss2 = all_conf [0.5,0.5,0.5]
```
The output of both pieces of code are lists of booleans ```[Bool,Bool]``` where the first one corresponds to the 2D case and the second one to the 3D case.

Finally,
```Haskell
λ> [lemma1_imp1, lemma1_imp2]
[[True,True],[True,True]]
```
