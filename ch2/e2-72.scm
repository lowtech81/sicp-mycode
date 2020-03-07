; Exercise 2.72.
;
; Consider the encoding procedure that you designed in exercise
; 2.68. What is the order of growth in the number of steps needed to encode a
; symbol? Be sure to include the number of steps needed to search the symbol
; list at each node encountered. To answer this question in general is
; difficult. Consider the special case where the relative frequencies of the n
; symbols are as described in exercise 2.71, and give the order of growth (as a
; function of n) of the number of steps needed to encode the most frequent and
; least frequent symbols in the alphabet.
; ------------------------------------------------------------

; Steps needed to search the symbol is O(n), in 2.68 the tree depth is logn. 
; So in a general case，the average growth is O(nlogn).
; But the in 2.71, the tree depth is n. So for the least frequant symbol, the growth is O(n^2).
; For the most the growth is O(n).