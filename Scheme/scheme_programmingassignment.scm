
;; 1. (fromTo k n) returns the list of integers from k to n. The size of the problem can be seen as the difference of k and n.
;; Base Case: if k = n (i.e. the size of the problem is 0), then the result is the list containing only n.
;; Hypothesis: Assume (fromTo (+ k 1) n) returns the list of integers from k+1 to n.
;; Recursive step: (fromTok k n) = (cons k (fromTo (+ k 1) n)

(define (fromTo k n)
  (cond ((= k n) ( list n))
        (else (cons k (fromTo (+ k 1) n)))))

;; 2. (removeMults m L) returns a list wherein the elements are not multiples of m. 
;; Base Case: if list L is empty, then the result is an empty list.
;; Hypothesis: Assume (removeMults m (CDR L)) returns a list of all elements of L that are not multiples of m. This list will not contain the first element of L
;; (irrespective of whether it is a multiple of m or not).
;; Recursive step: (removeMults m L) can either be (removeMults m (CDR L)), in case the first element of L, i.e., (CAR L) is a multiple of m. Else, (removeMults m L)
;; will be (cons (CAR L) (removeMults m (CDR L))), in case the first element of L is not a multiple of m.

(define (removeMults m L)
  (cond ((null? L) '())
        (else (if (= (modulo (CAR L) m) 0) (removeMults m (CDR L)) (cons (CAR L) (removeMults m (CDR L)))))))

;; 3. (removeAllMults L) returns a list wherein none of it's elements are multiples of each other.
;; Base Case: if list L is empty, then the result is an empty list.
;; Hypothesis: Assume (removeAllMults (CDR L)) returns the list of all elements of L that are not multiples of each other. This list will not contain the first element
;; of L irrespective of whether it is a multiple of any other element in the list L or not.
;; Recursive step: (removeAllMults L) = (cons (CAR L) (removeMults (CAR L) (removeAllMults (CDR L)))), i.e. remove all multiples of (CAR L) from (removeAllMults (CDR L))
;; by using the above removeMults function. After that, add (CAR L) in the first position of the resultant list.

(define (removeAllMults L)
  (cond ((null? L) '())
        (else (cons (CAR L) (removeMults (CAR L) (removeAllMults (CDR L)))))))

;; 4. (primes n) returns a list containing prime numbers less than or equal to n;
;; Base Case: if n = 0 or 1, then the result is an empty list as 0 and 1 are not prime numbers.
;; Hypothesis: Assume (primes (- n 1)) returns a list of prime numbers less than or equal to (n-1).
;; Recursive step: (primes n) = removeAllMults (APPEND (primes (- n 1)) (list n))), i.e., add n to the list of prime numbers till (n-1) if it is not a multiple of any
;; other number in the list, thus, n is added to the resultant list only if it is a prime number.

(define (primes n)
  (cond ((= n 0) '())
        ((= n 1) '())
        (else (removeAllMults (APPEND (primes (- n 1)) (list n))))))

;; 5. (maxDepth L) returns the maximum nesting depth of any element in L.
;; Base Case: if the list L is empty, then return 0 as the maximum depth.
;; Hypothesis: (maxDepth (CDR L)) returns the maximum depth of any element of L, except the first on, i.e, it does not evaluate the maximum depth of the first element. of L.
;; Recursive step: (maxDepth L) = max (+ 1 (maxDepth (CAR L))) (maxDepth (CDR L))), i.e. it calculates the maxDepth of the first element of L and compares it to the
;; maxDepth of the remaining elements of L. The maximum value amongst these two is returned as the result.

(define (maxDepth L)
  (cond ((null? L) 0)
        ((number? L) -1)
        ((symbol? L) -1)
        (else (max (+ 1 (maxDepth (CAR L))) (maxDepth (CDR L))))))

;; 6. (prefix exp) returns a prefix notation for an infix notation of an expression exp (without actually evaluating the expression exp)
;; Base Case: if the exp is an atom (number/symbol), return the expr as it is.
;; Hypothesis: (prefix (CDDR exp)) returns a prefix notation of the (CDR (CDR exp))
;; Recursive step: (prefix exp) = (if (null? (CDR exp))(CAR exp)(list (CAR(CDR exp)) (prefix (CAR exp)) (prefix (CDDR exp)))), i.e. if exp is a list, return a list that
;; contains the second element of the list as it's first element, and then the result obtained by converting the first element of exp to prefix notation, and then the
;; result obtained by converting the remainder of the exp (except it's first two elements) into prefix notation.

(define (prefix exp)
  (cond ((null? exp) 0)
        ((number? exp) exp)
        ((symbol? exp) exp)
        (else (if (null? (CDR exp))(CAR exp)(list (CAR(CDR exp)) (prefix (CAR exp)) (prefix (CDDR exp))))))
  )

;; 7. (composition fns) returns a function that is the composition of functions in fns.
;; Base Case: if fns contains only one function, return a function f which would require a parameter x to return a value.
;; Hypothesis: Assume (composition (CDR fns)) returns a function that is the composition of all functions in fns, except the first function.
;; Recursive step: (composition fns) = (lambda(x) ((CAR fns)((composition (CDR fns)) x))), i.e, return a function that is a composition of all functions
;; of fns. Thus, the first function of fns will accept as an argument the value returned by ((composition (CDR fns)) x).

(define (composition fns)
  (cond ((null? (CDR fns)) (lambda(x) ((CAR fns) x)))
        (else (lambda(x) ((CAR fns)((composition (CDR fns)) x))))))

