;; huffman.scm


;; DECODING
; Return a leaf s-expression in the form of ('leaf <symbol> weight)
; 'leaf is quoted symbol for determining if the s-expression is a "leaf" object.
; <symbol> is the symbol which will be encoded.
; weight is the relative frequency that the <symbol> occurs in the encoded message
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

; define the leaf predicate, which returns true if the 
; car item is the symbol 'leaf
(define (leaf? object)
  (eq? 'leaf (car object)))

;returns the symbol of a leaf object
(define (leaf-symbol leaf)
  (cadr leaf))

; returns the weight of a leaf object
(define (leaf-weight leaf)
  (caddr leaf))

; a tree is a left node and a right node. A node contains the set of symbols and the sum of the weights of the symbols 
; in the set. make-node can be passed either nodes or leaf objects. 
(define (make-node left right)
  (list left 
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

; acquire branches from a node
(define (left-branch node)
  (car node))
(define (right-branch node)
  (cadr node))

; acquire symbols of an object, or single symbol(wrapped in a list) if the object is a leaf
(define (symbols object)
  (if (leaf? object)
      (list (leaf-symbol object)) ; return the symbol, wrapped in a list
      (caddr object)))

; aquire the weight of an object; it will be either a node or leaf object
(define (weight object)
  (if (leaf? object)
      (leaf-weight object)
      (cadddr object)))

; decode a list of ones or zeros according to the Huffman tree given.      
(define (decode bits tree)
  (define (decode-1 bits current-branch) ; called recursively
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch))) ; look in the left or right branch of a node,
					; depending on the next bit in bits
	  (if (leaf? next-branch) ; is this a leaf, rather than a node?
	      (cons (leaf-symbol next-branch) ; if so, make a list of the symbol of the leaf,
		    (decode-1 (cdr bits) tree)); and append the next symbol(s), called with decode-1 on 
	      ; the rest of the bits, using the Huffman tree.
	      (decode-1 (cdr bits) next-branch))))); otherwise, continue to traverse the tree,
					;using the bits as the map, until a leaf is reached.
  (decode-1 bits tree))

; move to the appropriate branch depending on the bit.
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE BRANCH" bit))))
	
; create a sorted list of (<symbol>, <weight>) pairs.
(define (sort-pairs pair pair-list)
  (cond ((null? pair-list) (list pair)) ; return pair in a list, if this is the end of the list
	((< (weight pair) (weight (car pair-list))) (cons pair pair-list)) ; if the weight of 
					; pair is less than the next item in the pair-list,
					; cons the pair with the pair-list
	(else (cons (car pair-list) ; otherwise, call sort-pairs with pair and the cdr of the pair list
		    (sort-pairs pair (cdr pair-list))))))

; this takes an ordered set of pairs and converts them to an ordered set of 'leaf objects
(define (make-leaves pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	; call sort-pairs on the pairs given. Create a leaf object for each pair in the pair list
	(sort-pairs (make-leaf (car pair) ; symbol
			       (cadr pair)) ; weight
		    (make-leaves (cdr pairs)))))) ; call make-leaves on the rest of the pairs

; create an example tree
(define sample-tree
  (make-node (make-leaf 'A 4)
	     (make-node 
	      (make-leaf 'B 2)
	       (make-node (make-leaf 'D 1)
			  (make-leaf 'C 1)))))

;; ENCODING ;;
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

; iterate over list, checking for symbol
(define  (symbol-in-list? symbol test-list)
  (if (null? test-list)
      #f
      (if (eq? symbol (car test-list))
	  #t
	  (symbol-in-list? symbol (cdr test-list)))))

; traverse tree appending a value if left or right is taken; if not found
; return #f if found return the value that was appended in the search 
(define (search-tree symbol tree path)
  (if (leaf? tree)
      ; is the symbol in the leaf?
      (if (eq? symbol (cadr tree))
	  path ; if so, return the path list that is the map to arrive here
	  '()) ; if not, return nothing
      ; otherwise this is a node
      (


; traverse tree until symbol is found
(define (encode-symbol symbol tree)
  ; test if tree is a leaf; if it is, do symbols match?
  (if (leaf? tree)
      (if (eq? symbol (cadr tree))
	  '() ; found
	   (error "symbol not in tree" symbol))
      ; this is a node with either: a leaf at (car tree) and a leaf at (cadr tree), in
      ; which case we are going to check (car tree) and append 1 to a list
      






  ;; (let ((object (car (cadr tree)))
  ;; 	(if (leaf? object)
  ;; 	    (if (eq? symbol (cadr object))
  ;; 		(list symbol)
		
	      
		  
	    
    