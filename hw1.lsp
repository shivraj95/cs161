;;
;; [INPUTS]
;;		N (number) - checking if number N is in TREE
;;		TREE - an ordered Tree in the form (L m R)
;; [OUTPUT]
;;		T - if N is in TREE, NIL otherwise 
;; [EXPLANATION]
;;		BASE CASE: if only one item in Tree, check if N is equal to that item
;;		OTHERWISE: 1) Check if N is equal to the middle in list
;;				   2) Traverse right side of tree if N is larger than m
;;				   3) Traverse left side of tree if N is smaller than m


(defun TREE-CONTAINS(N TREE)
	(cond
		
		;BASE CASE
		((atom TREE) (= N TREE))

		;Check if N is equal to middle of TREE
		((= N (second TREE)) t)

		;Traverse right side of TREE if N is larger than m
        ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
        
        ;Otherwise traverse left side 
        (t (TREE-CONTAINS N (car TREE)))
  	)
)

;;
;; [INPUTS]	
;;		TREE - an ordered Tree in the form (L m R)
;; [OUTPUT]
;;		n - largest number in the tree
;; [EXPLANATION]
;;		BASE CASE: If TREE is empty or one item, then return TREE
;;		OTHERWISE: Recursively call the right side of the tree 
;;		


(defun TREE-MAX(TREE)
	(cond
		;Base case
		((atom TREE) TREE)

		;Otherwise, keep calling the right tree
        (t (TREE-MAX (third TREE)))
  	)
)

;;
;; [INPUTS]	
;;		TREE - an ordered Tree in the form (L m R)
;; [OUTPUT]
;;		L - in-ordered list of the numbers appearing in TREE
;; [EXPLANATION]
;;		BASE CASE: If TREE is empty or one item, then return TREE
;;		OTHERWISE: 1) If left and right sides of TREE are atoms, then return TREE
;;				   2) If only left is an atom, then change it into a list and recursively
;;				   call TREE-ORDER on the right side and append the left, middle, and right 
;;				   elements 
;;				   3) same logic as 2) but this time the right side is an atom
;;				   4) both sides are lists, so recursively call on the left and right side of the TREE
;;				   and use APPEND to JOIN the left, middle, and right elements 
;;				   of the TREE


(defun TREE-ORDER(TREE)
	(cond
		;Base Case
		((atom TREE) TREE)
		
		;if both sides of the tree are atoms
		((and (atom (first TREE)) (atom (third TREE))) TREE) 

		;;if only the left side is an atom
		((and (atom (first TREE)) (not (atom (third TREE)))) (append (list (TREE-ORDER (first TREE))) (list (second TREE)) 
			(TREE-ORDER (third TREE))))
        
        ;;if only the right side is an atom
        ((atom (third TREE)) (append (TREE-ORDER (first TREE)) (list (second TREE)) (list (TREE-ORDER (third TREE)))))
  		
  		;;if both sides are lists 
  		(t (append (TREE-ORDER (first TREE)) (list (second TREE)) (TREE-ORDER (third TREE))))
  	)
 )


;;
;; [INPUTS]	
;;		L - a list from which we will be getting a sublist  
;;		START - starting position of sublist in TREE
;;		LEN - length of sublist 
;; [OUTPUT]
;;		L - sublist starting at position START in TREE with length LEN
;; [EXPLANATION]
;;		BASE CASE: IF LEN is 0 or L is empty, then return NIL 
;;		OTHERWISE: 1) IF START IS 0, cons the first element of L
;;					  with the returned list from SUB-LIST on the remaining
;;					  items in the list i.e SUB-LIST((rest L ) START (- LEN 1))
;;					
;;				   2) decrement START by 1 and recursively call SUB-LIST 
;;					  for the remaining items in the list i.e (rest L)


(defun SUB-LIST(L START LEN)
	(cond
		;BASE CASE		
		((= LEN 0) NIL)
		((NULL L) NIL)

		;1)
		((= START 0) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
		
		;2)
        (t (SUB-LIST (rest L) (- START 1) LEN))
  	)
)

;;
;; [INPUTS]	
;;		L - a list which we will be splitting 
;; [OUTPUT]
;;		L* - list of two lists L1 and L2 such that L is the result of appending
;;			 L1 and L2 and length of L2 minus length of L1 is 0 or 1.
;; [EXPLANATION]
;;		BASE CASE: IF L is empty, then return NIL 
;;		OTHERWISE: 1) IF L is odd length, call sub-list to create sublists L1 and L2
;;					  such that L2 is longer by 1 and append the lists to form L*
;;					
;;				   2) Otherwise L is even, so call sub-list to create sublists L1 and L2
;;					  such that L1 and L2 split L in half, and append the lists to form L*



(defun SPLIT-LIST(L)
	(let* ((len (length L)))
		(cond
			;Base case: Nothing to the right of the tree, so return the first element
			((= len 0) NIL)

			;1)
			((oddp len) (list (SUB-LIST L 0 (/ (- len 1) 2)) (SUB-LIST L (/ (- len 1) 2) (/ (+ len 1) 2)))) 
			
			;2)
			(t (list (SUB-LIST L 0 (/ len 2)) (SUB-LIST L (/ len 2) (/ len 2)))) 
	  	)
	)
)


;;
;; [INPUTS]	
;;		TREE - binary tree in the form (L R)
;; [OUTPUT]
;;		height - length of longest path from root to farthest node in TREE 
;; [EXPLANATION]
;;		BASE CASE: IF TREE is one element or TREE is empty, then return 0 
;;		OTHERWISE: 1) If first element in TREE is an atom, then return length from current 
;;					  node to farthest node in right side of the TREE
;;					
;;				   2) If second element in TREE is an atom, then return length from current 
;;					  node to farthest node in left side of the TREE
;;				 
;;				   3) Otherwise, recursively call both sides of the tree and return whichever
;;					  one is longer



(defun BTREE-HEIGHT(TREE)
	
	(cond
		;Base case
		((atom TREE) 0)

		;1)
		((atom (first TREE)) (and  1 (+ (BTREE-HEIGHT (second TREE))  1)))
		
		;2)
		((atom (second TREE)) (and  1 (+ (BTREE-HEIGHT (first TREE)) 1)))

		;3)
		(t (let* ((left (+ (BTREE-HEIGHT (first TREE)) 1)) (right (+ (BTREE-HEIGHT (second TREE)) 1)))
				(cond 

					((<= left right) right)
					(t left)
				)
			)
		)
	)
)


;;
;; [INPUTS]	
;;		L - non-empty list of atoms 
;; [OUTPUT]
;;		TREE - binary tree with the atoms in L as nodes/leaves 
;; [EXPLANATION]
;;		BASE CASE: IF L is empty, then return NIL
;;		OTHERWISE: 1) If length of L is at most 2, then return TREE
;;				   2) If length of L is 3, then append first item of L 
;;					  with remaining two items with the list 
;;					  e.g (1 2 3) -> (1 (2 3))
;;				   2) Otherwise, split L in half by calling SPLIT-LIST
;;					  and recursively call LIST2BTREE on the left and right 
;;					  halves of L and apppend the results 

(defun LIST2BTREE(L)
	(let* ((len (length L) ))
		(cond
			((NULL L) NIL)

			;1) 
			((<= len 2) L)

			;2) 
			((= len 3) (list (first L) (rest L)))

			;3)
			(t (let* ((b_tree (SPLIT-LIST L )))

				(list (LIST2BTREE (first b_tree)) (LIST2BTREE (second b_tree)))
				)
			) 

	  	)
	)
)


;;
;; [INPUTS]	
;;		TREE - binary tree in the form (L R)
;; [OUTPUT]
;;		L - list of atoms from elements in TREE
;; [EXPLANATION]
;;		BASE CASE: IF TREE is empty or 1 element, then return TREE
;;		OTHERWISE: 1) If left and right sides of TREE are atoms, then return TREE
;;				   2) If only left is an atom, then change it into a list and recursively
;;				   call BTREE2LIST on the right side and append the left and right elements
;;				   3) same logic as 2) but this time the right side is an atom
;;				   4) both sides are lists, so recursively call on the left and right side of the TREE
;;				   and use APPEND to JOIN the left and right elements of the TREE


(defun BTREE2LIST(TREE)
	(cond
		;Base case: Nothing to the right of the tree, so return the first element
		((atom TREE) TREE)
		
		;if both sides of the tree are atoms
		((and (atom (first TREE)) (atom (second TREE))) TREE) 

		;;if only the left side is an atom
		((and (atom (first TREE)) (not (atom (second TREE)))) (append (list (BTREE2LIST (first TREE)))
			(BTREE2LIST (second TREE))))
        
        ;;if only the right side is an atom
        ((atom (second TREE)) (append (BTREE2LIST (first TREE)) (list (BTREE2LIST (second TREE)))))
  		
  		;;if both sides are lists 
		(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
	)
)


;;
;; [INPUTS]	
;;		E1 - list of numbers 
;;		E2 - list of numbers
;; [OUTPUT]
;;		T - if E1 and E2 are the same
;;		NIL - if E1 and E2 are not the same 
;; [EXPLANATION]
;;		BASE CASE: If both E1 and E2 are null, then return T
;;		OTHERWISE: 1) If both E1 and E2 are atoms, then check if they are equal
;;				   2) If E1 is an atom, return false since E2 is not an atom
;;				   3) If E2 is an atom, return false since E1 is not an atom
;;				   4) Recursively call IS-SAME on the first and remaining 
;;					  items in E1 and E2 and use 'and' to compare results



(defun IS-SAME(E1 E2)
	(cond
		;Base Case
		((and (NULL E1) (NULL E2)) t)
		
		;If both are atoms
		((and (atom E1) (atom E2)) (= E1 E2)) 

		;If only E1 is an atom
		((atom E1) nil)
		
		;If only E2 is an atom
		((atom E2) nil)

		;If both are lists, then use 'and' on the recursive calls
		(t (and (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))
	)
)


