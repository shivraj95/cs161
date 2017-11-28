
(defun init_state (n)
    (cond
    	;If there are 0 values in the list, then return nil
        ((= n 0) nil)
        ; otherwise add a 0 to the list and recusursively call init-state
        (t (cons 0 (init_state (- n 1))))
    )
)


(defun sat? (n delta)
    (sat_checker n delta (init_state n))
)


(defun abs_val (lit)
    (cond
        ((> lit 0) lit)
        (t (- lit))
    )
)


;Returns true if literal is a member of clause 
(defun member? (clause literal)
	(cond
		((null clause) nil)
		((= (car clause) literal) t)
		(t (member? (rest clause) literal))
	)
)


; Updates value of literal in curr_values
(defun assign_value (curr_values literal)
    (let*
        (
            (pos (abs_val literal))
        )
        (append (butlast curr_values (- (length curr_values) (- pos 1))) (list literal) (cdr (nthcdr (- pos 1) curr_values))) 
    )
)

;; [INPUTS]
;;    clause - list of literals 
;;	  literal - constant integer 
;; [OUTPUT]
;;    S' - a new clause with the literal removed
;; [EXPLANATION]
;;    Base Case: if clause is null, then return nil
;;	  1) if literal equals first value in clause, then remove and recursively call remove_literal 
;;    2) otherwise: append first item with recursive call to remove_literal on the rest of the list
(defun remove_literal (clause literal)

	(cond 
		;Base Case
		((null clause) nil)
		;1
		((= (first clause) literal) (remove_literal (rest clause) literal))
		;2
		(t (cons (first clause) (remove_literal (rest clause) literal)))
	)
)

;; [INPUTS]
;;    cnf - list of lists or can be seen as lists of clauses 
;;	  literal - constant integer 
;; [OUTPUT]
;;    t - if there is a contradiction for that literal in the cnf  
;;	  OR 
;;	  f - if there is no contraadiction for that literal in the cnf 
;; [EXPLANATION]
;;    Base Case: if cnf is null, then return nil
;;	  1) if size of first clause in cnf is 1 and that item is equal to literal, then return true
;;	  2) otherwise: keep checking remaining clauses in cnf with recursive call 
(defun check_unit_contradiction (cnf literal)
	(cond
		;base case
		((null cnf) nil)
		;1
		((and (= (length (car cnf)) 1) (= (caar cnf) literal)) t)
		;2
		(t (check_unit_contradiction (rest cnf) literal))
	)
)

;; [INPUTS]
;;    cnf - list of lists or can be seen as lists of clauses 
;;	  literal - constant integer 
;; [OUTPUT]
;;    t - if there is a contradiction for that literal in the cnf  
;;	  OR 
;;	  f - if there is no contradiction for that literal in the cnf 
;; [EXPLANATION]
;;    Base Case: if cnf is null, then return nil
;;	  1) if size of first clause in cnf is 1, then check if negation of that literal is in the remaining clauses with check_unit_contradiction
;;	  2) otherwise: keep checking remaining clauses in cnf with recursive call 

(defun check_contradiction (cnf literal)
	(cond
		;Base Case 
		((null cnf) nil)	
		((= (length (car cnf)) 1) 
			(cond
				((check_unit_contradiction cnf (- literal)) t)
				(t (check_contradiction (rest cnf) literal))
			)
		)
		(t (check_contradiction (rest cnf) literal))
	)
)

;; [INPUTS]
;;    cnf - list of lists or can be seen as lists of clauses 
;;	  literal - constant integer 
;; [OUTPUT]
;;    cnf' - new list of lists such thatall lists that contain literal are removed from cnf
;; [EXPLANATION]
;;    Base Case: if cnf is null, then return nil
;;	  1) if literal occurs more than once in first list of cnf, then remove list and recursively call remove_clauses with remaining lists
;;    2) otherwise: append first list with recursive call to remove_clauses on the remaining lists

;----------------------------Might need for ple?------
(defun remove_clauses (cnf literal)
	(cond 

			((null cnf) nil)

			((member? (first cnf) literal) (remove_clauses (rest cnf) literal))
			
			(t (cons (first cnf) (remove_clauses (rest cnf) literal)))
	)
)



;; [INPUTS]
;;    clauses - list of lists or can be seen as lists of clauses 
;;	  literal - constant integer 
;; [OUTPUT]
;;    cnf' - new list of lists such that 1) lists that contain negative of literal are removed from clauses  
;;									     2) clauses with literal are removed
;; [EXPLANATION]
;;    Base Case: if clauses is null, then return nil
;;	  1) if literal occurs more than once in first clause in list of clauses, then remove clause and recursively call simplify_clauses on remaining clauses
;;    2) otherwise return appended list of: 1)result of removing negation of literal from the first clause in list of clauses 2)recursive call of simplify_clauses on remaining clauses 
(defun simplify_clauses (clauses literal)
	(cond 
			;Base Case
			((null clauses) nil)
			;1) 
			((member? (first clauses) literal) (simplify_clauses (rest clauses) literal))
			;2)
			(t (cons (remove_literal (first clauses) (- literal)) (simplify_clauses (rest clauses) literal)))
	)
)



;; [INPUTS]
;;    explored - list of lists or can be seen as lists of clauses 
;;	  remaining - constant integer 
;;	  curr_values
;; [OUTPUT]
;;    cnf' - new list of lists such that 1) lists that contain negative of literal are removed from clauses  
;;									     2) lists with literal removed
;; [EXPLANATION]
;;    Base Case: if clauses is null, then return nil
;;	  1) if literal occurs more than once in first clause of clauses, then remove clause and recursively call simplify_clauses on remaining clauses
;;    2) otherwise: return appended list of: 1)result of removing negative of literal from first clause 2)recursive call of simplify_clauses on remaining clauses 

(defun unit_propagation (explored remaining curr_values)
	(cond
		;If we have nothing left to explore, then return a list of the explored clauses and current set of values for the variables 
        ((null remaining) (list explored curr_values))
        ((= (length (first remaining)) 1) 
        	(let*
	            (
	            	(literal (caar remaining))

	            	(is_inconsistent (check_unit_contradiction remaining (- literal)))

	            	;new_clauses remaining
	                (new_clauses (simplify_clauses (append explored remaining) literal))
	            )
	            (cond
	            	;check for inconsistencies in new list of clauses
	                (is_inconsistent nil)
	                	
	                (t (unit_propagation nil new_clauses (assign_value curr_values literal)))

	            )	                		
	        )
         )
        (t (unit_propagation (append explored (list (car remaining))) (rest remaining) curr_values))
    )
)

;; [INPUTS]
;;    delta - list of lists or can be seen as lists of clauses 
;;	  literal - constant integer
;; [OUTPUT]
;;    n - number of times literal occurs in delta
;; [EXPLANATION]
;;    Base Case: if delta is null, then return 0
;;	  1) Return sum of 1) count of literal in first list in delta and 2) count of literal in rest of the lists in delta with recursive call 

(defun count_literal (delta literal)
	(cond 
		;Base Case
		((null delta) 0)

		(t  (+ (count literal (first delta)) (count_literal (rest delta) literal))) 
	)
)

;; [INPUTS]
;;    delta - list of lists or can be seen as lists of clauses 
;;	  n - variable we are looking at 
;;	  curr_values - current assigned values
;; [OUTPUT]
;;    cnf' - new list of lists such that 1) any lists with pure literals are removed from delta
;;									     
;; [EXPLANATION]
;;    Base Case: if delta is null or we are look at the zeroth variable, then return nil
;;	  1) if the nth variable is equal to 0 in curr_values, then 
;;			compare counts of positive and negative of nth variable in delta, and if it is pure, then remove any clauses from delta with n
;;    2) check next variable

(defun pure_elim (delta n curr_values)
	(cond 
		;Base Cases
		((null delta) (list delta curr_values))
		((= n 0) (list delta curr_values))

		;if n is unassigned i.e equal to 0 in curr_values
		((= (nth (- (abs_val n) 1) curr_values) 0) 
			(let 
				(
					(pos_count (count_literal delta n))
					(neg_count (count_literal delta (- n)))
				) 
				(cond 

					((= neg_count 0) (cond 
									;If nth variable occurs 0 times in both, then it has no impact on delta, so assign it a value and check next variable
									((= pos_count 0) (pure_elim delta (- n 1) (assign_value curr_values n)))
									;n is a pure literal in delta
									((> pos_count 0) (pure_elim (remove_clauses delta n) (- n 1) (assign_value curr_values n)))))
					;-n is a pure literal in delta
					((= pos_count 0) (pure_elim (remove_clauses delta (- n)) (- n 1) (assign_value curr_values (- n))))

					;if n and -n are not a pure literal in delta
					(t (pure_elim delta (- n 1) curr_values))
				)
			)
		)
		(t (pure_elim delta (- n 1) curr_values))	
	)
)


;Fills any zeros in values with n + 1
(defun fill_zeros (values n)
	(cond 
		((null values) nil)
		((= n (length values)) values)
		((= (nth n values) 0) (fill_zeros (assign_value values (+ n 1)) (+ n 1)))
		(t (fill_zeros values (+ n 1)))
	)
)

;Gets first non zero term in new_values
(defun get_first_non_zero (new_values n)
	(cond 
		((null new_values) nil)
		((= (first new_values) 0) (+ n 1))
		(t (get_first_non_zero (rest new_values) (+ n 1)))
	)
)

;Performs unit propagation, pure literal elimination, and backtracking search
(defun sat_checker (n delta values)
    (let*
        (
            ; These set of variables perform unit resolution
            (unit_propagation_terms (unit_propagation nil delta values))
            (uprop_clauses (first unit_propagation_terms))
            (uprop_values (second unit_propagation_terms))
            
            ; These set of variables perform pure literal elimination 
            (pure_lit_terms (pure_elim uprop_clauses n uprop_values))
            (new_clauses (first pure_lit_terms))
            (new_values (second pure_lit_terms))
        )
        (cond
        	;if unit propagation results in empty clauses and values, then no solution
            ((and (null uprop_clauses) (null uprop_values) nil)) 
            ;otherwise if unit propagation and pure literal elimination results in an empty cnf, then we found a solution
            ((null new_clauses) (fill_zeros new_values 0))  
            ;Perform back_tracking
            (t
                (let* ; Back-tracking DFS
                    ((node (get_first_non_zero new_values 0))
                     (pos_result (sat_checker n (simplify_clauses new_clauses node) (assign_value new_values node)))
                     (neg_result (sat_checker n (simplify_clauses new_clauses (- node)) (assign_value new_values (- node)))))
                    (cond 
                    	;if both calls are nil, then no solution
                    	((and (null pos_result) (null neg_result)) nil)
                    	;if only pos_result is null, then return neg_result
                    	((null pos_result) neg_result)
                    	;otherwise return pos_result
                    	(t pos_result)
                    )
                )
         	)
        )
    )
)




                    