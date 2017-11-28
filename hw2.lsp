;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;; Shivraj Gill
;; 704-344-118
;; HW2


; Question 1 
;;Implementation of DFS on a search TREE
;;
;; [INPUTS]
;;    TREE - list representation of a tree
;; [OUTPUT]
;;    L - a list with the order of how the tree is explored in DFS
;; [EXPLANATION]
;;    BASE CASE: if TREE is null, then return nil
;;    OTHERWISE: 1) if TREE's head is an atom, then make it the head of a
;;                  list, and the tail of the list is the result of 
;;                  recursive call of DFS on the rest of the TREE
;;              
;;               2) if TREE'S head is a list, then recursively call DFS on 
;;                  the head and the rest of the TREE and append the results


(defun DFS (TREE)
    (cond
        ;BASE CASE
        ((null TREE) nil)
        ;1)
        ((atom (first TREE)) (cons (first TREE) (DFS (rest TREE))))
        ;2)
        ((listp (first tree)) (append (DFS (first tree)) (DFS (rest tree))))
    )
)

; ----------------------------------------------------------------------------
; Question 2 

;;DFID is a top-level function that helps implement depth-first iterative-deepening
;;search
;; [INPUTS]
;;    TREE - list representation of a tree
;;    max_depth - max depth of TREE
;; [OUTPUT]
;;    L - list of the nodes in the order that they would be visited by a
;;        left-to-right depth-first iterative-deepening search
;; [EXPLANATION]
;;  Call depth_check with a starting depth of 1 

(defun DFID (TREE max_depth)
    (depth_check tree 1 max_depth)
)


; depth_check is a helper function for DFID 
;;Performs a depth first search at the given depth n and stops once depth > maxdepth
;; [INPUTS]
;;    TREE - list representation of a tree
;;    cur_depth - current depth of tree
;;    max - max depth of TREE
;; [OUTPUT]
;;    L - list of the nodes in the order that they would be visited by a
;;        left-to-right depth-first iterative-deepening search
;; [EXPLANATION]
;;    BASE CASE: If cur_depth > max_depth , then return nil
;;    OTHERWISE: 1) If cur_depth < max_depth, then perform DFS on TREE for the 
;;                  cur_depth by calling perform_search, recursively call depth_check
;;                  and increment cur_depth by 1, and append the results of the call to 
;;                  results from perform_search
;;              
;;               2) If cur_depth = max_depth, then return results of perform_search
;;                  at max_depth
;;


(defun depth_check (TREE cur_depth max_depth)
    (cond
        ((> cur_depth max_depth) nil)
        ((< cur_depth max_depth)  (append (perform_search TREE cur_depth) (depth_check TREE (+ cur_depth 1) max_depth)))
        ((= cur_depth max_depth) (perform_search TREE max_depth))
    )
)

;; perform_search does a depth first search up to a depth d
;; [INPUTS]
;;    TREE - list representation of a tree
;;    depth - depth of DFS on TREE
;; [OUTPUT]
;;    L - list of the nodes in the order that they would be visited by a
;;        left-to-right limited depth-first search 
;; [EXPLANATION]
;;    BASE CASE: If depth = 0, return nil
;;               If depth = 1, return list of nodes that are leaves of root node
;;    
;;    OTHERWISE: Append result of recursive call of perform_search on the head of tree with depth (n - 1) to 
;;               result of recursive call to perform_search on the tail of the tree with depth (n - 1)

(defun perform_search (TREE depth)
    (cond
        ;Base Cases
        ((= depth 0) nil)
        ((= depth 1)
            (cond
                ((null tree) nil)
                ((atom (first tree)) (cons (first tree) (perform_search (rest tree) 1)))
                ((listp (first tree)) (perform_search (rest tree) 1))))
        ;Otherwise 
        (t
            (cond
                ((null tree) nil)
                (t
                    ;append the leaves returned by recursive calls to first and rest of 
                    ;the elements in the list
                    (append
                        (cond
                            ;If first element is a leaf, then change it to a list
                            ((atom (first tree)) (list (first tree)))            
                            ;if first element a list, recursively call function on first element and decrement
                            ;depth by 1                                 
                            ((listp (first tree)) (perform_search (first tree) (- depth 1)))
                        )
                        (perform_search (rest tree) depth)
                    )
                )
            )
        )
    )
)

; ----------------------------------------------------------------------------

; Question 3 

; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond
        ((equal s '(3 3 nil)) t)
        (t nil)
  )  
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let*
        (
            (runners (+ m c))
            (start_m (- (first s) m))
            (start_c (- (second s) c))
            (end_m (- 3 start_m))
            (end_c (- 3 start_c)))
        (cond
            ((null s) nil)
            ;if more than two people try to cross, return nil
            ((> runners 2) nil) 
            
            ;if no one is trying to cross, return nil
            ((= runners 0) nil) 
            
            ;If there are more cannibals than missionaries on starting side
            ;and at least one missionary on that side, then return nil
            ((and (< start_m start_c) (> start_m 0)) nil) 
            
            ;If there are more cannibals than missionaries on end side
            ;and at least one missionary on that side, then return nil
            ((and (< end_m end_c) (> end_m 0)) nil) 
            
            ;If there is not enough cannibals or missionaries to move on starting side
            ((or (< start_m 0) (< start_c 0)) nil) 
            
            ;otherwise return the next state in a list
            (t (list (list end_m end_c (not (third s)))))
        )
  )
)


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)
  (append
    ;There are five different moves available which can be attained by calling
    ;next-state and the number of cannibals/missionaries you want to move
          (next-state s 0 1) 
          (next-state s 1 0)
          (next-state s 1 1)
          (next-state s 2 0)
          (next-state s 0 2)
  )
)  

; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
; path from the initial state to the current state (PATH), the legal successor
; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a single depth-first iteration to the given depth on
; each element of STATES in turn. If any of those searches reaches the final
; state, MULT-DFS returns the complete path from the initial state to the goal
; state. Otherwise, it returns NIL.
(defun mult-dfs (states path depth)
  (cond
        ;Base Case
        ((null states) nil)
        ; If depth is 0, then check if any successor states are goal state, 
        ;and return resulting PATH
        ((= depth 0) 
            (cond
                ;check if the first item in successor states is goal state
                ((final-state (car states)) (append path (list (car states)))) ; Check head
                ;check if rest of the successor states are a goal state
                (t (mult-dfs (cdr states) path depth))
            )
        )
        ;Otherwise depth > 0, then do a depth first search  
        (t  (let* ((top_state (car states))) 
              (cond
                  ;if top successor state is goal state, then append it to PATH and return PATH
                  ((final-state top_state) (append path (list (top_state)))) 
                  ;Otherwise do dfs starting at top_state
                  (t 
                    (let* (
                            ;defines successor states of top state
                            (suc_states (succ-fn top_state)) 
                            ;dfs of top sucesssor state and decrement the depth by 1 and store resulting path of dfs
                            (result (mult-dfs suc_states (append path (list top_state)) (- depth 1))))
                          (cond
                              ;If result path is null, then perform DFS on remaining sucessor states 
                              ;through recursive call
                              ((NULL result) (mult-dfs (cdr states) path depth))
                              
                              ;otherwise, result is not null, so return result
                              (t result)
                          )
                      )
                  )
              )
            )
        )
  )
) 

; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
; three arguments: a state (S), the path from the initial state to S (PATH), and
; the depth (DEPTH). If S is the initial state in our search, PATH should be
; NIL. It performs a depth-first search starting at the given state. It returns
; the path from the initial state to the goal state, if any, or NIL otherwise.
(defun single-dfs (s path depth)
  (cond
        ;Base Case if depth is 0, then check if s is the goal-state 
        ((= depth 0) (cond 
                        ((final-state s) (list s)))
        )
        ;perform depth first search by calling mult-dfs and initializing successor states and depth
        (t (mult-dfs (succ-fn s) (append path (list s)) (- depth 1)))
  )
)

; ID-DFS is the top-level function. It takes two arguments: an initial state (S)
; and a search depth (DEPTH). ID-DFS performs a series of depth-first
; iterations, starting from the given depth until a solution is found. It
; returns the path from the initial state to the goal state. The very first call
; to ID-DFS should use depth = 0.
(defun id-dfs (s depth)
  (let ((result (single-dfs s nil depth)))
         (cond
            ;if result is NULL, increase depth and perform dfs
            ((NULL result) (id-dfs s (+ depth 1)))
    
            ;otherwise, return result
            ((not (NULL result)) result)           
          )
  )
) 




