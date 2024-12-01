

(defun insert-right-to-left (value sorted-list)
  (cond
    ((null sorted-list) (list value)) 
    ((<= value (car sorted-list))     
     (cons value sorted-list)) 
    (t 
     (let ((rest (insert-right-to-left value (cdr sorted-list)))) 
       (cons (car sorted-list) rest))))) 

(defun insertion-sort-functional (unsorted-list)
  (if (null unsorted-list)     
      nil
      (insert-right-to-left (car unsorted-list) (insertion-sort-functional (cdr unsorted-list)))))



;;;
(defun insertion-sort-imperative (list)
  (let ((sorted-list (copy-list list)))
    (loop for i from 1 below (length sorted-list) do
      (let ((key (nth i sorted-list))
            (j (- i 1)))
        (loop while (and (>= j 0) (> (nth j sorted-list) key)) do
          (setf (nth (+ j 1) sorted-list) (nth j sorted-list))
          (decf j))
        (setf (nth (+ j 1) sorted-list) key)))
    sorted-list))





;tests

(defun test-sorting ()
  (let ((test-cases '((3 1 4 1 5 9)
                      (9 7 5 3 1 0)
                      (1 2 3 4 5 6))))
    ;; Functional Tests
    (dolist (test-case test-cases)
      (format t "Functional Sort ~A -> ~A~%" test-case
              (insertion-sort-functional test-case)))
    ;; Imperative Tests
    (dolist (test-case test-cases)
      (format t "Imperative Sort ~A -> ~A~%" test-case
              (insertion-sort-imperative test-case)))))


 (test-sorting)

(defun check-insertion-sort-functional (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (insertion-sort-functional input) expected)
          name))

(defun check-insertion-sort-imperative (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (insertion-sort-imperative input) expected)
          name))

(defun test-insertion-sort-functional ()
  (check-insertion-sort-functional "Functional Test 1" '(3 1 4 1 5 9) '(1 1 3 4 5 9))
  (check-insertion-sort-functional "Functional Test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (check-insertion-sort-functional "Functional Test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-insertion-sort-functional "Functional Test 4" nil nil)
  (check-insertion-sort-functional "Functional Test 5" '(1) '(1)))

(defun test-insertion-sort-imperative ()
  (check-insertion-sort-imperative "Imperative Test 1" '(3 1 4 1 5 9) '(1 1 3 4 5 9))
  (check-insertion-sort-imperative "Imperative Test 2" '(9 7 5 3 1) '(1 3 5 7 9))
  (check-insertion-sort-imperative "Imperative Test 3" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-insertion-sort-imperative "Imperative Test 4" nil nil)
  (check-insertion-sort-imperative "Imperative Test 5" '(1) '(1)))

(test-insertion-sort-functional)
(test-insertion-sort-imperative)
