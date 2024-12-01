<p align="center"><b>Національний технічний університет України “Київський політехнічний інститут ім. Ігоря Сікорського”</b></p>
<p align="center"><b>Факультет прикладної математики Кафедра системного програмування і спеціалізованих комп’ютерних систем</b></p>
<p align="center"><b>ЛАБОРАТОРНА РОБОТА №3</b></p>
<p align="center"><b>з дисципліни «Вступ до функціонального програмування»</b></p>

<div align="right">
    <p>Студент: Горбик Данііл</p>
    <p>Група: КВ-13</p>
    <p>Рік: 2024</p>
</div>
## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно. 
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку. Не допускається використання: деструктивних операцій, циклів, функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів). 
2. Імперативний варіант реалізації має базуватись на використанні циклів і деструктивних функцій (псевдофункцій). Не допускається використання функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Тим не менш, оригінальний список цей варіант реалізації також не має змінювати, тому перед виконанням деструктивних змін варто застосувати функцію copy-list (в разі необхідності). Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).

## Завдання за варіантом №4

1. Алгоритм сортування вставкою №2 (з лінійним пошуком справа) за незменшенням.

```lisp




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


```

## Результат виконання програми

```
Functional Sort (3 1 4 1 5 9) -> (1 1 3 4 5 9)
Functional Sort (9 7 5 3 1 0) -> (0 1 3 5 7 9)
Functional Sort (1 2 3 4 5 6) -> (1 2 3 4 5 6)
Imperative Sort (3 1 4 1 5 9) -> (1 1 3 4 5 9)
Imperative Sort (9 7 5 3 1 0) -> (0 1 3 5 7 9)
Imperative Sort (1 2 3 4 5 6) -> (1 2 3 4 5 6)
passed... Functional Test 1
passed... Functional Test 2
passed... Functional Test 3
passed... Functional Test 4
passed... Functional Test 5
passed... Imperative Test 1
passed... Imperative Test 2
passed... Imperative Test 3
passed... Imperative Test 4
passed... Imperative Test 5
```
