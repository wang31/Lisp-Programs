(defun isAtomic (l)
	(if (listp l)
		nil
		t))

(defun isNegativeAtomicClause (l)
	(and (= (length l) 2) (isAtomic (cadr l)) (eq 'NOT (car l))))

(defun isAtomicClause (l)
	(or (isAtomic l) (isNegativeAtomicClause l))
	)

;;;get rid of implications
(defun implicationOut (l)
	(
	if (null l)
		nil
	(if (isAtomicClause l)
		l
	(if (eq 'IF (car l))
		(cons 'OR (list (list 'NOT (implicationOut (cadr l))) (implicationOut (caddr l))))
		(cons (car l) (mapcar #'implicationOut (cdr l)))))))
		
(defun negationIn_single (l)
  "handle negation for a single clause"
  (	if	(isAtomic l)
		(list 'NOT l)
	(if (isNegativeAtomicClause l)
		(cadr l)
	(let ((operator (car l)))
	(if (eq operator 'TRUE)
		'FALSE
    (if (eq operator 'FALSE)
		'TRUE
    (if (eq operator 'NOT)
		(cadr l)
    (if (eq operator 'AND)
		(cons 'OR (mapcar #'negationIn_single (cdr l)))
    (if (eq operator 'OR)
		(cons 'AND (mapcar #'negationIn_single (cdr l)))
		(list 'not operator))))))))))

(defun negationIn (l)
	(
	if (null l)
		nil
	(if (isAtomicClause l)
		l
	(if (eq 'NOT (car l))
		(negationIn_single (cadr l))
		(cons (car l) (mapcar #'negationIn (cdr l)))))))


(defun convert-to-CNF (l)
	(setf l (implicationOut l))
	(setf l (negationIn l))
	l
)