(defun isAtomic (l)
	(if (listp l)
		nil
		t))

(defun isNegativeAtomicClause (l)
	(and (= (length l) 2) (isAtomic (cadr l)) (eq 'NOT (car l))))

(defun isAtomicClause (l)
	(or (isAtomic l) (isNegativeAtomicClause l))
	)
	
(defun listP_Not (l)
	(if (isAtomic l)
		nil
	(if (isNegativeAtomicClause l)
		nil
		(listp l)
	)))


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
		
		
(defun decomposeIFF (l)
	(
	if (null l)
		nil
	(if (isAtomicClause l)
		l
	(if (eq 'IFF (car l))
		(cons 'AND (list (list 'IF (decomposeIFF (cadr l)) (decomposeIFF (caddr l))) (list 'IF (decomposeIFF (caddr l)) (decomposeIFF (cadr l)))))
		(cons (car l) (mapcar #'decomposeIFF (cdr l)))))))

		
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
		
(defun eliminateIdenticalAndOrOriginal (x l)
	"eliminate (and (and ...)) and (or (or ...)) cases"
	(
	if (isAtomicClause l)
		l
	(if (eq x (car l))
		;;;(mapcar #'(lambda (l) (eliminateIdenticalAndOr x l)) (cdr l))
		(eliminateIdenticalAndOr x (cdr l))
		(cons (car l) (mapcar #'(lambda (p) (eliminateIdenticalAndOr (car l) p)) (cdr l))))))
		
(defun eliminateIdenticalAndOr1 (x l)
	"eliminate (and (and ...)) and (or (or ...)) cases"
	(
	if(null l)
		nil
	(if (isAtomicClause l)
		l
	(if (eq x (car l))
		(eliminateIdenticalAndOr x (cdr l))
		(cons (car l) (eliminateIdenticalAndOr (car l) (cdr l)))))))
		

(defun eliminateIdenticalAndOr (x l)
	"eliminate (and (and ...)) and (or (or ...)) cases"
	(
	if(null l)
		nil
	(if (isAtomicClause l)
		l
	(if (eq x (car l))
		(eliminateIdenticalAndOr x (cdr l))
		(if (listP_Not (car l))
			(append (eliminateIdenticalAndOr x (car l)) (eliminateIdenticalAndOr x (cdr l)))
			(cons (car l) (eliminateIdenticalAndOr (car l) (cdr l)))
			)))))

		
(defun handleIdenticalAndOr (x l)
	"sweep through the sentence and call appropriate eliminateIdenticalAndOr"
	(if (null l)
		nil
	(if (and (= (length l) 1) (isAtomic (car l)))
		l
	(if (isAtomicClause l)
		l
	(if (eq x (caar l))
		(append (eliminateIdenticalAndOr x (car l)) (handleIdenticalAndOr x (cdr l)))
		(cons (eliminateIdenticalAndOr x (car l)) (handleIdenticalAndOr x (cdr l))))))))

(defun IdenticalAndOr (l)
	(if (isAtomicClause l)
		l
	(cons (car l) (handleIdenticalAndOr (car l) (cdr l)))))



(defun convert-to-CNF (l)
	(setf l (decomposeIFF l))
	(setf l (implicationOut l))
	(setf l (negationIn l))
	(setf l (IdenticalAndOr l))
	l
)