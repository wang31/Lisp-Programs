;;;main recursive function
(defun best_change (list aC)
  (cond ((= 0 aC) (cons 0 nil))
        ((null list) (cons aC nil))
        ((< aC 0) (cons (+ aC (car list)) nil))
        (T (let ((choicea (best_change (cdr list) aC))
                 (choiceb (cons (car list) (best_change list (- aC (car list))))))
             (cond ((= (car (last choicea)) (car (last choiceb)))  (if (< (length choicea) (length choiceb)) choicea choiceb))
                   ((> (car (last choicea)) (car (last choiceb)))  choiceb)
                   ((< (car (last choicea)) (car (last choiceb)))  choicea)
                   )
             )
           )
        )
  )

(defun removelast (l)
  (cond ((= (length l) 1) nil) 
	(T (cons (car l) (removelast (cdr l))))
        )
  )

(defun find-best-change (N L)
  (compress (add_unused L (removelast (best_change L N))))
  )

;;;compression functions for formatting the result, borrowed from professor's slide and modified 
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
    x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
    (let ((next (car lst)))
      (if (eql next elt)
	  (compr elt (+ n 1) (cdr lst))
	(cons (n-elts elt n)
	      (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
    (if (consp elt)
	(append elt nil)
	(list 1 elt)
      )
    )
  )


;;;more functions for formatting results
(defun isMember_atom (q list)
  (cond ((null list) nil)
        ((eq q (car list)) t)
        (t (isMember_atom q (cdr list)))))

;;add the unused coins to the result. eg. add (0 5) to result if 5 is not used
(defun add_unused (l1 l2)
  (cond ((null l1) l2)
	((isMember_atom (car l1) l2) (add_unused (cdr l1) l2))
	(t (
	    cons (list 0 (car l1)) (add_unused (cdr l1) l2)
		 )
	   )
	)
  )