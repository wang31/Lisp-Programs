(defun createhash (list)
	(cond ((null list) nil)
		  (t (setf (gethash (caar list) *zw_hash*) (cdr (car list)))
			 (updatename (car (cddr (car list))) (caar list))
			 (createhash (cdr list))
			 )
))

(defun updatename (names art)
	(cond ((null names) nil)
		  (t (let ((entry (append (gethash (car names) *zw_hash*) (cons art nil))))
			 (setf (gethash (car names) *zw_hash*) entry)
			 (updatename (cdr names) art)
))))

(defun load-db (filename)
  (defparameter *zw_hash* (make-hash-table))
  (with-open-file (in filename)
    (with-standard-io-syntax
      (createhash (read in))
	  )))


(defun isMember (a list)
	(cond ((null list) nil)
		  ((eq a (car list)) t)
		  (t (isMember a (cdr list)))))

(defun coauthorExist (name l1 l2)
	(cond ((or (null l2) (null l1)) nil)
		  ((eq name (car l1)) (coauthorExist name (cdr l1) l2))
		  ((isMember (car l1) l2) t)
		  (t (coauthorExist name (cdr l1) l2))
))

;;given a person, an article that person writes, and the article that cites the article
(defun one_one_score (name art cite)
	(let ((artInfo (gethash art *zw_hash*))
		  (citeInfo (gethash cite *zw_hash*)))
	(cond ((null citeInfo) 0)
		  ((isMember name (car (cdr citeInfo))) 0) ;;citation by self
		  ((coauthorExist name (car (cdr artInfo)) (car (cdr citeInfo))) (if (> (car citeInfo) 1995) 1 2)) ;;citation by co-author(after 1995 1, before 1995 2)
		  (t (if (> (car citeInfo) 1995) 6 4))
)))

;;given a person, an article that person writes, and a list of articles that cites the article
(defun one_mul_score (name art list)
	(cond ((null list) 0)
		  (t (+ (one_one_score name art (car list)) (one_mul_score name art (cdr list))))
))

;;given a person, a list of articles that person writes
(defun total_score (name arts)
	(cond ((null arts) 0)
		  (t (+ (one_mul_score name (car arts) (car (cdr (cddr (gethash (car arts) *zw_hash*))))) (total_score name (cdr arts)))))
)

(defun count-citations (name db)
	(defvar *zw_hash* nil)
	(cond ((null *zw_hash*) (load-db db) (total_score name (gethash name *zw_hash*)))
		  (t (total_score name (gethash name *zw_hash*)))
))