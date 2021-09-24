(defun make-tree (data right left)
  (list data right left))

(defun make-leaf (data)
  (list data nil nil))

(defun leaf-p (tree)
  (and (listp tree) (null (cadr tree)) (null (caddr tree))))

(defun insert-tree (data tree)
  (cond ((null tree)
	 (make-tree data nil nil))
	((equalp data (car tree))
	 (format t "~&~S ~S (DUPLICATE FOUND)" data (car tree))
	 tree)
	((string-lessp data (car tree))
	 (make-tree (car tree) (insert-tree data (cadr tree)) (caddr tree)))
	((string-greaterp data (car tree))
	 (make-tree (car tree) (cadr tree) (insert-tree data (caddr tree))))))
  
(defun node-p (data tree)
  (cond ((null tree) nil)
	((equalp data (car tree)) tree)
	((string-lessp data (car tree)) (node-p data (cadr tree)))
	((string-greaterp data (car tree)) (node-p data (caddr tree)))))

(defun count-nodes (tree)
  (cond ((null tree) 0)
	(t (+ 1 (count-nodes (cadr tree)) (count-nodes (caddr tree))))))

(defun serialize (tree)
  (cond ((null tree)
	 (list nil))
	(t
	 (append (list (car tree))
		 (serialize (cadr tree))
		 (serialize (caddr tree))))))

(defun deserialize (seq)
  (labels ((aux (x)
		(cond ((null x)
		       nil)
		      (t
		       (make-tree
			x
			(aux (pop seq))
			(aux (pop seq)))))))
	  (aux (pop seq))))

(defun serialize-to-file (tree filename)
  (let ((tree-list (serialize tree)))
    (with-open-file (outfile filename
				  :direction :output
				  :if-does-not-exist :create)
			 (format outfile "~S" tree-list))))

(defun deserialize-from-file (filename)
  (with-open-file (infile filename
			  :direction :input)
		  (deserialize (read infile))))

(defun nor (x y) (not (or x y)))

(defun pre-order (tree)
  (cond ((null tree) nil)
	(t
	 (format t "~&~S" (car tree))
	 (pre-order (cadr tree))
	 (pre-order (caddr tree)))))

(defun in-order (tree &optional port)
  (cond ((null tree) nil)
	(t
	 (in-order (cadr tree) port)
	 (if (null port)
	     (format t "~&~S" (car tree))
	   (format port "~A~%" (car tree)))
	 (in-order (caddr tree) port))))

(defun in-order-to-file (tree filename)
  (with-open-file (outfile filename
			   :direction :output
			   :if-does-not-exist :create)
		  (in-order tree outfile)))

(defun post-order (tree)
  (cond ((null tree) t)
	(t
	 (post-order (cadr tree))
	 (post-order (caddr tree))
	 (format t "~&~S" (car tree)))))

(defun count-leaves (tree)
  (cond ((null tree) 0)
	((leaf-p tree) 1)
	(t (+ (count-leaves (cadr tree)) (count-leaves (caddr tree))))))

(defun list->tree (list)
  (do ((rest list (cdr rest))
       (bst nil (insert-tree (car rest) bst)))
      ((null rest) bst)))
