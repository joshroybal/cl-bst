(defun root (node) (car node))
(defun key (node) (car node))
(defun left (node) (cadr node))
(defun right (node) (caddr node))

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

(defun height (tree)
  (cond ((null tree) -1)
	(t (+ 1 (max (height (cadr tree)) (height (caddr tree)))))))

(defun min-node (node)
  (if (null (left node))
      node
    (min-node (left node))))

(defun max-node (node)
  (if (null (right node))
      node
    (max-node (right node))))

(defun min-key (tree)
  (let ((node (min-node tree)))
    (if (null node)
	nil
      (key node))))

(defun max-key (tree)
  (let ((node (max-node tree)))
    (if (null node)
	nil
      (key node))))

(defun remove-tree (data tree)
  (cond ((null tree)
	 nil)
	((string-lessp data (key tree))
	 (make-tree
	  (key tree)
	  (remove-tree data (left tree))
	  (right tree)))
	((string-greaterp data (key tree))
	 (make-tree
	  (key tree)
	  (left tree)
	  (remove-tree data (right tree))))
	((null (left tree))
	 (right tree))
	((null (right tree))
	 (left tree))
	(t
	 (let ((new-key (max-key (left tree))))
	   (make-tree
	    new-key
	    (remove-tree new-key (left tree))
	    (right tree))))))

(defun defoliate (tree)
  (cond ((or (null tree) (and (null (left tree)) (null (right tree))))
	 nil)
	(t
	 (make-tree
	  (key tree)
	  (defoliate (left tree))
	  (defoliate (right tree))))))

(defun parent (k node)
  (cond ((or (null node) (leaf-p node) (equalp k (key node)))
	 nil)
	((and (not (null (left node))) (equalp k (key (left node))))
	 node)
	((and (not (null (right node))) (equalp k (key (right node))))
	 node)
	((string-lessp k (key node))
	 (parent k (left node)))
	((string-greaterp k (key node))
	 (parent k (right node)))))

(defun predecessor-node (k tree)
  (labels ((aux (k predecessor node)
		(cond ((null node)
		       nil)
		      ((string-lessp k (key node))
		       (aux k predecessor (left node)))
		      ((string-greaterp k (key node))
		       (aux k node (right node)))
		      (t
		       (if (null (left node))
			   predecessor
			 (max-node (left node)))))))
	  (aux k nil tree)))

(defun successor-node (k tree)
  (labels ((aux (k successor node)
		(cond ((null node)
		       nil)
		      ((string-lessp k (key node))
		       (aux k node (left node)))
		      ((string-greaterp k (key node))
		       (aux k successor (right node)))
		      (t
		       (if (null (right node))
			   successor
			 (min-node (right node)))))))
	  (aux k nil tree)))

(defun predecessor-key (k tree)
  (key (predecessor-node k tree)))

(defun successor-key (k tree)
  (key (successor-node k tree)))

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

(defun pre-order (f tree)
  (cond ((null tree) nil)
	(t
	 (funcall f (car tree))
	 (pre-order f (cadr tree))
	 (pre-order f (caddr tree)))))

(defun in-order (f tree)
  (cond ((null tree) nil)
	(t
	 (in-order f (cadr tree))
	 (funcall f (car tree))
	 (in-order f (caddr tree)))))

(defun post-order (f tree)
  (cond ((null tree) t)
	(t
	 (post-order f (cadr tree))
	 (post-order f (caddr tree))
	 (funcall f (car tree)))))

;; (defun in-order-to-file (tree filename)
;;   (with-open-file (outfile filename
;; 			   :direction :output
;; 			   :if-does-not-exist :create)
;; 		  (in-order tree outfile)))

(defun count-leaves (tree)
  (cond ((null tree) 0)
	((leaf-p tree) 1)
	(t (+ (count-leaves (cadr tree)) (count-leaves (caddr tree))))))

(defun list->tree (list)
  (do ((rest list (cdr rest))
       (bst nil (insert-tree (car rest) bst)))
      ((null rest) bst)))

(defun text-file->tree (filename)
  (with-open-file (infile filename)
		  (do ((symbol (read infile nil 'eof) (read infile nil 'eof))
		       (bst nil (insert-tree symbol bst)))
		      ((eq symbol 'eof) bst)
		      (cond ((or (not (symbolp symbol)) (equalp symbol nil))
			     (format t "~&~S (NOT A SYMBOL)" symbol))))))
