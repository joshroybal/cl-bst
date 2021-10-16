(defun key (node) (car node))
(defun value (node) (cadr node))
(defun left (node) (caddr node))
(defun right (node) (cadddr node))

(defun make-node (k v left-node right-node)
  (list k v left-node right-node))

(defun make-leaf (k v)
  (list k v nil nil))

(defun leaf-p (node)
  (and (null (left node)) (null (right node))))

(defun insert-node (k v node)
  (cond ((null node)
	 (make-leaf k v))
	((equalp k (key node))
	 node)
	((string-lessp k (key node))
	 (make-node
	  (key node)
	  (value node)
	  (insert-node k v (left node))
	  (right node)))
	((string-greaterp k (key node))
	 (make-node
	  (key node)
	  (value node)
	  (left node)
	  (insert-node k v (right node))))))
  
(defun node-p (k node)
  (cond ((null node) nil)
	((equalp k (key node)) node)
	((string-lessp k (key node)) (node-p k (left node)))
	((string-greaterp k (key node)) (node-p k (right node)))))

(defun get-value (k node)
  (value (node-p k node)))

(defun count-nodes (node)
  (cond ((null node) 0)
	(t (+ 1 (count-nodes (left node)) (count-nodes (right node))))))

(defun height (node)
  (cond ((null node) -1)
	(t (+ 1 (max (height (left node)) (height (right node)))))))

(defun min-node (node)
  (if (null (left node))
      node
    (min-node (left node))))

(defun max-node (node)
  (if (null (right node))
      node
    (max-node (right node))))

(defun min-key (node)
  (key (min-node node)))

(defun max-key (node)
  (key (max-node node)))

(defun remove-node (k node)
  (cond ((null node)
	 nil)
	((string-lessp k (key node))
	 (make-node
	  (key node)
	  (value node)
	  (remove-node k (left node))
	  (right node)))
	((string-greaterp k (key node))
	 (make-node
	  (key node)
	  (value node)
	  (left node)
	  (remove-node k (right node))))
	((null (left node))
	 (right node))
	((null (right node))
	 (left node))
	(t
	 (let ((new-key (max-key (left node))))
	   (make-node
	    new-key
	    (get-value new-key (left node))
	    (remove-node new-key (left node))
	    (right node))))))

(defun defoliate (node)
  (cond ((or (null node) (and (null (left node)) (null (right node))))
	 nil)
	(t
	 (make-node
	  (key node)
	  (value node)
	  (defoliate (left node))
	  (defoliate (right node))))))

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

(defun predecessor-node (k node)
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
	  (aux k nil node)))

(defun successor-node (k node)
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
	  (aux k nil node)))

(defun predecessor-key (k node)
  (key (predecessor-node k node)))

(defun successor-key (k node)
  (key (successor-node k node)))

(defun serialize (node)
  (cond ((null node)
	 (list nil))
	(t
	 (append (list (list (key node) (value node)))
		 (serialize (left node))
		 (serialize (right node))))))

(defun deserialize (seq)
  (labels ((aux (x)
		(cond ((null x)
		       nil)
		      (t
		       (make-node
			(key x)
			(value x)
			(aux (pop seq))
			(aux (pop seq)))))))
	  (aux (pop seq))))

(defun serialize-to-file (node filename)
  (let ((node-list (serialize node)))
    (with-open-file (outfile filename
				  :direction :output
				  :if-does-not-exist :create)
			 (format outfile "~S" node-list))))

(defun deserialize-from-file (filename)
  (with-open-file (infile filename
			  :direction :input)
		  (deserialize (read infile))))

;;; handy functions to send to the traversal functions
(defun display-node (node)
  (format t "~&~A: ~A" (key node) (value node)))

(defun display-key (node)
  (format t "~&~S" (key node)))

(defun display-value (node)
  (format t "~&~S" (value node)))

(defun pre-order (f node)
  (cond ((null node) nil)
	(t
	 (funcall f node)
	 (pre-order f (left node))
	 (pre-order f (right node)))))

(defun in-order (f node)
  (cond ((null node) nil)
	(t
	 (in-order f (left node))
	 (funcall f node)
	 (in-order f (right node)))))

(defun post-order (f node)
  (cond ((null node) t)
	(t
	 (post-order f (left node))
	 (post-order f (right node))
	 (funcall f node))))

(defun count-leaves (node)
  (cond ((null node) 0)
	((leaf-p node) 1)
	(t (+ (count-leaves (left node)) (count-leaves (right node))))))

(defun list->node (list)
  (do ((rest list (cdr rest))
       (bst nil (insert-node (key rest) (value rest) bst)))
      ((null rest) bst)))

(defun text-file->bst (filename)
  (with-open-file (infile filename)
		  (do ((symbol (read infile nil 'eof) (read infile nil 'eof))
		       (bst nil (insert-node (key symbol) (value symbol) bst)))
		      ((eq symbol 'eof) bst))))
