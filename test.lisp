(load "bst.lisp")
(defvar *bst* nil)
(defvar *serialized-bst* nil)
(defvar *deserialized-bst* nil)
(setf *bst* (text-file->bst "colors.txt"))
(format t "~&pre-order")
(pre-order #'(lambda (x) (format t "~&~S" (key x))) *bst*)
(format t "~&in-order")
(in-order #'(lambda (x) (format t "~&~S" (key x))) *bst*)
(format t "~&post-order")
(post-order #'(lambda (x) (format t "~&~S" (key x))) *bst*)
(format t "~&in-order predecessor key successor")
(in-order #'(lambda (x) (format t "~&~S ~S ~S"
				(predecessor-key (key x) *bst*)
				(key x)
				(successor-key (key x) *bst*)))
	  *bst*)
(format t "~&binary search tree")
(format t "~&~S" *bst*)
(format t "~&serialized tree")
(setf *serialized-bst* (serialize *bst*))
(format t "~&~S" *serialized-bst*)
(format t "~&deserialized tree")
(setf *deserialized-bst* (deserialize *serialized-bst*))
(format t "~&~S" *deserialized-bst*)
(format t "~&min key = ~S" (min-key *bst*))
(format t "~&max key = ~S" (max-key *bst*))
(format t "~&nodes = ~d" (count-nodes *bst*))
(format t "~&leaves = ~d" (count-leaves *bst*))
(format t "~&height = ~d" (height *bst*))
(setf *bst* (remove-node 'green *bst*))
(format t "~&deleted GREEN (interior node)")
(format t "~&~S" *bst*)
(setf *bst* (remove-node 'violet *bst*))
(format t "~&deleted VIOLET (leaf node)")
(format t "~&~S" *bst*)
(setf *bst* (remove-node 'red *bst*))
(format t "~&deleted RED (root node)")
(format t "~&~S" *bst*)
(format t "~&min key = ~S" (min-key *bst*))
(format t "~&max key = ~S" (max-key *bst*))
(format t "~&nodes = ~d" (count-nodes *bst*))
(format t "~&leaves = ~d" (count-leaves *bst*))
(format t "~&height = ~d" (height *bst*))
(format t "~&defoliating binary search tree")
(setf *bst* (defoliate *bst*))
(format t "~&~S" *bst*)
(format t "~&min key = ~S" (min-key *bst*))
(format t "~&max key = ~S" (max-key *bst*))
(format t "~&nodes = ~d" (count-nodes *bst*))
(format t "~&leaves = ~d" (count-leaves *bst*))
(format t "~&height = ~d" (height *bst*))
