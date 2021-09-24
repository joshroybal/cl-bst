;;; read unix words file into list of symbols
(defun read-file (infile)
  (with-open-file (stream infile)
		  (do ((symbol (read stream nil 'eof) (read stream nil 'eof))
		       (symbols nil (cons symbol symbols)))
		      ((eq symbol 'eof) (nreverse symbols))
		      (cond ((not (symbolp symbol))
			     (format t "~&~S (NOT A SYMBOL)" symbol))))))
