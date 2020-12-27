(defparameter *vertices* (make-hash-table :test #'equal))

(defparameter *graphs* (make-hash-table :test #'equal))

(defparameter *arcs* (make-hash-table :test #'equal))

(defparameter *visited* (make-hash-table :test #'equal))

(defparameter *vertex-keys* (make-hash-table :test #'equal))

(defparameter *previous* (make-hash-table :test #'equal))

(defun print-graphs()
  (maphash (lambda (k v)(format t "Key: ~S, value: ~S ~%" k v)) *graphs*))

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
  (let ((result (graph-vertices graph-id)))
    (rem-f-ht result)))

(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id)
		 *vertices*)
  (list 'vertex graph-id vertex-id)))

(defparameter v-list'())
(defun loop-l (lista graph-id)
  (cond ((null lista) nil)
	((eql graph-id (nth 1 (first lista)))
	 (push (first lista) v-list)
	 (loop-l (rest lista) graph-id)
	 )
        ((loop-l (rest lista) graph-id)))
  )
(defun rem-f-ht (lista)
  (cond ((null lista) nil)
	(t (remhash (first lista) *vertices*)
	   (rem-f-ht (rest lista))))
)
;;NON TOCCARE
(defun graph-vertices (graph-id)
  (let ((l (collect-hash-table *vertices*)))
  (loop-l l graph-id))
  v-list
)



(defun collect-hash-table (ht)
  (let (kvs)
  (maphash #'(lambda (k v) (push k kvs)) ht)
  kvs))



;(defun new-arc (graph-id vertex-id vertex-id &weight))

;(defun graph-arcs (graph-id))
