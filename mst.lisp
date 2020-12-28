(defparameter *vertices* (make-hash-table :test #'equal))

(defparameter *graphs* (make-hash-table :test #'equal))

(defparameter *arcs* (make-hash-table :test #'equal))

(defparameter *visited* (make-hash-table :test #'equal))

(defparameter *vertex-keys* (make-hash-table :test #'equal))

(defparameter *previous* (make-hash-table :test #'equal))

(defun is-graph (graph-id)()
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))

(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id)
		 *vertices*)
	(list 'vertex graph-id vertex-id)))

(defun new-arc (graph-id vertex-id-1 vertex-id-2 &optional (weight 1))
  (setf (gethash (list 'arc graph-id vertex-id-1 vertex-id-2 weight)
		 *arcs*)
	(list 'arc graph-id vertex-id-1 vertex-id-2 weight)))

;(defun graph-print(graph-id)
 ; (print (graph-vertices graph-id))
  ;(print (graph-arcs graph-id)))

(defun appartiene (lista graph-id &optional (vertex-id nil))
  (if (null vertex-id)
      (if (eql graph-id (second lista))
	 (return-from appartiene T))
      nil)
  (if (and(eql graph-id (second lista))
	  (eql vertex-id (third lista)))
      (return-from appartiene T))
  nil)

(defun collect-hash-table (ht graph-id &optional (vertex-id nil))
    (let ((kvs ()))
      (maphash #'(lambda (k v)(when (appartiene v graph-id vertex-id)(push k kvs))) ht)
      ;;(maphash #'(lambda (k v)(when (eql v '(vertex g2 x))(gethash k ht))) ht))
      kvs))

(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (maphash #'(lambda (k v) (when (appartiene v graph-id) (remhash k *vertices*))) *vertices*))

(defun graph-vertices(graph-id)
  (funcall 'collect-hash-table *vertices* graph-id))

(defun graph-arcs(graph-id)
  (funcall 'collect-hash-table *arcs* graph-id))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (funcall 'collect-hash-table *arcs* graph-id vertex-id))
  
