(defparameter *vertices* (make-hash-table :test #'equal))

(defparameter *graphs* (make-hash-table :test #'equal))

(defparameter *arcs* (make-hash-table :test #'equal))

(defparameter *visited* (make-hash-table :test #'equal))

(defparameter *vertex-keys* (make-hash-table :test #'equal))

(defparameter *previous* (make-hash-table :test #'equal))

(defparameter *heaps* (make-hash-table :test #'equal))


(defun is-graph (graph-id)()
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  (maphash #'(lambda (k v) (when (appartiene v graph-id) (remhash k *vertices*))) *vertices*)
  (maphash #'(lambda (k v) (when (appartiene v graph-id) (remhash k *arcs*))) *arcs*))

(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id)
		 *vertices*)
	(list 'vertex graph-id vertex-id)))

(defun new-arc (graph-id vertex-id-1 vertex-id-2 &optional (weight 1))
  (setf (gethash (list 'arc graph-id vertex-id-1 vertex-id-2 weight)
		 *arcs*)
	(list 'arc graph-id vertex-id-1 vertex-id-2 weight)))

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
      kvs))

(defun graph-vertices(graph-id)
  (funcall 'collect-hash-table *vertices* graph-id))

(defun graph-arcs(graph-id)
  (funcall 'collect-hash-table *arcs* graph-id))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (funcall 'collect-hash-table *arcs* graph-id vertex-id))

(defun graph-vertex-adjacent (graph-id vertex-id)
  (let ((arcs (graph-vertex-neighbors graph-id vertex-id))
	(vertex-list '()))
    (mapcar #'(lambda (x)
		(if (eql vertex-id (third x))
		    (push (list 'vertex graph-id (fourth x))
			  vertex-list)))
	    arcs)
    vertex-list))

(defun graph-print (graph-id)
  (let ((x (funcall 'graph-vertices graph-id))
	(y (funcall 'graph-arcs graph-id)))
    (format t "Vertici: ~S ~%Archi: ~S ~%" x y)))

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
	    (list 'heap heap-id 0 (make-array capacity :adjustable t)))))

(defun heap-id (heap-rep)
  (if (null heap-rep)
      nil)
  (second heap-rep))

(defun heap-size (heap-rep)
  (if (null heap-rep)
      nil)
  (third heap-rep))

(defun heap-actual-heap (heap-rep)
  (if (null heap-rep)
      nil)
  (fourth heap-rep))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*))

(defun heap-empty (heap-id)
  (let ((x  (funcall 'heap-size (gethash heap-id *heaps*))))
    (= x 0)))

(defun heap-not-empty (heap-id)
  (null (heap-empty heap-id)))

(defun get-heap (heap-id)
  (heap-actual-heap (gethash heap-id *heaps*)))

(defun get-heap-key (heap-id i)
  (first (aref (get-heap heap-id) i)))

(defun less (heap-id i j)
  (< (get-heap-key heap-id i) (get-heap-key heap-id j)))

(defun swim (i heap-id)
  (cond ((<= i 0) nil)
	((< (get-heap-key heap-id (parent i)) (get-heap-key heap-id i)) nil)
	(t (scambia (get-heap heap-id) i (parent i))
	   (swim (parent i) heap-id))))

(defun heapify (heap-id i)
  (let ((r (right i))
	(l (left i))
	(smallest i)
	(n (heap-size (gethash heap-id *heaps*))))
	(if (and (< l n)
	    (less heap-id l i))
	    (setf smallest l))
	(if (and (< r n)
	    (less heap-id r smallest))
	    (setf smallest r))
	(if (/= smallest i)
	    (scambia (get-heap heap-id) i smallest)
	    (heapify heap-id smallest))))

(defun parent (i)
  (floor (- i 1) 2))

(defun left(i)
  (+ (* 2 i) 1))

(defun right(i)
  (+ (* 2 i) 2))

(defun scambia (heap i smallest)
  (let ((tmp (aref heap i)))
    (setf (aref heap i) (aref heap smallest))
    (setf (aref heap smallest) tmp))
  nil)

(defun heap-head (heap-id)
  (aref (heap-actual-heap (gethash heap-id *heaps*)) 0))
      
(defun heap-extract (heap-id)
  (if (heap-empty heap-id)
      nil
      (let ((head (heap-head heap-id))
	    (last-element (1- (heap-size (gethash heap-id *heaps*)))))
	(scambia (get-heap heap-id) 0 last-element)
	(adjust-array (get-heap heap-id) last-element)
	(setf (third (gethash heap-id *heaps*)) last-element)
	(heapify heap-id 0)
	head)))

(defun heap-modify-key (heap-id new-key old-key v)
  (if (> new-key old-key)
      nil)
  (let ((i (position old-key (get-heap heap-id) :key #'first)))
    (setf (aref (get-heap heap-id) i) (list new-key v))
    (swim i heap-id)))

(defun heap-insert (heap-id k v)
  (let ((hs (heap-size (gethash heap-id *heaps*))))
    (setf (third (gethash heap-id *heaps*)) (1+ hs))
    (adjust-array (get-heap heap-id) (1+ hs))
    (setf (aref (get-heap heap-id) hs) (list k v))
    (swim hs heap-id)))

(defun heap-print (heap-id)
  (gethash heap-id *heaps*))

(defun create-heap ()
  (new-heap 'h1 15)
  (heap-insert 'h1 6 'o)
  (heap-insert 'h1 13 'p)
  (heap-insert 'h1 20 'w)
  (heap-insert 'h1 11 'x)
  (heap-insert 'h1 5 'y)
  (heap-insert 'h1 1 'a)
  (heap-insert 'h1 2 'b)
  (heap-insert 'h1 3 'c)
  (heap-insert 'h1 4 'd)
  (heap-insert 'h1 7 'e)
  (heap-insert 'h1 8 'f)
  (heap-insert 'h1 9 'g)
  (heap-insert 'h1 14 'i)
  (heap-insert 'h1 10 'h)
  (heap-insert 'h1 12 'l))

