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

;;(defun graph-vertex-adjacent (graph-id vertex-id))

(defun graph-print (graph-id)
  (let ((x (funcall 'graph-vertices graph-id))
	(y (funcall 'graph-arcs graph-id)))
    (format t "Vertici: ~S ~%Archi: ~S ~%" x y)))

;;MINHEAP

;(defparameter heap-rep '())

;(defparameter heap '())

(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
	    (list 'heap heap-id 0 (make-array capacity)))))

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

(defun heap-head (heap-id)
  (aref (heap-actual-heap (gethash heap-id *heaps*)) 0))
      
(defun heap-extract (heap-id)
  (if (heap-empty heap-id)
      nil
      (let ((head (heap-head heap-id))
	    (last-element (1- (heap-size (gethash heap-id *heaps*)))))
	(scambia (get-heap heap-id) 0 last-element);;scambia 0 con ultimo
	(setf (aref (get-heap heap-id) last-element) 0);;
	(setf (third (gethash heap-id *heaps*)) last-element)
	(heapify heap-id 0)
    head)))

(defun swim (i heap)
  (cond ((<= i 0) nil)
	((< (first (aref heap (parent i))) (first (aref heap i))) nil)
	(t (scambia heap i (parent i))
	   (swim (parent i) heap))))

(defun heap-modify-key (heap-id new-key old-key v)
  (if (> new-key old-key)
      nil)
  (let ((i (position old-key (get-heap heap-id) :key #'first)))
    (setf (aref (get-heap heap-id) i) (list new-key v))
     (swim i (get-heap heap-id))))

;; (defun heap-insert (heap-id k v)
;;   (if (= (heap-size heap-rep) (length heap))
;;       nil
;;   (let ((hs (heap-size heap-rep)))
;;     (setf (third heap-rep) (1+ hs))
;;     (setf (aref heap (1- (heap-size heap-rep))) (list k v))
;;     (swim (1- hs)))))

(defun heap-insert (heap-id k v)
  (let ((hs (heap-size (gethash heap-id *heaps*))))
	(cond ((= hs (length (get-heap heap-id))) nil)
	      (t (setf (third (gethash heap-id *heaps*)) (1+ hs))
		 (setf (aref (get-heap heap-id) hs) (list k v))
		 (swim hs (get-heap heap-id))))))
	 
;; (defun create-heap()
;;   (setf heap-rep (new-heap 'h1 10))
;;   (setf (third heap-rep) 9)
;;   (setf heap (heap-actual-heap heap-rep))
;;   (setf (aref heap 0) (list 1 'a))
;;   (setf (aref heap 1) (list 14 'b))
;;   (setf (aref heap 2) (list 3 'c))
;;   (setf (aref heap 3) (list 4 'd))
;;   (setf (aref heap 4) (list 7 'e))
;;   (setf (aref heap 5) (list 8 'f))
;;   (setf (aref heap 6) (list 9 'g))
;;   (setf (aref heap 7) (list 10 'h))
;;   (setf (aref heap 8) (list 2 'i))
;;   (setf (aref heap 9) (list 16 'l))
;;   )

(defun create-heap()
  (new-heap 'h1 5)
  (heap-insert 'h1 10 'e)
  (heap-insert 'h1 1 'a)
  (heap-insert 'h1 3 'c)
  (heap-insert 'h1 2 'b)
  (heap-insert 'h1 4 'd))
  
(defun let-function ()
  (let ((x 2))
    (format t "x nel 1° let: ~a~%" x)
    (let ((y (+ x 10)))
      (format t "x nel 2° let: ~a~%" x)
      (list x y)
      )))

(defun let-function-2()
  (let ((smallest 2))
	   (if (> 1 0)
	       (smallest 3)
	       (smallest 4))))

(defun parent (i)
  (floor (- i 1) 2))

(defun left(i)
  (+ (* 2 i) 1))

(defun right(i)
  (+ (* 2 i) 2))

(defun scambia (heap i smallest)
  (let ((tmp (aref heap i)))
    (setf (aref heap i)
	  (aref heap smallest)
	  (aref heap smallest) tmp)))

(defun get-heap (heap-id)
  (heap-actual-heap (gethash heap-id *heaps*)))

(defun get-heap-key (heap-id i)
  (first (aref (get-heap heap-id) i)))

(defun heapify (heap-id i);;attenzione se lanciata su uno heap cicla infinit.
  (if (<= (heap-size (gethash heap-id *heaps*)) 1)
      nil
  (let ((r (right i))
	(l (left i))
	(smallest i))
	(if (and (< l (heap-size (gethash heap-id *heaps*)))
	    (< (get-heap-key heap-id l)(get-heap-key heap-id i)))
	    (setf smallest l))
	(if (and (< r (heap-size (gethash heap-id *heaps*)))
	    (< (get-heap-key heap-id r) (get-heap-key heap-id smallest)))
	    (setf smallest r))
	(if (/= smallest i)
	    (scambia (get-heap heap-id) i smallest)
	    (heapify heap-id smallest)))))

;; (defparameter r '())
;; (defparameter l '())
;; (defparameter smallest '())

;; (defun heapify (heap i)
;;   (setf r (right i))
;;   (setf l (left i))
;;   (setf smallest i)
;; 	(if (and (< l (heap-size heap-rep))
;; 	    (< (aref heap l)(aref heap i)))
;; 	    (setf smallest l))
;; 	(if (and (< r (heap-size heap-rep))
;; 	    (< (aref heap r) (aref heap smallest)))
;; 	    (setf smallest r))
;; 	(if (/= smallest i)
;; 	    (scambia heap i smallest)
;; 	    (heapify heap smallest)))

;(defun build-min-heap (heap-id)
 ; (setf (third heap-rep) (length heap))
  
