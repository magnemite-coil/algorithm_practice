(defun insertion-sort (array)
  (let* ((a (copy-seq array))
         (states (list (copy-seq a))))

    (loop for i from 1 to (1- (length a)) do
          (let ((j (1- i))
                (v (aref a i)))

            (loop while (and (>= j 0) (> (aref a j) v)) do
                  (setf (aref a (1+ j)) (aref a j))
                  (decf j))

            (setf (aref a (1+ j)) v)
            (push (copy-seq a) states)))

    (nreverse states)))


(defun print-array-elements (a &optional (stream *standard-output*))
  (format stream "~{~A~^ ~}~%" (coerce a 'list)))


(defun main ()
  (let* ((n (read))
         (a (make-array n)))

    (loop for i from 0 to (1- n) do
          (setf (aref a i) (read)))

    (loop for state in (insertion-sort a) do
          (print-array-elements state))))

(main)
