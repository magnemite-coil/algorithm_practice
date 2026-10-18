;;; 挿入ソートの各段階の配列を返す
(defun insertion-sort (array)
  (let* ((a (copy-seq array))
         (states (list (copy-seq a))))
    (loop for i from 1 below (length a) do
          (let ((v (aref a i))
                (j (1- i)))
            (loop while (and (>= j 0) (> (aref a j) v)) do
                  (setf (aref a (1+ j)) (aref a j))
                  (decf j))
            (setf (aref a (1+ j)) v)
            (push (copy-seq a) states)))
    (nreverse states)))


;;; 配列を出力する
(defun print-array-elements (a &optional (stream *standard-output*))
  (format stream "~{~a~^ ~}~%" (coerce a 'list)))




;;; 入出力を担当する
(defun main ()
  (let* ((n (read))
         (a (make-array n)))
    (dotimes (i n)
      (setf (aref a i) (read)))
    (dolist (state (insertion-sort a))
      (print-array-elements state))))

(main)
