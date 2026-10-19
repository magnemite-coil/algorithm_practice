(defun bubble-sort (arr)
  (let ((a (copy-seq arr))
        (n (length arr))
        (swaps 0)
        (flag t))

    (loop for i from 0 to (1- n)
          while flag do
          (setf flag nil)

          (loop for j from (1- n) downto (1+ i)
                when (> (aref a (1- j)) (aref a j)) do
                (rotatef (aref a (1- j)) (aref a j))
                (setf flag t)
                (incf swaps)))

    (values a swaps)))


(defun read-array ()
  (let* ((n (read))
         (arr (make-array n)))

    (loop for i from 0 to (1- n) do
          (setf (aref arr i) (read)))
    arr))


(defun print-array-elements (arr)
  (format t "~{~A~^ ~}~%" (coerce arr 'list)))


(defun main ()
  (let ((arr (read-array)))
    (multiple-value-bind (sorted-array swaps)
      (bubble-sort arr)
      (print-array-elements sorted-array)
      (format t "~A~%" swaps))))


(main)
