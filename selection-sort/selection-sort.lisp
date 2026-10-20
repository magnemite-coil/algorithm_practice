(defun selection-sort (arr)
  (let ((a (copy-seq arr))
        (n (length arr))
        (swaps 0))

    (loop for i from 0 to (- n 2) do
          (let ((minj i))

            (loop for j from i to (1- n) do
                  (when (< (aref a j) (aref a minj))
                    (setf minj j)))

            (unless (= i minj)
              (rotatef (aref a i) (aref a minj))
              (incf swaps))))

    (values a swaps)))


;;; 配列を読み込む
(defun read-array ()
  (let* ((n (read))
         (arr (make-array n)))

    (loop for i from 0 to (1- n) do
          (setf (aref arr i) (read)))
    arr))


;;; 配列を出力する
(defun print-array-elements (arr)
  (format t "~{~A~^ ~}~%" (coerce arr 'list)))


;;; メイン処理
(defun main ()
  (let ((arr (read-array)))

    (multiple-value-bind (sorted-array swaps)
        (selection-sort arr)

      (print-array-elements sorted-array)
      (format t "~D~%" swaps))))


(main)
