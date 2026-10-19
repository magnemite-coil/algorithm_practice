;;; バブルソートを実行し、ソート後の配列と交換回数を返す
(defun bubble-sort (array)
  (let ((a (copy-seq array))
        (n (length array))
        (swaps 0)
        (flag t))
    (loop for i from 0 to (1- n)
          while flag do
          (setf flag nil)
          (loop for j from (1- n) downto (1+ i)
                when (< (aref a j) (aref a (1- j))) do
                (rotatef (aref a j) (aref a (1- j)))
                (setf flag t)
                (incf swaps)))
    (values a swaps)))


;;; 標準入力から配列を読み込む
(defun read-array ()
  (let* ((n (read))
         (array (make-array n)))
    (loop for i from 0 to (1- n) do
          (setf (aref array i) (read)))
    array))


;;; 配列を出力する
(defun print-array-elements (array)
  (format t "~{~a~^ ~}~%" (coerce array 'list)))


;;; メイン処理
(defun main ()
  (let ((array (read-array)))
    (multiple-value-bind (sorted-array swaps)
      (bubble-sort array)
      (print-array-elements sorted-array)
      (format t "~a~%" swaps))))


(main)
