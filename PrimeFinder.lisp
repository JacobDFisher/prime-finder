(defun euclidean-algorithm (num1 num2)
  (let ((remainder (mod num1 num2)))
    (if (eq remainder 0)
        num2
        (euclidean-algorithm num2 remainder))))

(defun save-number (filename num)
  (let ((loadednum))
    (if (probe-file filename)
        (with-open-file (in filename)
          (with-standard-io-syntax
            (setf loadednum (read in))))
        (setf loadednum 0))
    (if (> num loadednum)
        (progn
          (rename-file filename (concatenate 'string filename ".old"))
          (with-open-file (out filename
                               :direction :output
                               :if-exists :supersede)
            (with-standard-io-syntax
              (print num out)))))))

(defun save-list (filename list)
  (let ((listlength))
    (if (probe-file filename)
        (with-open-file (in filename)
          (with-standard-io-syntax
            (setf listlength (list-length (read in)))))
        (setf listlength 0))
    (if (> (list-length list) listlength)
        (progn
          (if (probe-file filename)
              (rename-file filename (concatenate 'string filename ".old")))
          (with-open-file (out filename
                               :direction :output
                               :if-exists :supersede)
            (with-standard-io-syntax
              (print list out)))))))

(defun load-from-file (filename)
  (if (probe-file filename)
      (with-open-file (in filename)
        (with-standard-io-syntax
          (read in)))))

(defun split-sorted-list (list num)
  (if (apply '<= list)
      (do ((prelist nil (append prelist `(,(pop postlist)))) (postlist list))
          ((> (car postlist) num) `(,prelist ,postlist)))))

(defun find-primes-euclidean (end)
  (let* ((prime-list (let ((safety (load-from-file "PrimeList"))) (if safety safety '(2))))
         (split-list (split-sorted-list prime-list (sqrt (car (last prime-list))))))
    (if (>= (car (last prime-list)) end)
        (car (split-sorted-list prime-list end))
        (do*
         ((cur (1+ (car (last prime-list))))
          (prelist (if (car split-list) (car split-list) '(2)) (append prelist `(,(pop postlist))))
          (product (if prelist (apply '* prelist) 1) (* product (car (last prelist))))
          (postlist (if (= 2 (caadr split-list)) (cdadr split-list) (cadr split-list))))
         ((>= cur end) prime-list)
          (do* ((innerend (expt (car (last prelist)) 2)))
               ((or (> cur innerend) (> cur end)))
            (if (eq 1 (euclidean-algorithm product cur))
                (setf prime-list (append prime-list `(,cur)) postlist (append postlist `(,cur))))
            (incf cur))))))

;; (defun is-relative-prime (num list)
;;   (do ((cur (pop list) (pop list)))
;;       ((or (eq (mod num cur) 0) (eq list nil)) (not (eq (mod num cur) 0)))))

(defun is-relative-prime (num1 num2)
  (if (>= num1 num2)
      (eq (mod num1 num2) 0)
      (eq (mod num2 num1) 0)))

(defun sorted-is-relative-prime (greater lesser)
  (not (eq (mod greater lesser) 0)))

(defun sorted-list-is-relative-prime (number list)
  "Number must be greater than all elements of the list"
  (do* ((pos 0 (1+ pos))
        (listlength (1- (list-length list)))
        (cur (nth pos list) (nth pos list))
        (relprime (sorted-is-relative-prime number cur) (sorted-is-relative-prime number cur)))
       ((or (>= pos listlength) (not relprime)) relprime)))

(defun number-sequence (from &optional (to from) (separation 1))
  (let ((return-list nil)) (dotimes (x (floor (/ (1+ (- to from)) separation)) return-list) (setf return-list (append return-list `(,(+ x from)))))))

(defun find-primes (end)
  (let* ((prime-list (let ((safety (load-from-file "PrimeList"))) (if safety safety '(2)))))
    (if (>= (car (last prime-list)) end)
        (car (split-sorted-list prime-list end))
        (do*
         ((cur (1+ (car (last prime-list))))
          (sqrt-cur (floor (sqrt cur)) (1+ sqrt-cur)))
         ((>= cur end) prime-list)
          (do* ((innerend (expt sqrt-cur 2))
                (num-list (number-sequence 2 (1- cur))))
               ((or (> cur innerend) (> cur end)))
            (if (sorted-list-is-relative-prime cur num-list)
                (setf prime-list (append prime-list `(,cur))))
            (incf cur))))))

(defun find-primes-optimized (end)
  (let* ((prime-list (let ((safety (load-from-file "PrimeList"))) (if safety safety '(2))))
         (split-list (split-sorted-list prime-list (sqrt (car (last prime-list))))))
    (if (>= (car (last prime-list)) end)
        (car (split-sorted-list prime-list end))
        (do*
         ((cur (1+ (car (last prime-list))))
          (prelist (if (car split-list) (car split-list) '(2)) (append prelist `(,(pop postlist))))
          (postlist (if (= 2 (caadr split-list)) (cdadr split-list) (cadr split-list))))
         ((>= cur end) prime-list)
          (do* ((innerend (expt (car (last prelist)) 2)))
               ((or (> cur innerend) (> cur end)))
            (if (sorted-list-is-relative-prime cur prelist)
                (setf prime-list (append prime-list `(,cur)) postlist (append postlist `(,cur))))
            (incf cur))))))

(defmacro get-program-run-time (test-program)
  (let ((start-time-name (gensym)))
    `(let ((,start-time-name (get-internal-run-time)))
       ,test-program
       (- (get-internal-run-time) ,start-time-name))))

(defun get-program-run-times (test-program start end &optional (factor 2))
  (do* ((cur (progn (format t "~d~%" start) start) (progn (format t "~d~%" (* factor cur)) (* factor cur)))
        (times nil (append times `(,(get-program-run-time (funcall test-program cur))))))
      ((>= cur end) times)))

(defun heap-new ()
  (list 1))

(defun heap-insert-item (heap val)
  (nconc heap `(,(gensym)))
  (do* ((hole (nth 0 heap) parent)
        (parent (floor (/ hole 2)) (floor (/ parent 2))))
       ((or (= hole 1) (>= (car val) (car (nth parent heap)))) (setf (nth hole heap) val))
    (setf (nth hole heap) (nth parent heap)))
  (incf (nth 0 heap)))

(defun heap-not-leaf (heap index)
  (if (> (length heap) (* index 2))
      (if (> (length heap) (1+ (* index 2)))
          nil 0.5)
      1))

(defun heap-smaller-child (heap index)
  (let ((children (heap-not-leaf heap index)))
    (if (not children)
        (if (< (car (nth (* index 2) heap)) (car (nth (1+ (* index 2)) heap))) (* index 2) (1+ (* index 2)))
        (if (= children 0.5) (* index 2) nil))))

(defun heap-remove-min (heap)
  (when (not (= 1 (length heap)))
    (do* ((val (caar (last heap)))
          (current 1 smaller-child)
          (smaller-child (heap-smaller-child heap current) (heap-smaller-child heap current)))
         ((or (not smaller-child) (<= val (car (nth smaller-child heap)))) (setf (nth current heap) (nth (1- (nth 0 heap)) heap)))
      (setf (nth current heap) (nth smaller-child heap)))
    (decf (nth 0 heap))
    (nbutlast heap)))

(defun heap-get-min (heap)
  (cadr heap))

(defun find-primes-sieve (end)
  (let* ((prime-list (let ((safety (load-from-file "PrimeList"))) (if safety safety '(2))))
         (prime-heap (heap-new)))
    (if (>= (car (last prime-list)) end)
        (car (split-sorted-list prime-list end))
        (progn
          (dotimes (x (length prime-list)) (heap-insert-item prime-heap (list (expt (nth x prime-list) 2) (nth x prime-list) x)))
          (do ((max-prime (car (last prime-list)))
               (min-heap (heap-get-min prime-heap) (heap-get-min prime-heap)))
              ((> (car min-heap) max-prime))
            (heap-insert-item prime-heap
                              (list
                               (* (car min-heap) (nth (caddr min-heap) prime-list))
                               (car min-heap)
                               (caddr min-heap)))
            (heap-insert-item prime-heap
                              (list
                               (* (cadr min-heap) (nth (1+ (caddr min-heap)) prime-list))
                               (cadr min-heap)
                               (1+ (caddr min-heap))))
            (heap-remove-min prime-heap))
          (do ((cur (1+ (car (last prime-list))) (1+ cur))
               (min-heap (heap-get-min prime-heap) (heap-get-min prime-heap)))
              ((> cur end) prime-list)
            (if (= cur (car (heap-get-min prime-heap)))
                (progn
                  (if (<= (* (car min-heap) (nth (caddr min-heap) prime-list)) (1+ end))
                      (heap-insert-item
                       prime-heap
                       (list
                        (* (car min-heap) (nth (caddr min-heap) prime-list))
                        (car min-heap)
                        (caddr min-heap))))
                  (if (<= (* (cadr min-heap) (nth (1+ (caddr min-heap)) prime-list)) (1+ end))
                      (heap-insert-item
                       prime-heap
                       (list
                        (* (cadr min-heap) (nth (1+ (caddr min-heap)) prime-list))
                        (cadr min-heap)
                        (1+ (caddr min-heap)))))
                  (heap-remove-min prime-heap))
                (progn
                  (if (<= (expt cur 2) (1+ end))
                      (heap-insert-item
                       prime-heap
                       (list
                        (expt cur 2)
                        cur
                        (length prime-list))))
                  (setf prime-list (append prime-list `(,cur))))))))))
