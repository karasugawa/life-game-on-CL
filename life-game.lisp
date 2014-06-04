;;;conwayのlife-game

(defparameter *width* 50)
(defparameter *hight* 20)
(defparameter *field* (make-array `(,*hight* ,*width*) :initial-element '_))

;;テスト用にグライダーを作る
(setf (aref *field* 0 1) 'o)
(setf (aref *field* 1 2) 'o)
(setf (aref *field* 2 0) 'o)
(setf (aref *field* 2 1) 'o)
(setf (aref *field* 2 2) 'o)

;;リセットする関数
(defun reset ()
  (setf *field* (make-array (list *hight* *width*) :initial-element '_))
  (setf (aref *field* 0 1) 'o)
  (setf (aref *field* 1 2) 'o)
  (setf (aref *field* 2 0) 'o)
  (setf (aref *field* 2 1) 'o)
  (setf (aref *field* 2 2) 'o))

;;与えられた座標のムーア近傍を求める関数
(defun get-moore (x y)
  (labels ((check (n dimention)
	     (cond ((= n 0) (list n (1+ n)))
		   ((= n (1- dimention)) (list (1- n) n))
		   (t (list (1- n) n (1+ n))))))
    (list (check y *hight*) (check x *width*))))

;;ムーア近傍の座標の値を求める関数
(defun get-moore-value (x y field)
  (let ((moore (get-moore x y)))
    (mapcar (lambda (ay)                           ;各ｙの値に対してｘを適用し、一列ずつ値を求める
	      (mapcar (lambda (ax)
			(if (and (= x ax) (= y ay))
			    'c                     ;現在の座標はcとする
			    (aref field ay ax)))
		      (cadr moore)))
	    (car moore))))

;;ムーア近傍でオンになっているセルの数を求める関数
(defun num-of-on-cell (x y field)
  (let ((moore-cell (apply #'nconc (get-moore-value x y field)))) ;再使用しないのでappendではなくnconcとした
    (labels ((f (moore-cell acc)
	       (cond ((null moore-cell) acc)
		     ((eq (car moore-cell) 'o)
		      (f (cdr moore-cell) (1+ acc)))
		     (t (f (cdr moore-cell) acc)))))
      (f moore-cell 0))))

;;現在のフィールドを表示する
(defun dump-field (field)
  (loop for y below *hight*
       do (progn (loop for x below *width*
		      do (princ (if (eq '_ (aref field y x))
				    #\.
				    #\O)))
		 (fresh-line))))

;;次世代でのセルのオンオフを求める関数
(defun onoff (x y field)
  (let ((on-cell (num-of-on-cell x y field)))
    (if (eq (aref field y x) '_)
	(if (= on-cell 3)
	    'o
	    '_)
	(if (or (= on-cell 2) (= on-cell 3))
	    'o
	    '_))))

;;フィールド全体にわたって次世代の様子を求める関数
(defun next ()
  (loop for n below *hight*
       collect (loop for m below *width*
		    collect (onoff m n *field*))))

;;nextの値を*field*に代入していく関数
(defun lkouho->array (next)
  (labels ((f (first lst acc-x acc-y)
	     (unless (and (null first) (null lst))
	       (if (null first)
		   (f (car lst) (cdr lst) 0 (1+ acc-y))
		   (progn (setf (aref *field* acc-y acc-x)
				(car first))
			  (f (cdr first) lst (1+ acc-x) acc-y))))))
    (f (car next) (cdr next) 0 0)))

;;n世代進める関数
(defun conway (n)
  (loop for m below n
       do (progn (dump-field *field*)
		 (lkouho->array (next)))))
