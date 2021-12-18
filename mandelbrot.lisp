;; GLOBAL VARS

;; Change window here:

(defvar *WUPPER* 1.6)       ;; Real left
(defvar *WLOWER* -1.6)      ;; Real right
(defvar *HUPPER* 1.6)       ;; Imag left
(defvar *HLOWER* -1.6)      ;; Imag right

;; Change output window here

(defvar *WIDTH* 100)        ;; Out Real
(defvar *HEIGHT* 150)       ;; Out Imag

;; Change iteration depth here
(defvar *ITERDEPTH* 50)     ;; Iteration depth

;; Change zoom location here

(defvar *ZOOMREAL* -1.2)    ;; Real location
(defvar *ZOOMIM* -0.8)      ;; Imag location

;; Change zoom speed here

(defvar *SPEED* 0.99)

;; Change frame count here

(defvar *FRAMES* 3)

;; Change ascii gradient here
(defvar *GRADIENT* (list "." "," "-" "^" "*" "+" ";" "?" "$" "&" "@" "#"))

;; The actual m-set calculation
(defvar new-real nil)
(defvar new-imag nil)
(defun calc-point(real im real-z im-z iter)
    ;; (format t "R: ~a I: ~a Rz: ~a Iz: ~a Iter: ~a ~%" real im real-z im-z iter)
    (if(= iter 0)
    (return-from calc-point " "))
    
    (if(or (> real 2.0)(< real -2.0))
    (return-from calc-point (nth (mod iter 12) *GRADIENT*)))
    
    (if(or (> im 2.0)(< im -2.0))
    (return-from calc-point (nth (mod iter 12) *GRADIENT*)))
    
    (setq new-real (+ (- (* real real) (* im im)) real-z))
    (setq new-imag (+ im-z (* (* real im) 2.0)))  
    
    (return-from calc-point (calc-point new-real new-imag real-z im-z (- iter 1))))

;; Applies the m-set calculation across the whole window
(defun calc-array (w-lower w-upper h-lower h-upper iter)
    (dotimes (i *WIDTH*)
        (dotimes (j *HEIGHT*)
            (defvar temp-real-z NIL)
            (defvar temp-im-z NIL)
            (setq temp-real-z (- w-upper w-lower))
            (setq temp-im-z (- h-upper h-lower))
            
            (setq temp-real-z (/ temp-real-z *WIDTH*))
            (setq temp-im-z (/ temp-im-z *HEIGHT*))
            
            (setq temp-real-z (+ (* i (abs temp-real-z)) w-lower))
            (setq temp-im-z (- h-upper (* j (abs temp-im-z))))
            
            (defvar mandel-plot NIL)
            ;;(format t "~a ~a ~%" temp-real-z temp-im-z)
            (setq mandel-plot (calc-point temp-real-z temp-im-z temp-real-z temp-im-z iter))
            ;; (terpri)
            (format t "~a" mandel-plot))
            (terpri)))
            
;; Calculates new bounds
(defun calc-bounds(w-lower w-upper h-lower h-upper)
    (defvar w-lower-n NIL)
    (defvar w-upper-n NIL)
    (defvar h-lower-n NIL)
    (defvar h-upper-n NIL)
    (setq w-lower-n (/ (+ *ZOOMREAL* (* w-lower *SPEED*)) 2.0))
    (setq w-upper-n (/ (+ *ZOOMREAL* (* w-upper *SPEED*)) 2.0))
    (setq h-lower-n (/ (+ *ZOOMIM* (* h-lower *SPEED*)) 2.0))
    (setq h-upper-n (/ (+ *ZOOMIM* (* h-upper *SPEED*)) 2.0))
    
    (defvar new-bounds (list w-lower-n w-upper-n h-lower-n h-upper-n))
    
    (return-from calc-bounds new-bounds))
    
(defun main()

    (defvar w-lower *WLOWER*)
    (defvar w-upper *WUPPER*)
    (defvar h-lower *HLOWER*)
    (defvar h-upper *HUPPER*)
    
    (defvar curr-iter *ITERDEPTH*)
    (defvar frame-count 0)
    
    (loop 
    
            (when (>= frame-count *FRAMES*)
                (return-from main 0))
                
            (calc-array w-lower w-upper h-lower h-upper curr-iter)
            (defvar new-bounds (calc-bounds w-lower w-upper h-lower h-upper))
            
            (setq w-lower (car new-bounds))
            (setq w-upper (cadr new-bounds))
            (setq h-lower (caddr new-bounds))
            (setq h-upper (cadddr new-bounds))
            
            (setq curr-iter (ceiling (* curr-iter 1.05)))
            (setq frame-count (+ frame-count 1))
            (terpri))
    (return-from main 0))
    
(main)
;; GLOBAL VARS

;; Change window here:

(defvar *WUPPER* 2.0)       ;; Real left
(defvar *WLOWER* -2.6)      ;; Real right
(defvar *HUPPER* 2.0)       ;; Imag left
(defvar *HLOWER* -2.0)      ;; Imag right

;; Change output window here

(defvar *WIDTH* 100)        ;; Out Real
(defvar *HEIGHT* 150)       ;; Out Imag

;; Change iteration depth here
(defvar *ITERDEPTH* 50)     ;; Iteration depth

;; Change zoom location here

(defvar *ZOOMREAL* -1.2)    ;; Real location
(defvar *ZOOMIM* -0.8)      ;; Imag location

;; Change zoom speed here

(defvar *SPEED* 0.99)

;; Change frame count here

(defvar *FRAMES* 10)

;; Change ascii gradient here
(defvar *GRADIENT* (list "." "," "-" "^" "*" "+" ";" "?" "$" "&" "@" "#"))

;; The actual m-set calculation
(defvar new-real nil)
(defvar new-imag nil)
(defun calc-point(real im real-z im-z iter)
    ;; (format t "R: ~a I: ~a Rz: ~a Iz: ~a Iter: ~a ~%" real im real-z im-z iter)
    (if(= iter 0)
    (return-from calc-point " "))
    
    (if(or (> real 2.0)(< real -2.0))
    (return-from calc-point (nth (mod iter 12) *GRADIENT*)))
    
    (if(or (> im 2.0)(< im -2.0))
    (return-from calc-point (nth (mod iter 12) *GRADIENT*)))
    
    (setq new-real (+ (- (* real real) (* im im)) real-z))
    (setq new-imag (+ im-z (* (* real im) 2.0)))  
    
    (return-from calc-point (calc-point new-real new-imag real-z im-z (- iter 1))))

;; Applies the m-set calculation across the whole window
(defun calc-array (w-lower w-upper h-lower h-upper iter)
    (dotimes (i *WIDTH*)
        (dotimes (j *HEIGHT*)
            (defvar temp-real-z NIL)
            (defvar temp-im-z NIL)
            (setq temp-real-z (- w-upper w-lower))
            (setq temp-im-z (- h-upper h-lower))
            
            (setq temp-real-z (/ temp-real-z *WIDTH*))
            (setq temp-im-z (/ temp-im-z *HEIGHT*))
            
            (setq temp-real-z (+ (* i (abs temp-real-z)) w-lower))
            (setq temp-im-z (- h-upper (* j (abs temp-im-z))))
            
            (defvar mandel-plot NIL)
            ;;(format t "~a ~a ~%" temp-real-z temp-im-z)
            (setq mandel-plot (calc-point temp-real-z temp-im-z temp-real-z temp-im-z iter))
            ;; (terpri)
            (format t "~a" mandel-plot))
            (terpri)))
            
;; Calculates new bounds
(defun calc-bounds(w-lower w-upper h-lower h-upper)
    (defvar w-lower-n NIL)
    (defvar w-upper-n NIL)
    (defvar h-lower-n NIL)
    (defvar h-upper-n NIL)
    (setq w-lower-n (/ (+ *ZOOMREAL* (* w-lower *SPEED*)) 2.0))
    (setq w-upper-n (/ (+ *ZOOMREAL* (* w-upper *SPEED*)) 2.0))
    (setq h-lower-n (/ (+ *ZOOMIM* (* h-lower *SPEED*)) 2.0))
    (setq h-upper-n (/ (+ *ZOOMIM* (* h-upper *SPEED*)) 2.0))
    
    (defvar new-bounds (list w-lower-n w-upper-n h-lower-n h-upper-n))
    
    (return-from calc-bounds new-bounds))
    
(defun main()

    (defvar w-lower *WLOWER*)
    (defvar w-upper *WUPPER*)
    (defvar h-lower *HLOWER*)
    (defvar h-upper *HUPPER*)
    
    (defvar curr-iter *ITERDEPTH*)
    (defvar frame-count 0)
    
    (loop 
    
            (when (>= frame-count *FRAMES*)
                (return-from main 0))
                
            (calc-array w-lower w-upper h-lower h-upper curr-iter)
            (defvar new-bounds (calc-bounds w-lower w-upper h-lower h-upper))
            
            (setq w-lower (car new-bounds))
            (setq w-upper (cadr new-bounds))
            (setq h-lower (caddr new-bounds))
            (setq h-upper (cadddr new-bounds))
            
            (setq curr-iter (ceiling (* curr-iter 1.05)))
            (setq frame-count (+ frame-count 1))
            (terpri))
    (return-from main 0))
    
(main)

















