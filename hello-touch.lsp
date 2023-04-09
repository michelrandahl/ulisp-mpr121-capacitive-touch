; simple example program for 

(defun scan (port)
  (dotimes (p 127)
    (with-i2c (str port p) (when str (print p)))))
; external stuff is on port 0 for micro:bit v2
(scan 0)
; returns 91 for me, which in hexa is x5B, so we use x5B as the address moving forward

; head of array
(defun ahead (xs)
  (aref xs 0))

(defun range (start end)
  (let ((xs nil)
        (to (- end 1)))
    (dotimes (i (- end start))
      (push (- to i) xs))
    xs))

(defun print-str-num (s n)
  (print
    (concatenate
      'string
      s ": "
      (with-output-to-string (str) (princ n str)))))

(defun repeat (n f)
  (mapcar (lambda (i) (f)) (range 0 n)))

(defun repeat-arr (n f)
  (let ((arr (make-array n)))
    (dotimes (i n)
      (setf (aref arr i) (f)))
    arr))

(defun forall (f xs)
  (let ((result t))
    (dolist (x xs)
      (when (not (f x))
        (setf result nil)
        (return)))
    result))

; update an index in an array
(defun update-with (arr i f)
  (let ((x (aref arr i)))
    (setf (aref arr i) (f i x))))

; map a function over an array and return the results as a list
(defun maparr (f arr)
  (let ((xs nil))
    (dotimes (i (length arr))
      (push (update-with arr i f) xs))
    xs))

(defun map-indexed (f xs)
  (mapcan (lambda (i x) (list (f i x))) (range 0 (length xs)) xs))

(defvar irq-pin 8)

(defvar mpr121-addr #x5B)

;(defun mpr121-i2c (stream-fn)
;  (with-i2c (str 0 mpr121-addr)
;    (stream-fn str)))
(defun write-bytes (register-addr xs)
  (with-i2c (str 0 mpr121-addr)
            (write-byte register-addr str)
            (dolist (x xs)
              (write-byte x str))))

(defun read-bytes (register-addr n)
  (with-i2c (str 0 mpr121-addr)
            (write-byte register-addr str)
            (restart-i2c str n)
            (repeat n (lambda () (read-byte str)))))

(defvar soft-reset #x80)

(defvar soft-reset-val #x63)

(defvar electrode-configuration #x5E)

(defvar touch-status-reg #x00)

(defun set-treshold-values (touch-treshold release-treshold)
  (with-i2c (str 0 mpr121-addr)
            (write-byte #x41 str)
            (dotimes (x 12)
              (write-byte touch-treshold str)
              (write-byte release-treshold str))))

(defun analog-frontend-and-filter ()
  (with-i2c (str 0 mpr121-addr)
            (write-byte #x5C str)
            (write-byte #x10 str)
            (write-byte #x24 str)
            (write-byte #x80 str)))

(defun auto-config ()
  (with-i2c (str 0 mpr121-addr)
            (write-byte #x7B str)
            (write-byte #x0B str)
            (write-byte #x80 str)
            (write-byte #xC8 str)
            (write-byte #x82 str)
            (write-byte #xB4 str)))

(defun setup (config-val)
  (with-i2c (str 0 mpr121-addr)
            (write-byte soft-reset str)
            (write-byte soft-reset-val str))
  (set-treshold-values 10 8)
  (analog-frontend-and-filter)
  (auto-config)
  (with-i2c (str 0 mpr121-addr)
            (write-byte electrode-configuration str)
            (write-byte config-val str)))

(defun read-touch-status ()
  (with-i2c (str 0 mpr121-addr)
            (write-byte touch-status-reg str)
            (restart-i2c str 2)
            (list
              (read-byte str)
              (read-byte str))))

(defvar buttons
  '((#b000000000001 . one)
    (#b000000000010 . four)
    (#b000000000100 . seven)
    (#b000000001000 . *)
    (#b000000010000 . two)
    (#b000000100000 . five)
    (#b000001000000 . eight)
    (#b000010000000 . zero)
    (#b000100000000 . three)
    (#b001000000000 . six)
    (#b010000000000 . nine)
    (#b100000000000 . "#")))

(defun match-button (input-val)
  (let ((result nil))
    (dolist (button buttons)
      (let ((button-val (car button)))
        (when (not (zerop (logand button-val input-val)))
          (setf result (cdr button))
          (return))))
    result))
(match-button #b100)
(when (not (zerop (logand #b100 #b100)))
  (print "hello"))

; TODO how to find the matching bitmask for the pressed button...?
; perhaps combine with the pressure data when determining which button it is
(assoc #b00000001 buttons)

(defun interpret-read (read-res)
  (let* ((lsb (car read-res))
         (msb (car (cdr read-res)))
         (combined (logior (ash msb 8) lsb))
         (sensor-data (logand combined #b0000111111111111))
         (over-current-flag (logand msb #b10000000)))
    (list (cons 'sensor-data sensor-data)
          (cons 'over-current-flag over-current-flag))))

; howto alist
;(defvar hello 42)
;(assoc 'foo (list (cons 'foo hello)
;                  (cons 'bar "stuff")))
;(assoc ':foo '((:foo . #.hello)))


(defvar enable-proximity (ash #b0011 4))

(defvar enable-all-touch-sensors #b1100)

;(defun read-capacitive-touch2 ()
;  (setup enable-all-touch-sensors)
;  (print "setup is done")
;  (loop (print (interpret-read (read-touch-status)))))

;(defun wait-for-touch-status-clear ()
;  (loop
;    (let ((status (interpret-read (read-touch-status))))
;      (when (zerop status)
;        (print "ready")
;        (return))
;      (print "waiting")
;      (delay 1000))))

(defvar electrode-1-filtered-data #x04)

(defvar electrode-1-baseline #x1E)

(defun read-filtered-data (electrode-filtered-data-reg)
  (with-i2c (str 0 mpr121-addr)
            (write-byte electrode-filtered-data-reg str)
            (restart-i2c str 2)
            (let ((lsb (read-byte str))
                  (msb (read-byte str)))
              (logior (ash msb 8) lsb))))

(defun read-baseline (electrode-baseline-reg)
  (with-i2c (str 0 mpr121-addr)
            (write-byte electrode-baseline-reg str)
            (restart-i2c str 1)
            (read-byte str)))

(defvar calibrated-baseline-values nil)

(defun mk-wrapped-index-inc-fn (size)
  (let ((index -1))
    (lambda ()
      (incf index)
      (if (<= size index)
        (setf index 0)
        index))))

(defun mk-ring-buffer (size)
  (let ((buffer (make-array size :initial-element 0))
        (get-index (mk-wrapped-index-inc-fn size)))
    (lambda (new-val)
      (setf (aref buffer (get-index)) new-val)
      buffer)))

(defun array-all-equal (xs)
  (not
    (dotimes (i (- (length xs) 1))
      (when (not (= (aref xs i) (aref xs (+ i 1))))
        (return t)))))

(defun list-all-equal (xs)
  (let ((first-el (car xs)))
    (not
      (dolist (x (cdr xs))
        (when (not (= first-el x))
          (return t))))))

;; TODO we want to calibrate all the baseline registers... perhaps its enough to look at the data for just one of them?
; This function simply continues running until the last five baseline values has stabalized to the same value.
; The MPR121 controller will keep adjusting the baseline registers on each touch, but will eventually stabalize
(defun calibrate-baseline-register (electrode-baseline-reg)
  (let ((ring-buffer (mk-ring-buffer 5)))
    (loop
      (when (not (digitalread irq-pin))
        (let* ((touch-data (interpret-read (read-touch-status)))
               (activated-sensors (cdr (assoc 'sensor-data touch-data))))
          (when (not (zerop activated-sensors))
            (let* ((baseline (read-baseline electrode-baseline-reg))
                   (xs (ring-buffer baseline)))
              (print xs)
              (when (and (not (zerop (ahead xs)))
                         (array-all-equal xs))
                (return)))))))))

(defun calibrate-all-sensors ()
  (dotimes (i 12)
    (print-str-num "calibrating sensor" i)
    (calibrate-baseline-register (+ electrode-1-baseline i))))

(defun calibration-process ()
  (setup enable-all-touch-sensors)
  (print "setup done, waiting for controller to be ready")
  (delay 16000)
  (print "controller is ready")
  (pinmode irq-pin :input)
  (calibrate-all-sensors)
  (print "calibration done"))
(calibration-process)

(defun calibrate-all-sensors2 ()
  (let ((ring-buffers (repeat 12 (lambda () (mk-ring-buffer 3))))
        (stop-predicate (lambda (ring-buffer-arrays)
                          (forall (lambda (arr)
                                    (and (not (zerop (ahead arr)))
                                         (array-all-equal arr)))
                                  ring-buffer-arrays)))
        (counter 0))
    (print-str-num "calibration-round" counter)
    (loop
      (when (not (digitalread irq-pin))
        (let* ((touch-data (interpret-read (read-touch-status)))
               (activated-sensors (cdr (assoc 'sensor-data touch-data))))
          (when (not (zerop activated-sensors))
            (let ((res (map-indexed (lambda (i ring-buffer)
                                      (let ((res (ring-buffer (read-baseline (+ electrode-1-baseline i)))))
                                        (print res)
                                        res))
                                    ring-buffers)))
              (incf counter)
              (print-str-num "calibration-round" counter)
              (when (or (stop-predicate res) (>= counter 50))
                (print "calibration done")
                (return)))))))))

(defun calibration-process2 ()
  (setup enable-all-touch-sensors)
  (print "setup done, waiting for controller to be ready")
  (delay 16000)
  (print "controller is ready")
  (pinmode irq-pin :input)
  (calibrate-all-sensors2))
(calibration-process2)

; irq is connected to pin 8
; note that irq gets triggered on touch and then it gets reset back when the touch register is read
(defun read-irq-signal ()
  (setup enable-all-touch-sensors)
  (print "setup done, waiting for controller to be ready")
  (delay 8000)
  (print "controller is ready")
  (pinmode irq-pin :input)
  (loop (when (not (digitalread irq-pin))
          (let ((touch-status (read-touch-status))
                (filtered-data (read-filtered-data electrode-1-filtered-data))
                (baseline-data (read-baseline electrode-1-baseline)))
            (print touch-status)
            (print filtered-data)
            (print baseline-data)))))

; TODO (- filtered-data baseline-data)

(defun read-irq-signal ()
  (setup enable-all-touch-sensors)
  (print "setup done, waiting for controller to be ready")
  (delay 15000)
  (print "controller is ready")
  (pinmode irq-pin :input)
  (loop
    (when (not (digitalread irq-pin))
      (let* ((touch-data (interpret-read (read-touch-status)))
             (activated-sensors (cdr (assoc 'sensor-data touch-data))))
        (when (not (zerop activated-sensors))
          (let ((button (match-button activated-sensors)))
            (print button)))))))

(defun read-irq-signal ()
  (setup enable-all-touch-sensors)
  (print "setup done, waiting for controller to be ready")
  (delay 15000)
  (print "controller is ready")
  (pinmode irq-pin :input)
  (loop
    (when (not (digitalread irq-pin))
      (print (interpret-read (read-touch-status))))))

(read-irq-signal)
