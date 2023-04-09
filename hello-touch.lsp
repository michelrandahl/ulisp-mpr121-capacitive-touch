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

(defun repeat-fun (n f)
  (mapcar (lambda (i) (f)) (range 0 n)))

(defun repeat-val (n v)
  (mapcar (lambda (i) v) (range 0 n)))

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

(defun write-bytes (register-addr xs)
  (with-i2c (str 0 mpr121-addr)
            (write-byte register-addr str)
            (dolist (x xs)
              (write-byte x str))))

(defun read-bytes (register-addr n)
  (with-i2c (str 0 mpr121-addr)
            (write-byte register-addr str)
            (restart-i2c str n)
            (repeat-fun n (lambda () (read-byte str)))))

(defun set-treshold-values (touch-treshold release-treshold)
  (let ((number-of-electrodes 12)
        (electrode-0-touch-treshold-reg #x41))
    (write-bytes electrode-0-touch-treshold-reg
                 (mapcan list
                         (repeat-val number-of-electrodes touch-treshold)
                         (repeat-val number-of-electrodes release-treshold)))))

(defun analog-frontend-and-filter ()
  (let ((afe-configuration-1-reg #x5C))
    (write-bytes afe-configuration-1-reg '(#x10 #x24 #x80))))

(defun auto-config ()
  (let ((auto-config-control-reg #x7B))
    (write-bytes auto-config-control-reg '(#x0B #x80 #xC8 #x82 #xB4))))

(defun setup (config-val)
  (let ((soft-reset #x80)
        (soft-reset-val #x63)
        (electrode-configuration #x5E))
    (write-bytes soft-reset (list soft-reset-val))
    (set-treshold-values 10 8)
    (analog-frontend-and-filter)
    (auto-config)
    (write-bytes electrode-configuration (list config-val))))

(defun read-touch-status ()
  (let ((touch-status-reg #x00))
    (read-bytes touch-status-reg 2)))

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

(defvar sensors
  '((#b000000000001 . 0)
    (#b000000000010 . 1)
    (#b000000000100 . 2)
    (#b000000001000 . 3)
    (#b000000010000 . 4)
    (#b000000100000 . 5)
    (#b000001000000 . 6)
    (#b000010000000 . 7)
    (#b000100000000 . 8)
    (#b001000000000 . 9)
    (#b010000000000 . 10)
    (#b100000000000 . 11)))

(defun match-button (input-val)
  (let ((result nil))
    (dolist (button buttons)
      (let ((button-val (car button)))
        (when (not (zerop (logand button-val input-val)))
          (setf result (cdr button))
          (return))))
    result))

(defun interpret-read (read-res)
  (let* ((lsb (first read-res))
         (msb (second read-res))
         (combined (logior (ash msb 8) lsb))
         (sensor-data (logand combined #b0000111111111111))
         (over-current-flag (logand msb #b10000000)))
    (list (cons 'sensor-data sensor-data)
          (cons 'over-current-flag over-current-flag))))

(defvar enable-proximity (ash #b0011 4))

(defvar enable-all-touch-sensors #b1100)

(defvar electrode-1-filtered-data #x04)

(defvar electrode-1-baseline #x1E)

(defun read-filtered-data (electrode-filtered-data-reg)
  (let* ((electrode-0-filtered-data-reg #x04)
         (bytes (read-bytes electrode-filtered-data-reg 2))
         (lsb (first bytes))
         (msb (second bytes)))
    (logior (ash msb 8) lsb)))

(defun read-baseline (electrode-baseline-reg)
  (car (read-bytes electrode-baseline-reg 1)))

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
  (calibrate-all-sensors))

(calibration-process)

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
  (pinmode irq-pin :input)
  (print "waiting for controller to be ready")
  (delay 10000)
  (print "calibration process, please touch until done")
  (calibrate-all-sensors)
  (print "READY")
  (loop
    (when (not (digitalread irq-pin))
      (let* ((touch-data (interpret-read (read-touch-status)))
             (activated-sensors (cdr (assoc 'sensor-data touch-data)))
             (filtered-data (read-filtered-data )))
        (when (not (zerop activated-sensors))
          (let ((button (match-button activated-sensors)))
            (print button)))))))

(defun read-irq-signal ()
  (setup enable-all-touch-sensors)
  (pinmode irq-pin :input)
  (print "waiting for controller to be ready")
  (delay 10000)
  (print "calibration process, please touch until done")
  (calibrate-all-sensors)
  (loop
    (when (not (digitalread irq-pin))
      (print (interpret-read (read-touch-status))))))

(read-irq-signal)
