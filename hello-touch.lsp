; simple example program for 

(defun scan (port)
  (dotimes (p 127)
    (with-i2c (str port p) (when str (print p)))))
; external stuff is on port 0 for micro:bit v2
(scan 0)
; returns 91 for me, which in hexa is x5B, so we use x5B as the address moving forward


(defvar irq-pin 8)

(defvar mpr121-addr #x5B)

(defun mpr121-i2c (stream-fn)
  (with-i2c (str 0 mpr121-addr)
    (stream-fn str)))

(defvar soft-reset #x80)

(defvar soft-reset-val #x63)

(defvar electrode-configuration #x5E)

(defvar touch-status-reg #x00)

(defun set-treshold-values (touch-treshold release-treshold)
  (mpr121-i2c
    (lambda (str)
      (write-byte #x41 str)
      (dotimes (x 12)
        (write-byte touch-treshold str)
        (write-byte release-treshold str)))))

(defun analog-frontend-and-filter ()
  (mpr121-i2c
    (lambda (str)
      (write-byte #x5C str)
      (write-byte #x10 str)
      (write-byte #x24 str)
      (write-byte #x80 str))))

(defun auto-config ()
  (mpr121-i2c
    (lambda (str)
      (write-byte #x7B str)
      (write-byte #x0B str)
      (write-byte #x80 str)
      (write-byte #xC8 str)
      (write-byte #x82 str)
      (write-byte #xB4 str))))

(defun setup (config-val)
  (mpr121-i2c
    (lambda (str)
      (write-byte soft-reset str)
      (write-byte soft-reset-val str)))
  (set-treshold-values 10 8)
  (analog-frontend-and-filter)
  (auto-config)
  (mpr121-i2c
    (lambda (str)
      (write-byte electrode-configuration str)
      (write-byte config-val str))))

(defun read-touch-status ()
  (mpr121-i2c
    (lambda (str)
      (write-byte touch-status-reg str)
      (restart-i2c str 2)
      (list
        (read-byte str)
        (read-byte str)))))

(defun interpret-read (read-res)
  (let ((lsb (car read-res))
        (msb (car (cdr read-res))))
    (logand
      (logior
        (ash msb 8)
        lsb)
      #b0000111111111111)))

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
  (mpr121-i2c
    (lambda (str)
      (write-byte electrode-filtered-data-reg str)
      (restart-i2c str 2)
      (let ((lsb (read-byte str))
            (msb (read-byte str)))
        (logior (ash msb 8) lsb)))))

(defun read-baseline (electrode-baseline-reg)
  (mpr121-i2c
    (lambda (str)
      (write-byte electrode-baseline-reg str)
      (restart-i2c str 1)
      (read-byte str))))

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

;; TODO we want to calibrate all the baseline registers... perhaps its enough to look at the data for just one of them?
; This function simply continues running until the last five baseline values has stabalized to the same value.
; The MPR121 controller will keep adjusting the baseline registers, but will eventually stabalize
(defun calibrate-baseline ()
  (setup enable-all-touch-sensors)
  (print "setup done, waiting for controller to be ready")
  (delay 8000)
  (print "controller is ready")
  (pinmode irq-pin :input)
  (let ((ring-buffer (mk-ring-buffer 5)))
    (loop
      (when (not (digitalread irq-pin))
        (let* ((baseline (read-baseline electrode-1-baseline))
               (xs (ring-buffer baseline)))
          (print xs)
          (when (array-all-equal xs)
            (print "calibration finished")
            (return)))))))
(calibrate-baseline)

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

(defun read-irq-signal ()
  (setup enable-all-touch-sensors)
  (print "setup done, waiting for controller to be ready")
  (delay 8000)
  (print "controller is ready")
  (pinmode irq-pin :input)
  (loop (when (not (digitalread irq-pin))
          (print (read-touch-status))
          (print (read-filtered-data electrode-1-filtered-data)))))

(read-irq-signal)
