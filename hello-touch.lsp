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

; note that irq gets triggered on touch and then it gets reset back when the touch register is read
; irq is connected to pin 8
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

(defvar buttons #('one 'four 'seven '* 'two 'five 'eight 'zero 'three 'six 'nine 'x))

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

(defun match-sensor (input-val)
  (dolist (sensor-lookup-pair sensors)
    (let ((sensor-mask (car sensor-lookup-pair)))
      (when (plusp (logand sensor-mask input-val))
        (return (cdr sensor-lookup-pair))))))

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

(defun read-filtered-data (sensor-number)
  (let* ((electrode-0-filtered-data-reg #x04)
         (reg (+ electrode-0-filtered-data-reg (* sensor-number 2)))
         (bytes (read-bytes reg 2))
         (lsb (first bytes))
         (msb (second bytes)))
    (logior (ash msb 8) lsb)))

(defun read-baseline (sensor-number)
  (let* ((electrode-1-baseline #x1E)
         (reg (+ electrode-1-baseline sensor-number)))
    (first (read-bytes reg 1))))

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
(defun calibrate-baseline-register (sensor-number)
  (let ((ring-buffer (mk-ring-buffer 7)))
    (loop
      (when (not (digitalread irq-pin))
        (let* ((touch-data (interpret-read (read-touch-status)))
               (activated-sensors (cdr (assoc 'sensor-data touch-data))))
          (when (not (zerop activated-sensors))
            (let* ((baseline (read-baseline sensor-number))
                   (xs (ring-buffer baseline)))
              (print xs)
              (when (and (not (zerop (ahead xs)))
                         (array-all-equal xs))
                (return)))))))))

(defun calibrate-all-sensors ()
  (dotimes (i 12)
    (format t "~&calibrating sensor: ~d" i)
    (calibrate-baseline-register i)))

(defun calibration-process ()
  (setup enable-all-touch-sensors)
  (print "setup done, waiting for controller to be ready")
  (delay 16000)
  (print "controller is ready")
  (pinmode irq-pin :input)
  (calibrate-all-sensors))


; This assumes that the dfrobot number pad is connected
(defun read-number-pad ()
  (setup enable-all-touch-sensors)
  (pinmode irq-pin :input)
  (print "waiting for controller to be ready")
  (delay 10000)
  (print "READY")
  (loop
    (when (not (digitalread irq-pin))
      (let* ((touch-data (interpret-read (read-touch-status)))
             (activated-sensors (cdr (assoc 'sensor-data touch-data))))
        (when (plusp activated-sensors)
          (let* ((sensor-number (match-sensor activated-sensors))
                 (button (car (cdr (aref buttons sensor-number)))))
            (print button)))))))
(read-number-pad)

; x-axis bit 0-4
; y-axis bit 5-11

(defvar x-sensors
  '((#b000000000001 . 0)
    (#b000000000010 . 1)
    (#b000000000100 . 2)
    (#b000000001000 . 3)
    (#b000000010000 . 4)))
(defvar y-sensors
  '((#b000000100000 . 5)
    (#b000001000000 . 6)
    (#b000010000000 . 7)
    (#b000100000000 . 8)
    (#b001000000000 . 9)
    (#b010000000000 . 10)
    (#b100000000000 . 11)))

(defvar x-values
  '((#b00001 . 0)
    (#b00011 . 1)
    (#b00010 . 2)
    (#b00110 . 3)
    (#b00100 . 4)
    (#b01100 . 5)
    (#b01000 . 6)
    (#b11000 . 7)
    (#b10000 . 8)))
(defvar y-values
  '((#b0000001 . 0)
    (#b0000011 . 1)
    (#b0000010 . 2)
    (#b0000110 . 3)
    (#b0000100 . 4)
    (#b0001100 . 5)
    (#b0001000 . 6)
    (#b0011000 . 7)
    (#b0010000 . 8)
    (#b0110000 . 9)
    (#b0100000 . 10)
    (#b1100000 . 11)
    (#b1000000 . 12)))

; TODO save last registered value, if current value is nil then use the old value
; todo increase resolution by using the capacitance value as an offset
; .... determine offset by looking at the neighbor sensors and see which one has lowest value (first check the bits)
; .... if both neighbors are equal, then we must be on top
; .... if value is negative, then we must be on top
; .... if sensor is at the edge, then the offset can only go in one direction
(defun interpret-xy-touch (activated-sensor-bits)
  (let* ((x-axis      (logand #b000000011111 activated-sensors))
         (y-axis (ash (logand #b111111100000 activated-sensors) -5))
         (try-match (lambda (sensor-mask-num-pair)
                      (let ((sensor-mask (car sensor-mask-num-pair))
                            (sensor-num (cdr sensor-mask-num-pair)))
                        (when (plusp (logand sensor-mask activated-sensor-bits))
                          (list sensor-num)))))
         (x-sensors (mapcan try-match x-sensors))
         (y-sensors (mapcan try-match y-sensors)))
    (list (cdr (assoc x-axis x-values))
          (cdr (assoc y-axis y-values)))))

(defun read-touch-pad ()
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
             (x-axis (logand #b000000011111 activated-sensors))
             (y-axis (ash (logand #b111111100000 activated-sensors) -5)))
        (when (plusp activated-sensors)
          (let* ((sensor-number (match-sensor activated-sensors))
                 (xs (mapcar (lambda (i)
                               (let ((baseline (read-baseline i))
                                     (filtered (read-filtered-data i)))
                                 (- filtered baseline)))
                             (range 0 12)))
                 (xy-touch (interpret-xy-touch activated-sensors)))
            (format t "~&~12,'0b" activated-sensors)
            (format t "~&x-axis: ~5,'0b" x-axis)
            (format t "~&y-axis: ~7,'0b" y-axis)
            (print (reverse xs))
            (print xy-touch)))))))

(read-touch-pad)

