; simple example program for 

(defun scan (port)
  (dotimes (p 127)
    (with-i2c (str port p) (when str (print p)))))
; external stuff is on port 0 for micro:bit v2
(scan 0)
; returns 91 for me, which in hexa is x5B, so we use x5B as the address moving forward



(defvar touch-addr #x5B)

(defvar soft-reset #x80)

(defvar soft-reset-val #x63)

(defvar electrode-configuration #x5E)

(defvar touch-status #x00)

(defun set-treshold-values (touch-treshold release-treshold)
  (with-i2c (str 0 touch-addr)
            (write-byte #x41 str)
            (dotimes (x 12)
              (write-byte touch-treshold str)
              (write-byte release-treshold str))))

(defun analog-frontend-and-filter ()
  (with-i2c (str 0 touch-addr)
            (write-byte #x5C str)
            (write-byte #x10 str)
            (write-byte #x24 str)
            (write-byte #x80 str)))

(defun auto-config ()
  (with-i2c (str 0 touch-addr)
            (write-byte #x7B str)
            (write-byte #x0B str)
            (write-byte #x80 str)
            (write-byte #xC8 str)
            (write-byte #x82 str)
            (write-byte #xB4 str)
            ))

(defun setup (config-val)
  (with-i2c (str 0 touch-addr)
            (write-byte soft-reset str)
            (write-byte soft-reset-val str))
  (set-treshold-values 10 8)
  (analog-frontend-and-filter)
  (auto-config)
  (with-i2c (str 0 touch-addr)
            (write-byte electrode-configuration str)
            (write-byte config-val str)))

(defun read ()
  (with-i2c (str 0 touch-addr)
            (write-byte touch-status str)
            (restart-i2c str 2)
            (list
              (read-byte str)
              (read-byte str))))

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

(defun read-capacitive-touch2 ()
  (setup enable-all-touch-sensors)
  (loop (print (interpret-read (read)))))

; irq is connected to pin 8
; note that irq gets triggered on touch and then it gets reset back when the touch register is read
(defun read-irq-signal ()
  (setup enable-all-touch-sensors)
  (pinmode 8 :input)
  (loop (when (not (digitalread 8))
          (print (read)))))
(read-irq-signal)
