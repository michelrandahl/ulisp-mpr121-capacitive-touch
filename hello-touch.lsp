; simple example program for 

(defun scan (port)
  (dotimes (p 127)
    (with-i2c (str port p) (when str (print p)))))
; external stuff is on port 0 for micro:bit v2
(scan 0)
; returns 91 for me, which in hexa is x5B, so we use x5B as the address moving forward

(defun setup ()
  (with-i2c (str 0 #x5B)
            (write-byte #x80 str)
            (write-byte #x63 str))
  (with-i2c (str 0 #x5B)
            (write-byte #x5E str)
            (write-byte #x0C str)))
(defun read ()
  (with-i2c (str 0 #x5B)
            (write-byte #x00 str)
            (restart-i2c str 2)
            (list
              (read-byte str)
              (read-byte str))))
(defun read-capacitive-touch ()
  (setup)
  (loop (print (read))))
(read-capacitive-touch)
