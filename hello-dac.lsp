(defvar first-byte
  (let ((control-bits #b0011))
    (logior (ash control-bits 4) #b1111)))
(format t "~8,'0b" (logand (ash first-byte -4) #b1111))
(format t "~8,'0b" #x00)
(< first-byte #b111111111111)
(let ((control-bits #b0011))
  (with-spi (str 16)
            (write-byte (logior (ash control-bits 4) #b0111) str) 
            (write-byte #x00 str)))

(let* ((control-bits #b0011)
       (value 0))
  (loop
    (if (< value #b111111111111)
      (incf value)
      (setf value 0))
    (let ((b1 (logior (ash control-bits 4) (ash value -8)))
          (b2 (logand value #xFF)))
      (with-spi (str 16)
              (write-byte b1 str) 
              (write-byte b2 str)))))


