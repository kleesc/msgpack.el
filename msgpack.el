;;; msgpack.el --- MessagePack implementation in emacs lisp
;;
;;; Commentary:
;;
;; Author: Kenny Lee Sin Cheong
;; 
;; 64 bit int and float not supported because of Emacs uses flag bits.
;; For the same reason, 32 bit objects are not supported on 32 bit machines.
;; Same for floats.
;;
;; ----------------------------------------------
;;                  | first byte  | first byte
;;     format name  | (in binary) | (in hex)
;; -----------------+-------------+--------------
;; positive fixint  |  0xxxxxxx   | 0x00 - 0x7f
;; negative fixint  |  111xxxxx   |	0xe0 - 0xff
;; ----------------------------------------------
;; fixmap           |  1000xxxx   | 0x80 - 0x8f
;; fixarray         |  1001xxxx   | 0x90 - 0x9f
;; fixstr           |  101xxxxx   | 0xa0 - 0xbf
;; negative fixint  |  111xxxxx   | 0xe0 - 0xff
;; ----------------------------------------------
;; nil              |  11000000   | 0xc0
;; (never used)     |  11000001   | 0xc1
;; false            |  11000010   | 0xc2
;; true 	        |  11000011   | 0xc3
;; ----------------------------------------------
;; bin 8 	        |  11000100   | 0xc4
;; bin 16 	        |  11000101   | 0xc5
;; bin 32 	        |  11000110   | 0xc6
;; ext 8 	        |  11000111   | 0xc7
;; ext 16 	        |  11001000   | 0xc8
;; ext 32 	        |  11001001   | 0xc9
;; ----------------------------------------------
;; float 32         |  11001010   | 0xca
;; float 64         |  11001011   | 0xcb
;; ----------------------------------------------
;; uint 8 	        |  11001100   | 0xcc
;; uint 16 	        |  11001101   | 0xcd
;; uint 32 	        |  11001110   | 0xce
;; uint 64 	        |  11001111   | 0xcf
;; int 8 	        |  11010000   | 0xd0
;; int 16 	        |  11010001   | 0xd1
;; int 32 	        |  11010010   | 0xd2
;; int 64 	        |  11010011   | 0xd3
;; ----------------------------------------------
;; fixext 1 	    |  11010100   | 0xd4
;; fixext 2 	    |  11010101   | 0xd5
;; fixext 4         |  11010110   | 0xd6
;; fixext 8         |  11010111   | 0xd7
;; fixext 16        |  11011000   | 0xd8
;; ----------------------------------------------
;; str 8 	        |  11011001   | 0xd9
;; str 16 	        |  11011010   | 0xda
;; str 32 	        |  11011011   | 0xdb
;; ----------------------------------------------
;; array 16 	    |  11011100   | 0xdc
;; array 32 	    |  11011101   | 0xdd
;; map 16 	        |  11011110   | 0xde
;; map 32 	        |  11011111   | 0xdf
;; ----------------------------------------------
;;
;;; Code:

(require 'bindat)

(setq nil-spec '((nil-type byte)))
(setq bool-spec '((bool-type byte)))
(setq fixnum-spec '((fixnum-type byte)))
(setq uint8-spec '((header-type byte)
                   (uint8-type u8)))
(setq uint16-spec '((header-type byte)
                    (uint16-type u16)))
(setq uint32-spec '((header-type byte)
                    (uint32-type u32)))
(setq int8-spec '((header-type byte)
                  (int8-type u8)))
(setq int16-spec '((header-type byte)
                   (int16-type u16)))
(setq int32-spec '((header-type byte)
                   (int32-type u32)))

;; Need to define is closure in respective functions to get length !!! (with let)
(setq fixstr-spec '((header-type byte)
                    (fixstr-type str 31))) ;;
(setq str8-spec '((header-type byte)
                  (str-len u8)
                  (str8-type str (- (expt 2 8) 1)))) ;;
(setq str16-spec '((header-type byte)
                   (str-len u16)
                   (str16-type str (- (expt 2 16) 1)))) ;;
(setq str32-spec '((header-type byte)
                   (str-len u32)
                   (str32-type str (- (expt 2 32) 1)))) ;;
(setq bin8-spec '((header-type byte)
                  (bin-len u8)
                  (bin8-type vec (- (expt 2 8) 1) [byte]))) ;;
(setq bin16-spec '((header-type byte)
                   (bin-len u16)
                   (bin16-type vec (- (expt 2 16) 1) [byte]))) ;;
(setq bin32-spec '((header-type byte)
                   (bin-len u32)
                   (bin32-type vec (- (expt 2 32) 1) [byte]))) ;;
(setq fixarray-spec '((header-type byte)
                      (fixarray-type vec (- (expt 2 8) 1) [byte]))) ;; redefine [type], length
(setq array16-spec '((header-type byte)
                     (array-len u16)
                     (array16-type vec (- (expt 2 16) 1) [byte]))) ;; redefine [type], length
(setq array32-spec '((header-type byte)
                     (array-len u32)
                     (array32-type vec (- (expt 2 32) 1) [byte]))) ;; redefine [type], length
(setq fixmap-spec '((header-type byte)
                    (fixmap-type vec (- (expt 2 8) 1) [byte]))) ;; redefine [type], length
(setq map16-spec '((header-type byte)
                   (map-len u16)
                   (map16-type vec (- (expt 2 16) 1) [byte]))) ;; redefine [type], length
(setq map32-spec '((header-type byte)
                   (map-len u32)
                   (map32-type vec (- (expt 2 32) 1) [byte]))) ;; redefine [type], length


(defun pack-nil (obj)
  "Pack OBJ as nil type."
  (bindat-pack nil-spec '((nil-type . #xc0))))

(defun pack-bool (obj)
  "Pack OBJ as true if OBJ is not nil, false otherwise."
  (if (and (boundp obj) (fboundp obj))
      (bindat-pack bool-spec '((bool-type . #xc2)))
      (bindat-pack bool-spec '((bool-type . #xc3)))))

(defun pack-int (obj)
  "Pack OBJ as int type."
  (if (>= obj 0)
      ;; Positive ints
      (cond ((<= obj 127) ;; Positive fixnum
             (bindat-pack fixnum-spec '((fixnum-type . obj))))
            ((<= obj (- (expt 2 8) 1)) ;; uint8
             (bindat-pack uint8-spec '((header-type . #xcc)
                                       (uint8-type . obj))))
            ((<= obj (- (expt 2 16) 1)) ;; uint16
             (bindat-pack uint16-spec '((header-type . #xcd)
                                        (uint16-type . obj))))
            ((<= obj (- (expt 2 32) 1)) ;; uint32
             (bindat-pack uint32-spec '((header-type . #xce)
                                        (uint32-type . obj))))
            (t (error "Unsupported type")))
      
      ;; Negative ints
      (cond ((>= -32) ;; Negative fixnum
             (bindat-pack fixnum-spec '((fixnum-type . obj))))
            ((>= obj (expt (2 (- 8 1)))) ;; int8 (signed, so a two's complement number on 8 bit)
             (bindat-pack int8-spec '((header-type . #xd0)
                                      (int8-type . obj))))
            ((>= obj (expt (2 (- 16 1)))) ;; int16 (signed, so a two's complement number on 16 bit)
             (bindat-pack int16-spec '((header-type . #xd1)
                                       (int16-type . obj))))
            ((>= obj (expt (2 (- 32 1)))) ;; int32 (signed, so a two's complement number on 32 bit)
             (bindat-pack int32-spec '((header-type . #xd2)
                                       (int32-type . obj))))
            (t (error "Unsupported type")))))

(defun pack-str (obj)
  "Pack OBJ as string type."
  (let ((len (length obj)))
    (cond ((<= (length obj) 31) ;; fixstr
           (bindat-pack fixstr-spec `((header-type . ,(logior #xa0 (length obj))) ;; 10100000 | (length str)
                                      (fixstr-type . obj))))
          ((<= (length obj) (- (expt (2 8) 1 ))) ;; str8
           (bindat-pack str8-spec '((header-type . #xd9)
                                    (str-len . len)
                                    (str8-type . obj))))
          ((<= (length obj) (- (expt (2 16) 1 ))) ;; str16
           (bindat-pack str16-spec '((header-type . #xda)
                                     (str-len . len)
                                     (str16-type . obj))))
          ((<= (length obj) (- (expt (2 32) 1 )))
           (bindat-pack str32-spec '((header-type . #xdb)
                                     (str-len . len)
                                     (str32-type . obj))))
          (t (error "Unsupported type")))))

(defun pack-bin (obj) ;; []
  "Pack OBJ as binary type."
  (let ((len (length obj)))
    (cond ((<= (length obj) (- (expt 2 8) 1))
           (bindat-pack bin8-spec '((header-type . #xc4)
                                    (bin-len . len)
                                    (bin8-type . obj))))
          ((<= (length obj) (- (expt 2 8) 1))
           (bindat-pack bin16-spec '((header-type . #xc5)
                                     (bin-len . len)
                                     (bin16-type . obj))))
          ((<= (length obj) (- (expt 2 8) 1))
           (bindat-pack bin32-spec '((header-type . #xc6)
                                     (bin-len . len)
                                     (bin32-type . obj))))
          (t (error "Unsopported type")))))

(defun pack-array (obj) ;; Vector []
  "Pack OBJ as binary type."
  (let ((len (length obj)))
    (cond ((<= (length obj) (- (expt 2 4) 1))
           (bindat-pack fixarray-spec `((header-type . ,(logior #x90 (length obj)))
                                        (fixarray-type . obj))))
          ((<= (length obj) (- (expt 2 16) 1))
           (bindat-pack array16-spec '((header-type . #xdc)
                                       (array-len . len)
                                       (array16-type . obj))))
          ((<= (length obj) (- (expt 2 32) 1))
           (bindat-pack array32-spec '((header-type . #xdd)
                                       (array-len . len)
                                       (array32-type . obj))))
          (t (error "Unsupported type")))))

(defun pack-map (obj) ;; [header][length][key value key value ...]
  "Pack OBJ as binary type."
  (let ((len (/ (length obj) 2)))
    (cond ((<= (length obj) (- (expt 2 4) 1))
           (bindat-pack fixmap-spec `((header-type . ,(logior #x80 (length obj)))
                                      (fixmap-type . obj))))
          ((<= (length obj) (- (expt 2 16) 1))
           (bindat-pack map16-spec '((header-type . #xdc)
                                     (map-len . len)
                                     (map16-type . obj))))
          ((<= (length obj) (- (expt 2 32) 1))
           (bindat-pack map32-spec '((header-type . #xdd)
                                     (map-len . len)
                                     (map32-type . obj))))
          (t (error "Unsupported type")))))



;;; msgpack.el ends here
