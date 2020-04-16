(ns conversions
  (:require [clojure.java.io :as io])
  (:import (java.io ByteArrayOutputStream)
           (java.util.zip InflaterInputStream)))

;; Convert a hex string to a byte sequence
(defn hex-digits->byte
  [[dig1 dig2]]
  ;; This is tricky because something like "ab" is "out of range" for a
  ;; Byte, because Bytes are signed and can only be between -128 and 127
  ;; (inclusive). So we have to temporarily use an int to give us the room
  ;; we need, then adjust the value if needed to get it in the range for a
  ;; byte, and finally cast to a byte.
  (let [i (Integer/parseInt (str dig1 dig2) 16)
        byte-ready-int (if (< Byte/MAX_VALUE i)
                         (byte (- i 256))
                         i)]
    (byte byte-ready-int)))

(defn from-hex-string
  [hex-str]
  (byte-array (map hex-digits->byte (partition 2 hex-str))))

;; Read a seq of unzipped bytes from a zipped file on disk
(defn unzip
  "Unzip the given file's contents with zlib."
  [path]
  (with-open [input (-> path io/file io/input-stream)
              unzipper (InflaterInputStream. input)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

;; Note that if given binary data this will fail with an error message
;; like:
;; Execution error (IllegalArgumentException) at ,,,.
;; Value out of range for char: -48
(defn bytes->str [bytes]
  (->> bytes (map char) (apply str)))

(defn split-at-byte [b bytes]
  (let [part1 (take-while (partial not= b) bytes)
        part2 (nthrest bytes (-> part1 count inc))]
    [part1 part2]))
