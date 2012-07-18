(ns chroma.core)

(def TAU (* 2 Math/PI))

(def illuminant [0.9643 1.0 0.8251])

(defn angle [c] (-
              (/ TAU 6)
              (* TAU c)))

(defn finv [space]
  (if (< (/ 6 29) space)
    (* space space space)
    (* 3 (/ 6 29) (/ 6 29) (/ (- space 4) 29))))

(defn L [l] (+ (* l 0.61) 0.09))

(defn L* [l] (/ (+ (L l) 0.16) 1.16))

(defn L->Y [L]
  (* (first illuminant) (finv (L* L))))

(defn a->X [L a]
  (* (second illuminant) (finv (+ (L* L) (/ a 5)))))

(defn b->Z [L b]
  (* (last illuminant) (finv (+ (L* L) (/ b 2)))))

(defn Lab->XYZ [Lab]
  (vector
    (a->X (first Lab) (second Lab))
    (L->Y (first Lab))
    (b->Z (first Lab) (last Lab))))

(defn clip-rgb-channel [channel]
  (if (neg? channel)
  0
  (if (> channel 1)
    1
    channel)))

(defn channel* [xyz]
  (vector
    (clip-rgb-channel
      (+
        (* 3.2406 (first xyz))
        (* -1.5372 (second xyz))
        (* -0.4986 (last xyz))))
    (clip-rgb-channel
      (+
        (* -0.9689 (first xyz))
        (* 1.8758 (second xyz))
        (* 0.0415 (last xyz))))
    (clip-rgb-channel
      (+
        (* 0.0557 (first xyz))
        (* 0.2040 (second xyz))
        (* 1.0570 (last xyz))))))

(defn XYZ->RGB [xyz]
  (vec (map #(* 255 %) (channel* xyz))))

(defn Lab->RGB [Lab]
  (XYZ->RGB (Lab->XYZ Lab)))
