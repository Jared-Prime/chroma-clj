(ns chroma.core)

(def TAU (* 2 Math/PI))

(def illuminant [0.9643 1.0 0.8251])

(defn abs [n]
  (cond
    (neg? n) (- n)
    :else n))

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

(defn clamp
  "Constrains all elements in v to be between vmin and vmax"
  [vmin vmax v] (map (fn [x] (max vmin (min vmax x))) v))

(defn clamp-rgb [rgb]
  "Constrains vector to RGB color space"
  (clamp 0 255 rgb))

(defn clamp-cyl [cyl]
  "Constrains vector to HSL or HSV color space"
  (conj
   (clamp 0 360 (first cyl))
   (clamp 0 1 (rest cyl))))

(defn channel* [xyz]
  (vector
    (clamp-rgb
      (+
        (* 3.2406 (first xyz))
        (* -1.5372 (second xyz))
        (* -0.4986 (last xyz))))
    (clamp-rgb
      (+
        (* -0.9689 (first xyz))
        (* 1.8758 (second xyz))
        (* 0.0415 (last xyz))))
    (clamp-rgb
      (+
        (* 0.0557 (first xyz))
        (* 0.2040 (second xyz))
        (* 1.0570 (last xyz))))))

(defn XYZ->RGB [xyz]
  (vec (map #(* 255 %) (channel* xyz))))

(defn Lab->RGB [Lab]
  (XYZ->RGB (Lab->XYZ Lab)))

;; karma to Richard Newman for the hex converter
;; https://groups.google.com/d/msg/clojure/JrnYQp84Dig/YqzTYRllJTkJ
(defn HEX->RGB [#^String hex]
  (Integer/parseInt (.substring hex 2) 16))

(defn chroma [hsv]
  (* (second hsv) (last hsv)))

(defn H* [hsv]
  (/ (first hsv) 60))

(defn intermediate [hsv]
  (* (chroma hsv)
     (- 1
        (abs (-
               (mod (H* hsv) 2)
               1)))))

(defn HSV->RGB [hsv] )
