(ns chroma.core)

(def TAU
  "twice PI: the ratio of diameter to circumference"
  (* 2 Math/PI))

(def illuminant
  "constant supplied by Dalrymple. what for? why? not sure..."
  [0.9643 1.0 0.8251])

(defn abs [n]
  "returns the absolute value of a number"
  (cond
    (neg? n) (- n)
    :else n))

(defn angle [c]
  ""
  (-
    (/ TAU 6)
    (* TAU c)))

(defn finv [space]
  ""
  (if (< (/ 6 29) space)
    (* space space space)
    (* 3 (/ 6 29) (/ 6 29) (/ (- space 4) 29))))

(defn L [l]
  ""
  (+ (* l 0.61) 0.09))

(defn L* [l]
  ""
  (/ (+ (L l) 0.16) 1.16))

(defn L->Y [L]
  "transform L of  Lab space to Y of XYZ space"
  (* (first illuminant) (finv (L* L))))

(defn a->X [L a]
  "transform a of Lab space to X of XYZ space.
  requires L* to perform calculation"
  (* (second illuminant) (finv (+ (L* L) (/ a 5)))))

(defn b->Z [L b]
  "transforms b of Lab space to Z of XYZ space.
  requires L* to perform calculation"
  (* (last illuminant) (finv (+ (L* L) (/ b 2)))))

(defn Lab->XYZ [Lab]
  "Complete transformation of Lab defined color to XYZ defined color.
  Composed of the a->X, L->Y and b->Z functions."
  (vector
    (a->X (first Lab) (second Lab))
    (L->Y (first Lab))
    (b->Z (first Lab) (last Lab))))

;; this is a handy function from http://mark.reid.name/sap/minilight-clojure-vectors.html
;;(defn clamp
;;  "Constrains all elements in vector v to be between vmin and vmax"
;;  [vmin vmax v] (map (fn [x] (max vmin (min vmax x))) v))

(defn clamp [xmin xmax x]
  "Constrains value x to be between xmin and xmax."
  (cond
    (< x xmin) xmin
    (> x xmax) xmax
    :else x
    ))

(defn clamp-rgb [rgb]
  "Constrains vector to RGB color space"
  (clamp 0 255 rgb))

(defn clamp-cyl [cyl]
  "Constrains vector to HSL or HSV color space"
  (conj
   (clamp 0 360 (first cyl))
   (clamp 0 1 (rest cyl))))

(defn channel* [xyz]
  "computes a derivative of RGB color from XYZ color."
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
  "Computes RGB color by integrating the channel* function"
  (vec (map #(* 255 %) (channel* xyz))))

(defn Lab->RGB [Lab]
  "Transformation of Lab color to RGB color by composing the XYZ->RGB and Lab->XYZ functions."
  (XYZ->RGB (Lab->XYZ Lab)))

;; karma to Richard Newman for the hex converter
;; https://groups.google.com/d/msg/clojure/JrnYQp84Dig/YqzTYRllJTkJ
(defn HEX->RGB [#^String hex]
  "parses a six charater string as a hex numeral,
  returns an RGB color."
  (Integer/parseInt (.substring hex 2) 16))

(defn hsv-chroma [hsv]
  "The distance from the origin in the hsv color space.
  Used to calculate color transformations and mixtures."
  (* (second hsv) (last hsv)))

(defn H* [hsv]
  "The hue of an HSV color, 'cut' to fix its hexagonal representation.
  Used to calculate color transformations and mixtures."
  (/ (first hsv) 60))

(defn intermediate [chroma cyl]
  "Computes the intermediate value between hsv and rgb."
  (* (chroma cyl)
     (- 1
        (abs (-
               (mod (H* cyl) 2)
               1)))))

;; http://en.wikipedia.org/wiki/HSL_and_HSV#From_HSV
(defn HSV->RGB [hsv]
  "Converts hsv colors to rgb. Recombination of hue and intermediate values is calculated according to the hue.
  Hue determines which 'face' of the HSV hexagon corresponds to RGB balance."
  (let [hsv* (clamp-cyl hsv)]
    (let [h (H* hsv*) x (intermediate hsv-chroma hsv*) c (hsv-chroma hsv*)]
      (cond
        (= h 0) (vector 0 0 0)
        (< h 1) (vector c x 0)
        (< h 2) (vector x c 0) 
        (< h 3) (vector 0 c x)
        (< h 4) (vector 0 x c)
        (< h 5) (vector x 0 c)
        (< h 6) (vector c 0 x)
        :else (vec 0 0 0)
       ))))

(defn hsl-chroma [hsl]
  "Distance of HSL color from the origin."
  (* (second hsl) (- 1 (abs (- (* 2 (last hsl)) 1)))))

(defn HSL->RGB [hsl]
  "Converts hsv colors to rgb."
  (let [h (H* hsl) x (intermediate hsl-chroma hsl) c (hsl-chroma hsl)]
   (cond
    (= h 0) (vector 0 0 0)
    (< h 1) (vector c x 0)
    (< h 2) (vector x c 0) 
    (< h 3) (vector 0 c x)
    (< h 4) (vector 0 x c)
    (< h 5) (vector x 0 c)
    (< h 6) (vector c 0 x)
    :else (vector 0 0 0)
     )))

