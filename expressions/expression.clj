(ns expression)
;Functions:
(defn operation [f]
  (fn [& operands]
    (fn [args] (apply f ((apply juxt operands) args)))
    )
  )

(def add (operation +))
(def subtract (operation -))
(def multiply (operation *))
(defn divide [a b] (fn [args] (/ (double (a args)) (double (b args)))))
(def negate subtract)

(def sin (operation (fn [x] (Math/sin x))))
(def cos (operation (fn [x] (Math/cos x))))

(def sqrt (operation (fn [x] (Math/sqrt (Math/abs x)))))
(def square (operation (fn [x] (* x x ))))

(defn constant [x] (fn [args] x))
(defn variable [name] (fn [args] (get args name)))


(def opers {'+ add '- subtract '* multiply '/ divide
            'negate negate
            'sqrt sqrt 'square square
            'sin sin 'cos cos})

(defn parseRecursiveFunction [getOperation, getOperands, curNode]
  (cond
    (number? curNode) (constant curNode)
    (symbol? curNode) (variable (str curNode))
    (list? curNode) (apply (getOperation curNode) (getOperands curNode getOperation getOperands))
    )
  )

(defn parseFunction [str]  ( parseRecursiveFunction (fn [list] (get opers (first list)))
                                        (fn [list, op, args] (mapv (partial parseRecursiveFunction op args) (rest list)))
                                        (read-string str)))

;Objects:

(defn get-prototype [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (get-prototype (obj :prototype) key)))


(defn method [key]
  (fn [this & args] (apply (get-prototype this key) this args)))

(def evaluate (method :evaluate))
(def toString (method :toString))


(defn Constant [x]
  {
   :evaluate (constantly x)
   :toString (constantly (format "%.1f" (double x)))
   })

(defn Variable [x]
  {
   :evaluate (fn [a vars] (get vars x))
   :toString (constantly (str x))
   })



(defn field [key]
  (fn [name] (get-prototype name key)))

(def operands (field :operands))
(def action (field :action))
(def sign (field :sign))

(def Abstract-Operation
  {
   :evaluate (fn [this vars] (apply (action this) (mapv (fn [operand] (evaluate operand vars)) (operands this))))
   :toString (fn [this] (str "(" (sign this) (apply str (mapv  (fn [x] (str " " (toString x)))  (operands this))) ")"))
   })

(defn Op [sign action ]
  (let [base {
              :prototype Abstract-Operation
              :sign sign
              :action action
              }]
    (fn [& args]
      {
       :prototype base
       :operands (vec args)
       })))



(def Add (Op "+" +))

(def Subtract (Op "-" -))

(def Multiply (Op "*" *))

(def Divide (Op "/" (fn [x y] (/ (double x) (double y)))))

(def Negate (Op "negate" -))




(def Square (Op "square" (fn [x] ( * x x))
               ))

(def Sqrt (Op "sqrt" (fn [x] (Math/sqrt(Math/abs(double x))))))

(def operations
  {'+ Add
   '- Subtract
   '* Multiply
   '/ Divide
   'negate Negate
   'square Square
   'sqrt Sqrt
   })

(defn parseRecursiveObject [expression]
  (cond
    (seq? expression) (apply (get operations (first expression)) (mapv parseRecursiveObject (rest expression)))
    (number? expression) (Constant expression)
    (symbol? expression) (Variable (str expression))))

(def expr
  (Subtract
    (Multiply
      (Constant 2)
      (Variable "x"))
    (Constant 3)))

(println (toString expr))

(println  (evaluate expr {"x" 2}))
