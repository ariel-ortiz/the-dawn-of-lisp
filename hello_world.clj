(defn hello
  "Classical Hello World program."
  ([] (hello "World"))
  ([name] (println (str "Hello " name "!"))))

(hello)
(hello "You")
