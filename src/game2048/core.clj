(ns game2048.core)

(defn debug [x] (do (println x) x))

(defn update-board
  [board pos value]
  (let [size (count board)]
    (update-in board [(quot pos size) (rem pos size)] (fn [x] value))))

(defn init-board
  [size]
  (let [board (into [] (map (partial into []) (partition size (take (* size size) (repeat 0)))))
        rand-pos [
                  (rand-int (* size size))
                  (rand-int (* size size))]]
    (reduce #(update-board %1 %2 2) board rand-pos)))

(defn draw-board
  [board]
  (doseq [row board]
    (do
      (doseq [ele row]
        (if (zero? ele)
          (print "-" " ")
          (print ele " ")))
      (println "  "))))

(defn print-instructions [] (println "w - up, a - left, s - down, d - right"))


(defn turn-left
  [matrix]
  (apply map vector (map reverse matrix)))

(defn turn-right
  [matrix]
  (let [reverse-matrix (reverse matrix)]
    (apply map vector reverse-matrix)))

(defn remove-zeros-from-start
  [list]
  (drop-while zero? list))

(defn add-zeros-at-start
  [size list]
  (concat (take (- size (count list)) (repeat 0)) list))

(defn isValid [num]
  (not (or (nil? num) (zero? num))))

(defn add-same-consecutive-numbers
  [row]
  (reduce (fn [row num]
            (if (isValid num)
              (if (= (first row) num)
                (cons (+ num num) (rest row))
                (cons num row))
              row))
          [] (reverse row)))

(defn move-right-row
  [row]
  (let [size (count row)]
    (->> row
         (remove-zeros-from-start)
         (add-same-consecutive-numbers)
         (add-zeros-at-start size)
         (into []))))


(defn move-right
  [board]
  (map move-right-row board))

(defn move-left
  [board]
  (-> board
      (turn-right)
      (turn-right)
      (move-right)
      (turn-left)
      (turn-left)))

(defn move-up
  [board]
  (-> board
      (turn-right)
      (move-right)
      (turn-left)))

(defn move-down
  [board]
  (-> board
      (turn-left)
      (move-right)
      (turn-right)))

(defn get-zero-positions
  [board]
  (keep-indexed #(when (zero? %2) %1) (flatten board)))

(defn randomly-insert-2
  [board]
  (let [zero-pos (get-zero-positions board)]
    (update-board (into [] board) (rand-nth zero-pos) 2)))

(defn handle-move
  [board move]
  (if-let [board (case move
                   "w" (move-up board)
                   "d" (move-right board)
                   "a" (move-left board)
                   "s" (move-down board)
                   (println " invalid move"))]
    (randomly-insert-2 board)
    board))

(defn start-game [board]
  (loop [board board]
    (draw-board board)
    (print-instructions)
    (let [move (read-line)]
      (if (= move "z")
        (draw-board board)
        (recur (handle-move board move))))))

(defn -main []
  (let [board (init-board 4)]
    (start-game board)))
