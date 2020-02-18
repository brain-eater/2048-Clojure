(ns game2048.core)

(defn debug [m, x] (do (println m " - " x) x))


(defn update-board-values
  [board pos value]
  (let [size (count board)]
    (update-in board [(quot pos size) (rem pos size)] (fn [x] value))))

(defn update-board
  [board pos value]
  (let [size (count board)]
    (update-in board [(quot pos size) (rem pos size)] (fn [x] {:pos (:pos x) :val value}))))


(defn create-cell
  [x, y, value]
  {:pos [x, y] :val value})

(defn duplicate-cell-pos
  [cell]
  (create-cell (first (:pos cell)) (second (:pos cell)) 0))

(defn all-positions
  [board-values]
  (keep-indexed (fn [index, value]
                  (keep-indexed #(create-cell index %1 %2) value)) board-values))

(defn init-values
  [size]
  (let [board (into [] (map (partial into []) (partition size (take (* size size) (repeat 0)))))
        rand-pos [
                  (rand-int (* size size))
                  (rand-int (* size size))]]
    (reduce #(update-board-values %1 %2 2) board rand-pos)))

(defn init-board
  [size]
  (all-positions (init-values size)))

(defn draw-board
  [board]
  (doseq [row board]
    (do
      (doseq [cell row]
        (if (zero? (:val cell))
          (print "-" " ")
          (print (:val cell) " ")))
      (println "  "))))

(defn print-instructions [] (println "w - up, a - left, s - down, d - right"))

(defn turn-left
  [matrix]
  (apply map vector (map reverse matrix)))

(defn turn-right
  [matrix]
  (let [reverse-matrix (reverse matrix)]
    (apply map vector reverse-matrix)))

(defn add
  [cell1, cell2]
  {:pos (:pos cell2) :val (+ (:val cell1) (:val cell2))})

(defn cons-cell
  [cell1, cell2, row]
  (if (or (= (:val cell1) (:val cell2)) (zero? (:val cell2)))
    (cons (add cell1 cell2) (rest row))
    (cons cell1 row)))

(defn add-same-consecutive-numbers
  [row]
  (reduce (fn [row cell]
            (cons-cell cell (first row) row))
          [(first (reverse row))] (rest (reverse row))))

(defn remove-zeros-from-extremes
  [list]
  (->> list
       (drop-while #(zero? (:val %)))
       (reverse)
       (drop-while #(zero? (:val %)))
       (reverse)))

(defn add-zero-at-start
  [row]
  (let [x (first (:pos (first row)))
        y (- (second (:pos (first row))) 1)
        val 0]
    (cons
      (create-cell x y val) row)))

(defn add-zeros-at-start
  [size row]
  (loop [row row]
    (if (= (count row) size)
      row
      (recur (add-zero-at-start row)))))

(defn move-right-row
  [row]
  (let [size (count row)
        row-without-extreme-zeros (remove-zeros-from-extremes row)]
    (if (empty? row-without-extreme-zeros)
      row
      (->> row-without-extreme-zeros
           (add-same-consecutive-numbers)
           (add-zeros-at-start size)
           (into [])))))


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
  (keep-indexed #(when (zero? (:val %2)) %1) (flatten board)))

(defn randomly-insert-2
  [board]
  (let [zero-pos (get-zero-positions board)]
    (update-board (into [] board) (rand-nth zero-pos) 2)))

(defn handle-move
  [move board]
  (if-let [board (case move
                   "w" (move-up board)
                   "d" (move-right board)
                   "a" (move-left board)
                   "s" (move-down board)
                   (println " invalid move -" move))]
    (randomly-insert-2 board)
    board))

(defn start-game [board]
  (loop [board board]
    (draw-board board)
    (print-instructions)
    (let [move (read-line)]
      (if (= move "z")
        (draw-board board)
        (recur (handle-move move board))))))

(defn -main []
  (let [board (init-board 4)]
    (start-game board)))
