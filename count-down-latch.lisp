(in-package #:com.varoun.concurrency)

(defclass count-down-latch ()
  ((initial-count :initarg :initial-count
                  :reader latch-initial-count)
   (current-count :initarg :current-count
                  :accessor %%latch-current-count)
   (lock :initform (make-lock :name "count-down-latch lock")
         :accessor latch-lock)
   (condition :initform (make-condition-variable :name "count-down-latch condition")
              :accessor latch-condition)))

(defun make-count-down-latch (&key count)
  "Creates a count down latch with initial count COUNT.
   Threads can call `latch-wait' to wait until the latch's count reaches zero.
   The latch count is decremented by calling `latch-count-down'"
  (make-instance 'count-down-latch
    :initial-count count
    :current-count count))

(defmethod latch-wait ((latch count-down-latch))
  "Puts the thread into a waiting mode until the latch reaches zero count."
  (with-lock-held ((latch-lock latch))
    (loop until (zerop (%%latch-current-count latch))
          doing
       (condition-wait (latch-condition latch) (latch-lock latch)))))

(defmethod latch-count-down (latch)
  "Decrements latch's count, or leaves it unchanged if it is already zero.
   When the count reaches zero, all waiting threads are released."
  (with-lock-held ((latch-lock latch))
    (when (plusp (%%latch-current-count latch))
      (decf (%%latch-current-count latch))
      (when (zerop (%%latch-current-count latch))
        (condition-notify-all (latch-condition latch))))))

(defmethod latch-current-count (latch)
  (with-lock-held ((latch-lock latch))
    (%%latch-current-count latch)))
