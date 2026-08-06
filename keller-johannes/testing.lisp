(in-package :explorateur.testing)

(def-suite explorateur-tests
  :description "Testing the entire EXPLORATEUR system.")

(def-suite midi-monitor
  :description "Testing the internal functions belonging to the MIDI monitor feature."
  :in explorateur-tests)

(in-suite midi-monitor)

(test increasing-counter
  (let ((current-cursor midi::*midi-monitor-cursor*)
        (test-cursor 0))
    (is (= (1+ current-cursor) (midi::inc-midi-monitor-cursor midi::*midi-monitor-cursor*)))
    (is (= (1+ current-cursor) midi::*midi-monitor-cursor*))
    (is (= 1 (midi::inc-midi-monitor-cursor test-cursor)))
    (is (= 1 test-cursor))
    (setf test-cursor 0)
    (is (= (1- midi::*midi-monitor-cursor*) (midi::dec-midi-monitor-cursor midi::*midi-monitor-cursor*)))
    (is (= (1- midi::*midi-monitor-number-of-lines*) (midi::dec-midi-monitor-cursor test-cursor)))
    (is (= (1- midi::*midi-monitor-number-of-lines*) test-cursor))
    ))

(test reset-counter
  (setf midi::*midi-monitor-cursor* (1- midi::*midi-monitor-number-of-lines*))
  (is (= 0 (midi::inc-midi-monitor-cursor midi::*midi-monitor-cursor*)))
  (is (= 0 midi::*midi-monitor-cursor*)))
