; DURATION.TIME = {
;     'c1': 1,
;     'c2': 2,
;     'c3': 3,
;     'c4': 4,
;     'c5': 4,
;  }

; DURATION.COUNTER = {
;     'c1': 1,
;     'c2': 2,
;     'c3': 3,
;     'c4': 4,
;     'c5': 4,
; }

; def ResetState():
;     state.value = {'a': 0}

; tasks = {
;     1: ['t4'],
; }

(begin
    (def-facts (a 0))
    (add-task-to-execute t4)
)