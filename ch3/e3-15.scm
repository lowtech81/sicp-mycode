; Exercise 3.15.  Draw box-and-pointer diagrams to explain the effect of
; set-to-wow! on the structures z1 and z2 above.
; ------------------------------------------------------------


; z1 -> ( . )
;        | |
;        v v
; x --> ( . ) -> ( . ) -> null
;        |        |
;        v        v
;       'wow     'b

; z2 -> ( . ) -> ( . ) -> ( . ) -> null
;        |        |        |
;        |        v        v
;        |       'a       'b
;        |                 ^
;        |                 |
;        `-----> ( . ) -> ( . ) -> null
;                 |
;                 v
;                'wow