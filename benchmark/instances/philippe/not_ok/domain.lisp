(begin
	(def-types location truck plane package)
	(def-state-function truck-at '(?a truck) '(?b location) '(r bool))
	(def-state-function plane-at '(?a plane) '(?b location) '(r bool))
	(def-state-function package-at '(?a package) '(?b location) '(r bool))
	(def-state-function package-in-truck '(?a package) '(?b truck) '(r bool))
	(def-state-function package-in-plane '(?a package) '(?b plane) '(r bool))
	(def-state-function goal-reward-received  '(r bool))
	(def-state-function road '(?a location) '(?b location) '(r bool))
	(def-state-function is-airport '(?a location) '(r bool))
	(def-action unload-plane '(?a package) '(?b plane) '(?c location))
	(def-task tdo_deliver '(?a package) '(?b location))
	(def-action move-truck '(?a truck) '(?b location) '(?c location))
	(def-action load-truck '(?a package) '(?b truck) '(?c location))
	(def-action load-plane '(?a package) '(?b plane) '(?c location))
	(def-task deliver '(?a package) '(?b location))
	(def-task tdo_deliver_PART128_ST1 )
	(def-task deliver_PART128_ST1 )
	(def-action move-plane '(?a plane) '(?b location) '(?c location))
	(def-action unload-truck '(?a package) '(?b truck) '(?c location))
	(def-method m_2_deliver_PART128_ST1
	'((task tdo_deliver_PART128_ST1)
	(:params (?a location) (?b plane) (?c package))
	(:pre-conditions (and-cond (package-in-plane ?c ?b) (is-airport ?a)))
	(:score 0)
	(:body (do (unload-plane ?c ?b ?a)))))
	
	
	(def-method mdo_deliver
	'((task deliver)
	(:params (?a package) (?b location))
	(:pre-conditions true)
	(:score 0)
	(:body (do (tdo_deliver ?a ?b) (deliver ?a ?b)))))
	(def-method m_0_deliver
	'((task tdo_deliver)
	(:params (?a package) (?b location) (?c location) (?d location) (?e plane))
	(:pre-conditions (and-cond (is-airport ?d) (is-airport ?c) (plane-at ?e ?d)))
	(:score 0)
	(:body (do (move-plane ?e ?d ?c)))))
	(def-method m_3_deliver
	'((task tdo_deliver)
	(:params (?a package) (?b location) (?c truck))
	(:pre-conditions (and-cond (truck-at ?c ?b) (package-in-truck ?a ?c)))
	(:score 0)
	(:body (do (unload-truck ?a ?c ?b)))))
	(def-method m_1_deliver_PART128_ST1
	'((task tdo_deliver_PART128_ST1)
	(:params (?a location) (?b truck) (?c package))
	(:pre-conditions true)
	(:score 0)
	(:body (do (load-truck ?c ?b ?a)))))
	(def-method mcheck_deliver
	'((task deliver)
	(:params (?a package) (?b location))
	(:pre-conditions (check (package-at ?a ?b)))
	(:score 1)
	(:body )))
	(def-method m_1_deliver
	'((task tdo_deliver)
	(:params (?a package) (?b location))
	(:pre-conditions true)
	(:score 0)
	(:body (do (deliver_PART128_ST1 )))))
	(def-method m_2_deliver
	'((task tdo_deliver)
	(:params (?a package) (?b location) (?c location) (?d plane) (?e package))
	(:pre-conditions (check (is-airport ?c)))
	(:score 0)
	(:body (do (load-plane ?e ?d ?c)))))
	(def-method mdo_deliver_PART128_ST1
	'((task deliver_PART128_ST1)
	(:params )
	(:pre-conditions true)
	(:score 0)
	(:body (do (tdo_deliver_PART128_ST1 ) (deliver_PART128_ST1 )))))
	(def-method m_0_deliver_PART128_ST1
	'((task tdo_deliver_PART128_ST1)
	(:params (?a location) (?b location) (?c truck))
	(:pre-conditions (and-cond (road ?b ?a) (road ?a ?b) (truck-at ?c ?b)))
	(:score 0)
	(:body (do (move-truck ?c ?b ?a)))))
	(def-method mcheck_deliver_PART128_ST1
	'((task deliver_PART128_ST1)
	(:params )
	(:pre-conditions true)
	(:score 1)
	(:body )))
	(def-action-model load-plane
	    '((:params (?a package) (?b plane) (?c location))
	        (:pre-conditions (and-cond (plane-at ?b ?c) (package-at ?a ?c)))
	        (:effects
	        (begin
	            (assert `(package-in-plane ,?a ,?b) true)
	            (assert `(package-at ,?a ,?c) false)
	            ))))

	(def-action-model load-truck
	    '((:params (?a package) (?b truck) (?c location))
	        (:pre-conditions (and-cond (truck-at ?b ?c) (package-at ?a ?c)))
	        (:effects
	        (begin
	            (assert `(package-in-truck ,?a ,?b) true)
	            (assert `(package-at ,?a ,?c) false)
	            ))))

	(def-action-model unload-plane
	    '((:params (?a package) (?b plane) (?c location))
	        (:pre-conditions (and-cond (plane-at ?b ?c) (package-in-plane ?a ?b)))
	        (:effects
	        (begin
	            (assert `(package-in-plane ,?a ,?b) false)
	            (assert `(package-at ,?a ,?c) true)
	            ))))


	(def-action-model unload-truck
	    '((:params (?a package) (?b truck) (?c location))
	        (:pre-conditions (and-cond (truck-at ?b ?c) (package-in-truck ?a ?b)))
	        (:effects
	        (begin
	            (assert `(package-in-truck ,?a ,?b) false)
	            (assert `(package-at ,?a ,?c) true)
	            ))))

	(def-action-model move-plane
	    '((:params (?a plane) (?b location) (?c location))
	        (:pre-conditions (and-cond (plane-at ?a ?b) (is-airport ?c)))
	        (:effects
	        (begin
	            (assert `(plane-at ,?a ,?c) true)
	            (assert `(plane-at ,?a ,?b) false)
	            ))))

	(def-action-model move-truck
	    '((:params (?a truck) (?b location) (?c location))
	        (:pre-conditions (and-cond (truck-at ?a ?b) (road ?b ?c)))
	        (:effects
	        (begin
	            (assert `(truck-at ,?a ,?c) true)
	            (assert `(truck-at ,?a ,?b) false)
	            ))))

	; (def-action-model NULL
	;     '((:params )
	;         (:pre-conditions true)
	;         (:effects nil)))
)