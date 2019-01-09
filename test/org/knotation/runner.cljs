(ns org.knotation.runner
	(:require [cljs.test :as test]
			  [doo.runner :refer-macros [doo-all-tests doo-tests]]
			  [org.knotation.cljs-api-test]))

(doo-tests 'org.knotation.cljs-api-test)