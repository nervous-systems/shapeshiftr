(ns shapeshiftr.test.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [shapeshiftr.test.core-test]))

(doo-tests 'shapeshiftr.test.core-test)
