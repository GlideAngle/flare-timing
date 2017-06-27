import Reflex.Dom (mainWidget)

import FlareTiming.Task (tasks)

main :: IO ()
main = mainWidget tasks
