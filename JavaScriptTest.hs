import Prelude hiding (LT, GT, EQ, id)
import Base
import JavaScript
import JavaScriptParse

execute exp = show (runStateful (evaluate exp []))

main = testMain parseExp execute
      
  