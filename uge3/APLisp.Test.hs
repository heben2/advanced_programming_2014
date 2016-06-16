import Test.HUnit
import APLisp



test1 = TestCase $ assertBool "Test1" $ "Right.(IntVal 5)"== APLisp.interpret "(let ((f (lambda (x y) (+ x y)))) (funcall f (list 2 3)))"


tests = TestList [TestList [test1]]


main = runTestTT tests