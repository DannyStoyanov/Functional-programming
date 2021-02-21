module Task1_Test where 

import Test.HUnit
import Task1
-- Даниел Здравков Стоянов, 45574, Информатика, 1-ва група
testTree = (Node 70 
                (Node 49
                        (Node 37 
                                (Node 22 
                                        EmptyTree 
                                        EmptyTree)
                                (Node 40 
                                        EmptyTree 
                                        EmptyTree))
                        (Node 54 
                                EmptyTree 
                                EmptyTree))
                (Node 84 
                        (Node 78
                                (Node 76
                                        EmptyTree
                                        EmptyTree)
                                (Node 80 
                                        EmptyTree
                                        EmptyTree))
                        (Node 85 
                                EmptyTree
                                EmptyTree)))

testInorder = TestCase $ assertEqual "Strategy Inorder" (values Inorder testTree) [22,37,40,49,54,70,76,78,80,84,85]
testPostorder = TestCase $ assertEqual "Strategy Postorder" (values Postorder testTree) [22,40,37,54,49,76,80,78,85,84,70]
testPreorder = TestCase $ assertEqual "Strategy Preorder" (values Preorder testTree) [70,49,37,22,40,54,84,78,76,80,85]

tests = TestList[testInorder, testPostorder, testPreorder]
main = runTestTT tests