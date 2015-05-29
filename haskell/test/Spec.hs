import Test.Hspec
import CG/CG3

main = hspec $ do
  describe "insertLineInY" $ do
    it "inserts l into an empty list" $ do
      insertLine testLine [] `shouldBe` [testLine]
    it "insert l at the beginning, when y of l's start point is lower than the first list element's"
      insertLine testLine greaterLines `shouldBe` testLine:greaterLines
  describe "intersectPoint" $ do
    it "returns Nothing when lines are parallel" $ do
      uncurry intersectPoint parallelLines `shouldBe` Nothing
    it "throws error on a vertical line" $ do
      uncurry intersectPoint lineAndVerticalLine `shouldThrow` error
    it "returns Nothing when intersection point is not between the endpoints" $ do
      uncurry intersectPoint linesWithIntersectionOutside `shouldBe` Nothing
    it "returns Just p when intersection point is between the endpoints" $ do
      uncurry intersectPoint linesWithIntersectionInside `shouldBe` Just Point { xCoord = 0.25, yCoord = 1.25 }



mkLine x1 y1 x2 y2 = (Point { xCoord=x1, yCoord=y1 }, Point { xCoord=x2, yCoord=y2 })

testLine = mkLine 0 1 1 2
greaterLines = [mkLine 2 4 4 5, mkLine 2 5 4 6]

parallelLines = (testLine, mkLine 1 1 2 2)
lineAndVerticalLine = (testLine, mkLine 0.5 0 0.5 1)
linesWithIntersectionOutside = (testLine, mkLine 0 2 0.5 3) 
linesWithIntersectionInside = (testLine, mkLine 0 0.5 0.5 2) 
