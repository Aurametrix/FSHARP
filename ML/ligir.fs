load "LogisticRegression.fs"
// replace this path with the local path where FSharpChart is located
#r @"C:\Users\Mathias\Documents\GitHub\Machine-Learning-In-Action\MachineLearningInAction\packages\MSDN.FSharpChart.dll.0.60\lib\MSDN.FSharpChart.dll"
#r "System.Windows.Forms.DataVisualization"
open MachineLearning.LogisticRegression
open System.Drawing
open System.Windows.Forms.DataVisualization
open MSDN.FSharp.Charting

#time

// illustration on small example
let testSet =
    [ [ 0.5 ; 0.7 ];
      [ 1.5 ; 2.3 ];
      [ 0.8 ; 0.8 ];
      [ 6.0 ; 9.0 ];
      [ 9.5 ; 5.5 ];     
      [ 6.5 ; 2.7 ];
      [ 2.1 ; 0.1 ];
      [ 3.2 ; 1.9 ] ]
let testLabels = [ 1.0 ; 1.0 ; 1.0; 1.0; 0.0 ; 0.0; 0.0; 0.0 ]
let dataset = Seq.zip testLabels testSet

// compute weights on 10 iterations, with alpha = 0.1
let estimates = simpleTrain dataset 10 0.1
let classifier = predict estimates

// display dataset, and "separating line"
let display (dataSet: (float * float) seq) (labels: string seq) (line: float -> float) =
    let byLabel = Seq.zip labels dataSet |> Seq.toArray
    let uniqueLabels = Seq.distinct labels
    FSharpChart.Combine 
        [ // separate points by class and scatterplot them
          for label in uniqueLabels ->
               let data = 
                    Array.filter (fun e -> label = fst e) byLabel
                    |> Array.map snd
               FSharpChart.Point(data) :> ChartTypes.GenericChart
               |> FSharpChart.WithSeries.Marker(Size=10)
          // plot line between left- and right-most points
          let x = Seq.map fst dataSet
          let xMin, xMax = Seq.min x, Seq.max x           
          let lineData = [ (xMin, line xMin); (xMax, line xMax)]
          yield FSharpChart.Line (lineData)  :> ChartTypes.GenericChart
        ]
    |> FSharpChart.Create    

let xy = testSet |> Seq.map (fun e -> e.[0], e.[1])
let labels = testLabels |> Seq.map (fun e -> e.ToString())
let line x = - estimates.[0] / estimates.[2] - x * estimates.[1] / estimates.[2]
let show = display xy labels line
