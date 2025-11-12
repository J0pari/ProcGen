namespace TestInfrastructure

module Statistics =

    open System
    open MathNet.Numerics
    open MathNet.Numerics.Statistics
    open MathNet.Numerics.Distributions
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Optimization
    open Accord.Statistics.Testing
    open global.Xunit

    let mean (samples: float seq) : float = samples |> Seq.average
    let variance (samples: float seq) : float = samples |> Seq.toArray |> ArrayStatistics.Variance
    let stdDev (samples: float seq) : float = samples |> variance |> sqrt
    let standardError (samples: float seq) : float = (stdDev samples) / sqrt (float (Seq.length samples))

    let acceptanceRate (predicate: 'a -> bool) (trials: 'a seq) : float =
        let total = trials |> Seq.length |> float
        let successes = trials |> Seq.filter predicate |> Seq.length |> float
        successes / total

    let andersonDarlingNormal (samples: float seq) : float * float * bool =
        let arr = samples |> Seq.toArray |> Array.sort
        let n = arr.Length |> float
        let sampleMean = mean arr
        let sampleStd = stdDev arr
        let normalDist = Normal(sampleMean, sampleStd)

        let aSq =
            arr
            |> Array.mapi (fun i x ->
                let Fi = normalDist.CumulativeDistribution(x)
                let FnMinusI = normalDist.CumulativeDistribution(arr.[arr.Length - 1 - i])
                float (2 * i + 1) * (log Fi + log (1.0 - FnMinusI)))
            |> Array.sum
            |> fun sum -> -n - sum / n

        let aStarSq = aSq * (1.0 + 0.75/n + 2.25/(n*n))

        let criticalValue = 0.787
        let reject = aStarSq > criticalValue

        let pValue =
            if aStarSq < 0.2 then 1.0 - exp(-13.436 + 101.14*aStarSq - 223.73*aStarSq*aStarSq)
            elif aStarSq < 0.34 then 1.0 - exp(-8.318 + 42.796*aStarSq - 59.938*aStarSq*aStarSq)
            elif aStarSq < 0.6 then exp(0.9177 - 4.279*aStarSq - 1.38*aStarSq*aStarSq)
            else exp(1.2937 - 5.709*aStarSq + 0.0186*aStarSq*aStarSq)

        (aStarSq, pValue, reject)

    let cramerVonMises (cdf: float -> float) (samples: float seq) : float * float =
        let arr = samples |> Seq.toArray |> Array.sort
        let n = arr.Length |> float

        let omega2 =
            arr
            |> Array.mapi (fun i x ->
                let Fi = cdf x
                let empirical = (2.0 * float i + 1.0) / (2.0 * n)
                (Fi - empirical) ** 2.0)
            |> Array.sum
            |> (+) (1.0 / (12.0 * n))

        let omegaStarSq = omega2 * (1.0 + 0.5/n)

        let criticalValue = 0.461
        (omegaStarSq, if omegaStarSq > criticalValue then 0.05 else 0.10)

    let shapiroWilk (samples: float seq) : float * float * bool =
        let arr = samples |> Seq.toArray
        let test = ShapiroWilkTest(arr)
        (test.Statistic, test.PValue, test.Significant)

    let chiSquareUniformity (bins: int) (samples: float seq) : float * float * bool =
        let arr = samples |> Seq.toArray
        let minVal, maxVal = Array.min arr, Array.max arr
        let binWidth = (maxVal - minVal) / float bins

        let observed =
            arr
            |> Array.groupBy (fun x -> int ((x - minVal) / binWidth) |> min (bins - 1))
            |> Map.ofArray
            |> fun m -> Array.init bins (fun i -> Map.tryFind i m |> Option.map Array.length |> Option.defaultValue 0 |> float)

        let expected = float arr.Length / float bins
        let chiSquare =
            observed
            |> Array.sumBy (fun obs -> (obs - expected) ** 2.0 / expected)
        let df = bins - 1
        let test = ChiSquareTest(chiSquare, df)
        (test.Statistic, test.PValue, test.Significant)

    let kolmogorovSmirnov (cdf: float -> float) (samples: float seq) : float * float * bool =
        let arr = samples |> Seq.toArray |> Array.sort
        let n = arr.Length |> float

        let dPlus = arr |> Array.mapi (fun i x -> (float (i + 1)) / n - cdf x) |> Array.max
        let dMinus = arr |> Array.mapi (fun i x -> cdf x - float i / n) |> Array.max
        let dStat = max dPlus dMinus

        let pValue = 2.0 * exp(-2.0 * n * dStat * dStat)
        (dStat, pValue, pValue < 0.05)

    let pearsonCorrelation (x: float seq) (y: float seq) : float =
        let xArr = x |> Seq.toArray
        let yArr = y |> Seq.toArray
        if xArr.Length <> yArr.Length then failwith "Sequences must have same length"
        let n = float xArr.Length
        let xMean = mean xArr
        let yMean = mean yArr
        let numerator = Array.map2 (fun xi yi -> (xi - xMean) * (yi - yMean)) xArr yArr |> Array.sum
        let xStd = stdDev xArr
        let yStd = stdDev yArr
        numerator / (float (xArr.Length - 1) * xStd * yStd)

    let autocorrelation (lag: int) (series: float array) : float =
        let n = series.Length
        let sampleMean = mean series
        let sampleVar = variance series
        let numerator =
            series
            |> Array.skip lag
            |> Array.mapi (fun i x -> (series.[i] - sampleMean) * (x - sampleMean))
            |> Array.sum
        numerator / (float n * sampleVar)

    let fitAR (order: int) (series: float seq) : float[] * float * float =
        let arr = series |> Seq.toArray
        let acf = Array.init (order + 1) (fun lag -> autocorrelation lag arr)
        let gamma = acf |> Array.take order
        let gammaMatrix = Array.init order (fun i -> Array.init order (fun j -> acf.[abs (i - j)]))

        let matrixG = DenseMatrix.ofRowArrays(gammaMatrix)
        let vectorGamma = DenseVector.ofArray(gamma)
        let coeffs = matrixG.Solve(vectorGamma).ToArray()

        let sampleMean = mean arr
        let intercept = sampleMean * (1.0 - Array.sum coeffs)

        let predictions =
            arr |> Array.mapi (fun i _ ->
                if i < order then sampleMean
                else intercept + (Array.init order (fun j -> coeffs.[j] * arr.[i - j - 1]) |> Array.sum))

        let residuals = Array.map2 (-) arr predictions
        let residualVar = variance residuals

        (coeffs, intercept, residualVar)

    let akaikeCorrectedWithJacobian (k: int) (n: int) (logLikelihood: float) (jacobianLogDet: float) : float =
        let aic = -2.0 * logLikelihood + 2.0 * float k
        let correction = 2.0 * float (k * (k + 1)) / float (n - k - 1)
        aic + correction + jacobianLogDet

    let akaike (k: int) (n: int) (logLik: float) : float = -2.0 * logLik + 2.0 * float k
    let bayesian (k: int) (n: int) (logLik: float) : float = -2.0 * logLik + float k * log (float n)

    let durbinWatson (residuals: float seq) : float =
        let arr = residuals |> Seq.toArray
        let numerator = arr |> Array.pairwise |> Array.sumBy (fun (e1, e2) -> (e2 - e1) ** 2.0)
        let denominator = arr |> Array.sumBy (fun e -> e ** 2.0)
        numerator / denominator

    let ljungBox (lags: int) (residuals: float seq) : float * float * bool =
        let arr = residuals |> Seq.toArray
        let n = arr.Length |> float
        let acfVals = Array.init lags (fun k -> autocorrelation (k + 1) arr)
        let qStat = acfVals |> Array.mapi (fun k rho -> rho * rho / (n - float k - 1.0)) |> Array.sum |> (*) (n * (n + 2.0))
        let chiSq = ChiSquared(float lags)
        let pValue = 1.0 - chiSq.CumulativeDistribution(qStat)
        (qStat, pValue, pValue < 0.05)


    let coefficientOfVariation (samples: float seq) : float =
        stdDev samples / mean samples

    let monteCarloError (sampleSize: int) (sampleVariance: float) : float =
        sqrt (sampleVariance / float sampleSize)

    let effectiveSampleSize (samples: float seq) (maxLag: int) : float =
        let arr = samples |> Seq.toArray
        let n = arr.Length |> float
        let sumAcf = [1..maxLag] |> List.sumBy (fun k -> autocorrelation k arr)
        n / (1.0 + 2.0 * sumAcf)

    let hannanQuinn (k: int) (n: int) (logLik: float) : float =
        -2.0 * logLik + 2.0 * float k * log (log (float n))


    let levenbergMarquardt
        (f: float[] -> float -> float)
        (jacobian: float[] -> float -> float[])
        (xData: float[])
        (yData: float[])
        (initialParams: float[])
        (maxIterations: int)
        (tolerance: float) : float[] * float =

        let mutable currentParams = initialParams
        let mutable lambda = 0.001
        let n = xData.Length
        let p = currentParams.Length

        let residuals (prms: float[]) =
            Array.map2 (fun x y -> y - f prms x) xData yData

        let sse (prms: float[]) =
            residuals prms |> Array.sumBy (fun r -> r * r)

        let mutable currentSSE = sse currentParams

        for iter in 1..maxIterations do
            let J = Array.init n (fun i -> jacobian currentParams xData.[i]) |> array2D
            let r = residuals currentParams

            let JtJ = DenseMatrix.ofArray2(Array2D.init p p (fun i j ->
                Array.sumBy (fun k -> J.[k,i] * J.[k,j]) [|0..n-1|]))

            let JtR = DenseVector.ofArray(Array.init p (fun i ->
                Array.sumBy (fun k -> J.[k,i] * r.[k]) [|0..n-1|]))

            let identity = DenseMatrix.identity p
            let augmented = JtJ.Add(identity.Multiply(lambda))

            let delta = augmented.Solve(JtR).ToArray()
            let newParams = Array.map2 (+) currentParams delta
            let newSSE = sse newParams

            if newSSE < currentSSE then
                currentParams <- newParams
                currentSSE <- newSSE
                lambda <- lambda / 10.0
                if Array.forall (fun d -> abs d < tolerance) delta then
                    ()
            else
                lambda <- lambda * 10.0

        (currentParams, currentSSE)

    let trustRegion
        (f: float[] -> float)
        (gradient: float[] -> float[])
        (hessian: float[] -> float[,])
        (initialParams: float[])
        (maxIterations: int)
        (tolerance: float) : float[] * float =

        let mutable currentParams = initialParams
        let mutable radius = 1.0
        let mutable currentF = f currentParams

        for iter in 1..maxIterations do
            let g = gradient currentParams
            let H = hessian currentParams |> DenseMatrix.ofArray2

            let p =
                try
                    let step = H.Solve(DenseVector.ofArray(g |> Array.map (~-))).ToArray()
                    let stepNorm = Array.sumBy (fun x -> x*x) step |> sqrt
                    if stepNorm <= radius then step
                    else Array.map (fun x -> x * radius / stepNorm) step
                with _ ->
                    let gNorm = Array.sumBy (fun x -> x*x) g |> sqrt
                    Array.map (fun x -> -x * radius / gNorm) g

            let newParams = Array.map2 (+) currentParams p
            let newF = f newParams
            let predictedReduction =
                let gp = Array.sumBy (fun i -> g.[i] * p.[i]) [|0..g.Length-1|]
                let pHp =
                    [|0..p.Length-1|]
                    |> Array.sumBy (fun i ->
                        [|0..p.Length-1|]
                        |> Array.sumBy (fun j -> p.[i] * (hessian currentParams).[i,j] * p.[j]))
                -(gp + 0.5 * pHp)

            let actualReduction = currentF - newF
            let rho = if predictedReduction > 0.0 then actualReduction / predictedReduction else 0.0

            if rho > 0.25 then
                currentParams <- newParams
                currentF <- newF
                if rho > 0.75 then radius <- min (2.0 * radius) 10.0
            elif rho < 0.25 then
                radius <- radius * 0.25

            if Array.sumBy (fun x -> x*x) p |> sqrt < tolerance then ()

        (currentParams, currentF)


    let difference (order: int) (series: float[]) : float[] =
        let mutable result = series
        for _ in 1..order do
            result <- Array.pairwise result |> Array.map (fun (a, b) -> b - a)
        result

    let seasonalDifference (period: int) (series: float[]) : float[] =
        series |> Array.skip period |> Array.mapi (fun i x -> x - series.[i])

    type ARIMAModel = {
        AR: float[]      // φ coefficients
        MA: float[]      // θ coefficients
        Intercept: float
        Sigma2: float    // innovation variance
    }

    let fitARIMA (p: int) (d: int) (q: int) (series: float[]) : ARIMAModel =
        let diffed = difference d series
        let n = diffed.Length

        let mutable maCoeffs = Array.zeroCreate q

        let arCoeffs =
            if p > 0 then
                let (ar, _, _) = fitAR p diffed
                ar
            else Array.empty

        let predictions =
            diffed |> Array.mapi (fun i _ ->
                if i < p then mean diffed
                else
                    let arPart = if p > 0 then Array.sumBy (fun j -> arCoeffs.[j] * diffed.[i-j-1]) [|0..p-1|] else 0.0
                    arPart)

        let residuals = Array.map2 (-) diffed predictions
        let sigma2 = variance residuals

        {
            AR = arCoeffs
            MA = maCoeffs
            Intercept = mean diffed
            Sigma2 = sigma2
        }

    type SARIMAModel = {
        Nonseasonal: ARIMAModel
        SeasonalAR: float[]   // Φ coefficients
        SeasonalMA: float[]   // Θ coefficients
        Period: int
    }

    let fitSARIMA (p: int) (d: int) (q: int) (P: int) (D: int) (Q: int) (period: int) (series: float[]) : SARIMAModel =
        let diffed = difference d series
        let seasonalDiffed = seasonalDifference period diffed |> Array.skip (D * period)

        let nonseasonal = fitARIMA p 0 q seasonalDiffed

        let seasonalAR =
            if P > 0 then
                Array.init P (fun i -> autocorrelation ((i+1) * period) seasonalDiffed)
            else Array.empty

        {
            Nonseasonal = nonseasonal
            SeasonalAR = seasonalAR
            SeasonalMA = Array.zeroCreate Q
            Period = period
        }

    let autoARIMA (series: float[]) (maxP: int) (maxD: int) (maxQ: int) : ARIMAModel * float =
        let mutable bestModel = Unchecked.defaultof<ARIMAModel>
        let mutable bestAICc = Double.PositiveInfinity

        for p in 0..maxP do
            for d in 0..maxD do
                for q in 0..maxQ do
                    let model = fitARIMA p d q series
                    let diffed = difference d series
                    let n = diffed.Length
                    let k = p + q + 1

                    let residuals =
                        let preds = Array.create diffed.Length model.Intercept
                        Array.map2 (-) diffed preds
                    let logLik = -0.5 * float n * (1.0 + log (2.0 * Math.PI) + log model.Sigma2)

                    let aicc = akaikeCorrectedWithJacobian k n logLik 0.0

                    if aicc < bestAICc then
                        bestModel <- model
                        bestAICc <- aicc

        (bestModel, bestAICc)


    let bartlettSphericity (data: float[,]) : float * int * float * bool =
        let n = Array2D.length1 data  // samples
        let p = Array2D.length2 data  // variables

        let means = Array.init p (fun j -> [|0..n-1|] |> Array.averageBy (fun i -> data.[i,j]))
        let stds = Array.init p (fun j ->
            let m = means.[j]
            [|0..n-1|] |> Array.averageBy (fun i -> (data.[i,j] - m) ** 2.0) |> sqrt)

        let R = Array2D.init p p (fun i j ->
            if i = j then 1.0
            else
                let cov = [|0..n-1|] |> Array.averageBy (fun k ->
                    (data.[k,i] - means.[i]) * (data.[k,j] - means.[j]))
                cov / (stds.[i] * stds.[j]))

        let Rmatrix = DenseMatrix.ofArray2(R)
        let detR = Rmatrix.Determinant()

        let chiSq = -(float n - 1.0 - (2.0 * float p + 5.0) / 6.0) * log detR
        let df = p * (p - 1) / 2

        let chiDist = ChiSquared(float df)
        let pValue = 1.0 - chiDist.CumulativeDistribution(chiSq)

        (chiSq, df, pValue, pValue < 0.05)

    let kmo (data: float[,]) : float * float[] =
        let n = Array2D.length1 data
        let p = Array2D.length2 data

        let means = Array.init p (fun j -> [|0..n-1|] |> Array.averageBy (fun i -> data.[i,j]))
        let stds = Array.init p (fun j ->
            let m = means.[j]
            [|0..n-1|] |> Array.averageBy (fun i -> (data.[i,j] - m) ** 2.0) |> sqrt)

        let R = Array2D.init p p (fun i j ->
            if i = j then 1.0
            else
                let cov = [|0..n-1|] |> Array.averageBy (fun k ->
                    (data.[k,i] - means.[i]) * (data.[k,j] - means.[j]))
                cov / (stds.[i] * stds.[j]))

        let Rmatrix = DenseMatrix.ofArray2(R)
        let Rinv = Rmatrix.Inverse()
        let A = Array2D.init p p (fun i j ->
            if i = j then 0.0
            else -Rinv.[i,j] / sqrt (Rinv.[i,i] * Rinv.[j,j]))

        let variableKMO = Array.init p (fun i ->
            let sumR2 = [|0..p-1|] |> Array.sumBy (fun j -> if i = j then 0.0 else R.[i,j] ** 2.0)
            let sumA2 = [|0..p-1|] |> Array.sumBy (fun j -> if i = j then 0.0 else A.[i,j] ** 2.0)
            sumR2 / (sumR2 + sumA2))

        let totalSumR2 = [|0..p-1|] |> Array.sumBy (fun i ->
            [|0..p-1|] |> Array.sumBy (fun j -> if i = j then 0.0 else R.[i,j] ** 2.0))
        let totalSumA2 = [|0..p-1|] |> Array.sumBy (fun i ->
            [|0..p-1|] |> Array.sumBy (fun j -> if i = j then 0.0 else A.[i,j] ** 2.0))

        let overallKMO = totalSumR2 / (totalSumR2 + totalSumA2)

        (overallKMO, variableKMO)

    type PCAResult = {
        Components: float[,]        // Principal components (eigenvectors)
        EigenValues: float[]        // Eigenvalues (variance explained)
        VarianceExplained: float[]  // Proportion of variance per component
        CumulativeVariance: float[] // Cumulative variance explained
        Communalities: float[]      // Variance explained per variable
        Loadings: float[,]          // Component loadings (scaled eigenvectors)
        Scores: float[,]            // Transformed data in PC space
    }

    let pca (data: float[,]) (nComponents: int) : PCAResult =
        let n = Array2D.length1 data
        let p = Array2D.length2 data

        let means = Array.init p (fun j -> [|0..n-1|] |> Array.averageBy (fun i -> data.[i,j]))
        let centered = Array2D.init n p (fun i j -> data.[i,j] - means.[j])

        let cov = Array2D.init p p (fun i j ->
            [|0..n-1|] |> Array.averageBy (fun k -> centered.[k,i] * centered.[k,j]))

        let covMatrix = DenseMatrix.ofArray2(cov)
        let evd = covMatrix.Evd()

        let eigenPairs =
            Array.zip (evd.EigenValues.Real().ToArray())
                      [|0..p-1|]
            |> Array.sortByDescending fst

        let eigenValues = eigenPairs |> Array.map fst |> Array.take nComponents
        let eigenVectors =
            eigenPairs
            |> Array.map (fun (_, idx) -> evd.EigenVectors.Column(idx).ToArray())
            |> Array.take nComponents
            |> array2D
            |> (fun arr -> Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun i j -> arr.[j,i]))

        let totalVariance = eigenPairs |> Array.sumBy fst
        let varianceExplained = eigenValues |> Array.map (fun ev -> ev / totalVariance)
        let cumulativeVariance =
            varianceExplained
            |> Array.scan (+) 0.0
            |> Array.skip 1

        let loadings = Array2D.init p nComponents (fun i j ->
            eigenVectors.[i,j] * sqrt eigenValues.[j])

        let communalities = Array.init p (fun i ->
            [|0..nComponents-1|] |> Array.sumBy (fun j -> loadings.[i,j] ** 2.0))

        let scores = Array2D.init n nComponents (fun i j ->
            [|0..p-1|] |> Array.sumBy (fun k -> centered.[i,k] * eigenVectors.[k,j]))

        {
            Components = eigenVectors
            EigenValues = eigenValues
            VarianceExplained = varianceExplained
            CumulativeVariance = cumulativeVariance
            Communalities = communalities
            Loadings = loadings
            Scores = scores
        }

    type Kernel =
        | RBF of gamma: float
        | Polynomial of degree: int * coef: float
        | Sigmoid of alpha: float * coef: float

    let kernelFunction (kernel: Kernel) (x: float[]) (y: float[]) : float =
        match kernel with
        | RBF gamma ->
            let dist2 = Array.map2 (fun xi yi -> (xi - yi) ** 2.0) x y |> Array.sum
            exp (-gamma * dist2)
        | Polynomial (degree, coef) ->
            let dot = Array.map2 (*) x y |> Array.sum
            (dot + coef) ** float degree
        | Sigmoid (alpha, coef) ->
            let dot = Array.map2 (*) x y |> Array.sum
            tanh (alpha * dot + coef)

    let kernelPCA (data: float[,]) (kernel: Kernel) (nComponents: int) : PCAResult =
        let n = Array2D.length1 data

        let K = Array2D.init n n (fun i j ->
            let xi = [|0..Array2D.length2 data - 1|] |> Array.map (fun k -> data.[i,k])
            let xj = [|0..Array2D.length2 data - 1|] |> Array.map (fun k -> data.[j,k])
            kernelFunction kernel xi xj)

        let rowMeans = Array.init n (fun i -> [|0..n-1|] |> Array.averageBy (fun j -> K.[i,j]))
        let colMeans = Array.init n (fun j -> [|0..n-1|] |> Array.averageBy (fun i -> K.[i,j]))
        let totalMean = rowMeans |> Array.average

        let Kcentered = Array2D.init n n (fun i j ->
            K.[i,j] - rowMeans.[i] - colMeans.[j] + totalMean)

        let Kmatrix = DenseMatrix.ofArray2(Kcentered)
        let evd = Kmatrix.Evd()

        let eigenPairs =
            Array.zip (evd.EigenValues.Real().ToArray()) [|0..n-1|]
            |> Array.sortByDescending fst
            |> Array.take nComponents

        let eigenValues = eigenPairs |> Array.map fst
        let eigenVectors =
            eigenPairs
            |> Array.map (fun (_, idx) -> evd.EigenVectors.Column(idx).ToArray())

        let alphas = Array.init nComponents (fun k ->
            let normFactor = sqrt eigenValues.[k]
            Array.map (fun x -> x / normFactor) eigenVectors.[k])

        let scores = Array2D.init n nComponents (fun i j ->
            [|0..n-1|] |> Array.sumBy (fun k -> Kcentered.[i,k] * alphas.[j].[k]))

        let totalVariance = eigenValues |> Array.sum
        let varianceExplained = eigenValues |> Array.map (fun ev -> ev / totalVariance)
        let cumulativeVariance =
            varianceExplained |> Array.scan (+) 0.0 |> Array.skip 1

        {
            Components = alphas |> array2D |> (fun arr -> Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun i j -> arr.[j,i]))
            EigenValues = eigenValues
            VarianceExplained = varianceExplained
            CumulativeVariance = cumulativeVariance
            Communalities = [||]
            Loadings = Array2D.zeroCreate 0 0
            Scores = scores
        }

    let parallelAnalysis (data: float[,]) (nSimulations: int) : int =
        let n = Array2D.length1 data
        let p = Array2D.length2 data

        let realPCA = pca data p
        let realEigenvalues = realPCA.EigenValues

        let rng = Random()
        let randomEigenvalues =
            Array.init nSimulations (fun _ ->
                let randomData = Array2D.init n p (fun _ _ -> rng.NextDouble())
                let randomPCA = pca randomData p
                randomPCA.EigenValues)
            |> Array.transpose
            |> Array.map Array.average

        Array.zip realEigenvalues randomEigenvalues
        |> Array.takeWhile (fun (real, random) -> real > random)
        |> Array.length
