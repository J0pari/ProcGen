namespace TestInfrastructure

module Metrology =

    open System
    open MathNet.Numerics.Statistics
    open global.Xunit

    type GaugeRnRResult = {
        TotalVariation: float
        PartVariation: float
        Repeatability: float
        Reproducibility: float
        MeasurementVariation: float
        GRRPercent: float
        Ndc: float
        StudyVariationPercent: float option
    }
    let computeGaugeRnR (parts: int) (operators: int) (trials: int) (measurements: float[,,]) (tolerance: float option) : GaugeRnRResult =

        let grandMean =
            seq { for p in 0..parts-1 do
                    for o in 0..operators-1 do
                        for t in 0..trials-1 -> measurements.[p,o,t] }
            |> Seq.average

        let partAvgs =
            Array.init parts (fun p ->
                seq { for o in 0..operators-1 do
                        for t in 0..trials-1 -> measurements.[p,o,t] }
                |> Seq.average)

        let operatorAvgs =
            Array.init operators (fun o ->
                seq { for p in 0..parts-1 do
                        for t in 0..trials-1 -> measurements.[p,o,t] }
                |> Seq.average)

        let ssPart =
            float (operators * trials) *
            (partAvgs |> Array.sumBy (fun avg -> (avg - grandMean) ** 2.0))

        let ssOperator =
            float (parts * trials) *
            (operatorAvgs |> Array.sumBy (fun avg -> (avg - grandMean) ** 2.0))

        let partOpAvgs =
            Array2D.init parts operators (fun p o ->
                seq { for t in 0..trials-1 -> measurements.[p,o,t] }
                |> Seq.average)

        let ssInteraction =
            float trials *
            (seq { for p in 0..parts-1 do
                     for o in 0..operators-1 ->
                        let cellAvg = partOpAvgs.[p,o]
                        let expectedAvg = partAvgs.[p] + operatorAvgs.[o] - grandMean
                        (cellAvg - expectedAvg) ** 2.0 }
             |> Seq.sum)

        let ssRepeatability =
            seq { for p in 0..parts-1 do
                    for o in 0..operators-1 ->
                        let cellAvg = partOpAvgs.[p,o]
                        seq { for t in 0..trials-1 -> measurements.[p,o,t] }
                        |> Seq.sumBy (fun x -> (x - cellAvg) ** 2.0) }
            |> Seq.sum

        let dfPart = parts - 1
        let dfOperator = operators - 1
        let dfInteraction = dfPart * dfOperator
        let dfRepeatability = parts * operators * (trials - 1)

        let msPart = ssPart / float dfPart
        let msOperator = ssOperator / float dfOperator
        let msInteraction = ssInteraction / float dfInteraction
        let msRepeatability = ssRepeatability / float dfRepeatability

        let varRepeatability = msRepeatability

        let varInteraction = max 0.0 ((msInteraction - msRepeatability) / float trials)

        let varReproducibility =
            let varOp = max 0.0 ((msOperator - msInteraction) / float (parts * trials))
            varOp + varInteraction

        let varPart = max 0.0 ((msPart - msInteraction) / float (operators * trials))

        let varMeasurement = varRepeatability + varReproducibility
        let varTotal = varPart + varMeasurement

        let sdPart = sqrt varPart
        let sdMeasurement = sqrt varMeasurement
        let sdTotal = sqrt varTotal

        let grrPercent = 100.0 * sdMeasurement / sdTotal

        let ndc = 1.41 * sdPart / sdMeasurement

        let studyVarPercent =
            tolerance |> Option.map (fun tol ->
                100.0 * (5.15 * sdMeasurement) / tol)

        {
            TotalVariation = varTotal
            PartVariation = varPart
            Repeatability = varRepeatability
            Reproducibility = varReproducibility
            MeasurementVariation = varMeasurement
            GRRPercent = grrPercent
            Ndc = ndc
            StudyVariationPercent = studyVarPercent
        }

    let shouldHaveAcceptableGRR (result: GaugeRnRResult) : unit =
        Assert.True(result.GRRPercent < 30.0,
            sprintf "GRR%% = %.2f%% exceeds 30%% threshold (unacceptable measurement system)" result.GRRPercent)

    let shouldHaveAdequateDiscrimination (result: GaugeRnRResult) : unit =
        Assert.True(result.Ndc >= 5.0,
            sprintf "ndc = %.2f < 5 (inadequate discrimination)" result.Ndc)

    type ProcessCapability = {
        Mean: float
        StdDevWithin: float
        StdDevOverall: float
        LSL: float option
        USL: float option
        Target: float option
        Cp: float option
        Cpk: float option
        Pp: float option
        Ppk: float option
        Cpm: float option
        SigmaLevel: float option
        DPMO: float option
    }
    let computeProcessCapability (samples: float seq) (lowerSpecLimit: float option) (upperSpecLimit: float option) (target: float option) : ProcessCapability =
        let arr = samples |> Seq.toArray
        let sampleMean = arr |> Array.average

        let stdDevWithin = ArrayStatistics.StandardDeviation(arr)
        let stdDevOverall = stdDevWithin

        let cp =
            match lowerSpecLimit, upperSpecLimit with
            | Some l, Some u -> Some ((u - l) / (6.0 * stdDevWithin))
            | _ -> None

        let cpk =
            match lowerSpecLimit, upperSpecLimit with
            | Some l, Some u ->
                let cpkUpper = (u - sampleMean) / (3.0 * stdDevWithin)
                let cpkLower = (sampleMean - l) / (3.0 * stdDevWithin)
                Some (min cpkUpper cpkLower)
            | Some l, None ->
                Some ((sampleMean - l) / (3.0 * stdDevWithin))
            | None, Some u ->
                Some ((u - sampleMean) / (3.0 * stdDevWithin))
            | None, None -> None

        let pp =
            match lowerSpecLimit, upperSpecLimit with
            | Some l, Some u -> Some ((u - l) / (6.0 * stdDevOverall))
            | _ -> None

        let ppk =
            match lowerSpecLimit, upperSpecLimit with
            | Some l, Some u ->
                let ppkUpper = (u - sampleMean) / (3.0 * stdDevOverall)
                let ppkLower = (sampleMean - l) / (3.0 * stdDevOverall)
                Some (min ppkUpper ppkLower)
            | Some l, None ->
                Some ((sampleMean - l) / (3.0 * stdDevOverall))
            | None, Some u ->
                Some ((u - sampleMean) / (3.0 * stdDevOverall))
            | None, None -> None

        let cpm =
            match lowerSpecLimit, upperSpecLimit, target with
            | Some l, Some u, Some t ->
                let denominator = 6.0 * sqrt (stdDevWithin ** 2.0 + (sampleMean - t) ** 2.0)
                Some ((u - l) / denominator)
            | _ -> None

        let sigmaLevel =
            match lowerSpecLimit, upperSpecLimit with
            | Some l, Some u ->
                let sigmaUpper = (u - sampleMean) / stdDevWithin
                let sigmaLower = (sampleMean - l) / stdDevWithin
                Some (min sigmaUpper sigmaLower)
            | Some l, None -> Some ((sampleMean - l) / stdDevWithin)
            | None, Some u -> Some ((u - sampleMean) / stdDevWithin)
            | None, None -> None

        let dpmo =
            sigmaLevel |> Option.map (fun sigma ->
                let normalDist = MathNet.Numerics.Distributions.Normal(0.0, 1.0)
                let defectRate = 2.0 * (1.0 - normalDist.CumulativeDistribution(sigma))
                defectRate * 1000000.0)

        {
            Mean = sampleMean
            StdDevWithin = stdDevWithin
            StdDevOverall = stdDevOverall
            LSL = lowerSpecLimit
            USL = upperSpecLimit
            Target = target
            Cp = cp
            Cpk = cpk
            Pp = pp
            Ppk = ppk
            Cpm = cpm
            SigmaLevel = sigmaLevel
            DPMO = dpmo
        }

    let shouldBeCapable (result: ProcessCapability) : unit =
        match result.Cpk with
        | Some cpk ->
            Assert.True(cpk >= 1.33,
                sprintf "Cpk = %.3f < 1.33 (process not capable)" cpk)
        | None ->
            Assert.Fail("Cannot assess capability: missing specification limits")

    let shouldBeSixSigma (result: ProcessCapability) : unit =
        match result.SigmaLevel with
        | Some sigma ->
            Assert.True(sigma >= 6.0,
                sprintf "Sigma level = %.2f < 6 (not Six Sigma)" sigma)
        | None ->
            Assert.Fail("Cannot assess sigma level: missing specification limits")

    type Recommendation = {
        Severity: string
        Category: string
        Message: string
        Threshold: float option
        ActualValue: float option
    }
    let generateRecommendations (gaugernr: GaugeRnRResult option) (capability: ProcessCapability option) : Recommendation list =
        let mutable recs = []

        match gaugernr with
        | Some grr ->
            if grr.GRRPercent >= 30.0 then
                recs <- { Severity = "Critical";
                         Category = "Measurement System";
                         Message = sprintf "GRR = %.1f%% exceeds 30%% - measurement system unacceptable" grr.GRRPercent;
                         Threshold = Some 30.0;
                         ActualValue = Some grr.GRRPercent } :: recs
            elif grr.GRRPercent >= 10.0 then
                recs <- { Severity = "Warning";
                         Category = "Measurement System";
                         Message = sprintf "GRR = %.1f%% in marginal range (10-30%%)" grr.GRRPercent;
                         Threshold = Some 10.0;
                         ActualValue = Some grr.GRRPercent } :: recs

            if grr.Ndc < 5.0 then
                recs <- { Severity = "Critical";
                         Category = "Measurement Discrimination";
                         Message = sprintf "ndc = %.1f < 5 - inadequate measurement resolution" grr.Ndc;
                         Threshold = Some 5.0;
                         ActualValue = Some grr.Ndc } :: recs
        | None -> ()

        match capability with
        | Some cap ->
            match cap.Cpk with
            | Some cpk when cpk < 1.0 ->
                recs <- { Severity = "Critical";
                         Category = "Process Capability";
                         Message = sprintf "Cpk = %.2f < 1.0 - process incapable" cpk;
                         Threshold = Some 1.33;
                         ActualValue = Some cpk } :: recs
            | Some cpk when cpk < 1.33 ->
                recs <- { Severity = "Warning";
                         Category = "Process Capability";
                         Message = sprintf "Cpk = %.2f < 1.33 - process marginally capable" cpk;
                         Threshold = Some 1.33;
                         ActualValue = Some cpk } :: recs
            | _ -> ()

            match cap.DPMO with
            | Some dpmo when dpmo > 3.4 ->
                recs <- { Severity = "Info";
                         Category = "Quality Level";
                         Message = sprintf "DPMO = %.1f exceeds Six Sigma target (3.4)" dpmo;
                         Threshold = Some 3.4;
                         ActualValue = Some dpmo } :: recs
            | _ -> ()
        | None -> ()

        recs
