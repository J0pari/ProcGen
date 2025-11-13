#include <cuda_runtime.h>
#include "sheaf_template.cuh"

// Convergence diagnostics computed on GPU to avoid CPU-GPU transfer overhead
// Uses warp reductions and atomic operations for real-time monitoring

struct ConvergenceMetrics {
    float gelmanRubin;           // R̂ statistic
    float effectiveSampleSize;   // ESS for coldest chain
    float costStabilization;     // Coefficient of variation
    float avgSwapAcceptance;     // Moving average swap rate
    int iteration;
    bool hasConverged;
};

// Compute per-chain cost mean and variance using block reduction
__global__ void compute_chain_statistics(
    const float* __restrict__ costHistory,
    float* chainMeans,
    float* chainVars,
    int historyLength,
    int numChains
) {
    int chainIdx = blockIdx.x;
    if (chainIdx >= numChains) return;

    const float* chainCosts = &costHistory[chainIdx * historyLength];

    // Each thread computes partial sum
    float sum = 0.0f;
    for (int i = threadIdx.x; i < historyLength; i += blockDim.x) {
        sum += chainCosts[i];
    }

    // Block reduction for sum
    sum = BlockReduce<256>::sum(sum);

    __shared__ float sharedMean;
    if (threadIdx.x == 0) {
        sharedMean = sum / float(historyLength);
        chainMeans[chainIdx] = sharedMean;
    }
    __syncthreads();

    // Compute variance
    float variance = 0.0f;
    float mean = sharedMean;
    for (int i = threadIdx.x; i < historyLength; i += blockDim.x) {
        float diff = chainCosts[i] - mean;
        variance += diff * diff;
    }

    variance = BlockReduce<256>::sum(variance);

    if (threadIdx.x == 0) {
        chainVars[chainIdx] = variance / float(historyLength - 1);
    }
}

// Gelman-Rubin R̂ diagnostic computed on GPU
__global__ void compute_gelman_rubin(
    const float* __restrict__ chainMeans,
    const float* __restrict__ chainVars,
    int numChains,
    int historyLength,
    float* outRHat
) {
    if (blockIdx.x > 0 || numChains < 2) return;

    // Compute grand mean across chains
    float grandMeanSum = 0.0f;
    for (int i = threadIdx.x; i < numChains; i += blockDim.x) {
        grandMeanSum += chainMeans[i];
    }
    grandMeanSum = BlockReduce<256>::sum(grandMeanSum);

    __shared__ float sharedGrandMean;
    if (threadIdx.x == 0) {
        sharedGrandMean = grandMeanSum / float(numChains);
    }
    __syncthreads();

    float grandMean = sharedGrandMean;

    // Between-chain variance: B = (n/(m-1)) * Σ(θ̄ⱼ - θ̄)²
    float betweenVar = 0.0f;
    for (int i = threadIdx.x; i < numChains; i += blockDim.x) {
        float diff = chainMeans[i] - grandMean;
        betweenVar += diff * diff;
    }
    betweenVar = BlockReduce<256>::sum(betweenVar);

    __shared__ float B;
    if (threadIdx.x == 0) {
        B = (float(historyLength) / float(numChains - 1)) * betweenVar;
    }
    __syncthreads();

    // Within-chain variance: W = (1/m) * Σsⱼ²
    float withinVar = 0.0f;
    for (int i = threadIdx.x; i < numChains; i += blockDim.x) {
        withinVar += chainVars[i];
    }
    withinVar = BlockReduce<256>::sum(withinVar);

    __shared__ float W;
    if (threadIdx.x == 0) {
        W = withinVar / float(numChains);
    }
    __syncthreads();

    // R̂ = sqrt(V̂ / W) where V̂ = ((n-1)/n)*W + (1/n)*B
    if (threadIdx.x == 0) {
        float n = float(historyLength);
        float varPlus = ((n - 1.0f) / n) * W + (1.0f / n) * B;

        if (W > 1e-6f) {
            *outRHat = sqrtf(varPlus / W);
        } else {
            *outRHat = 1.0f;  // Perfect convergence
        }
    }
}

// Autocorrelation for ESS computation (coldest chain only for efficiency)
__global__ void compute_autocorrelation(
    const float* __restrict__ costSeries,
    int seriesLength,
    int maxLag,
    float* outAutocorr
) {
    if (blockIdx.x >= maxLag) return;

    int lag = blockIdx.x;

    // Compute mean
    float sum = 0.0f;
    for (int i = threadIdx.x; i < seriesLength; i += blockDim.x) {
        sum += costSeries[i];
    }
    sum = BlockReduce<256>::sum(sum);

    __shared__ float sharedMean;
    __shared__ float sharedVariance;

    if (threadIdx.x == 0) {
        sharedMean = sum / float(seriesLength);
    }
    __syncthreads();

    float mean = sharedMean;

    // Compute variance
    float variance = 0.0f;
    for (int i = threadIdx.x; i < seriesLength; i += blockDim.x) {
        float diff = costSeries[i] - mean;
        variance += diff * diff;
    }
    variance = BlockReduce<256>::sum(variance);

    if (threadIdx.x == 0) {
        sharedVariance = variance / float(seriesLength);
    }
    __syncthreads();

    float var = sharedVariance;

    // Compute autocovariance at lag
    float autocovar = 0.0f;
    int count = seriesLength - lag;
    for (int i = threadIdx.x; i < count; i += blockDim.x) {
        autocovar += (costSeries[i] - mean) * (costSeries[i + lag] - mean);
    }
    autocovar = BlockReduce<256>::sum(autocovar);

    if (threadIdx.x == 0) {
        if (var > 1e-6f) {
            outAutocorr[lag] = autocovar / (float(count) * var);
        } else {
            outAutocorr[lag] = 0.0f;
        }
    }
}

// Effective Sample Size from autocorrelations
__global__ void compute_ess(
    const float* __restrict__ autocorr,
    int maxLag,
    int seriesLength,
    float* outESS
) {
    if (blockIdx.x > 0) return;

    // Sum positive autocorrelations until negligible
    float rhoSum = 0.0f;
    for (int k = 1 + threadIdx.x; k < maxLag; k += blockDim.x) {
        if (autocorr[k] > 0.05f) {
            rhoSum += autocorr[k];
        }
    }

    rhoSum = BlockReduce<256>::sum(rhoSum);

    if (threadIdx.x == 0) {
        float ess = float(seriesLength) / (1.0f + 2.0f * rhoSum);
        *outESS = fmaxf(1.0f, ess);
    }
}

// Coefficient of variation for cost stabilization
__global__ void compute_cost_cv(
    const float* __restrict__ recentCosts,
    int costCount,
    float* outCV
) {
    if (blockIdx.x > 0 || costCount == 0) return;

    float sum = 0.0f;
    for (int i = threadIdx.x; i < costCount; i += blockDim.x) {
        sum += recentCosts[i];
    }
    sum = BlockReduce<256>::sum(sum);

    __shared__ float sharedMean;
    if (threadIdx.x == 0) {
        sharedMean = sum / float(costCount);
    }
    __syncthreads();

    float mean = sharedMean;

    if (fabsf(mean) < 1e-6f) {
        if (threadIdx.x == 0) {
            *outCV = 0.0f;  // Near-zero costs = converged
        }
        return;
    }

    float variance = 0.0f;
    for (int i = threadIdx.x; i < costCount; i += blockDim.x) {
        float diff = recentCosts[i] - mean;
        variance += diff * diff;
    }
    variance = BlockReduce<256>::sum(variance);

    if (threadIdx.x == 0) {
        float stdDev = sqrtf(variance / float(costCount));
        *outCV = stdDev / fabsf(mean);
    }
}

// Check convergence with all metrics (executed once per check interval)
__global__ void check_convergence_kernel(
    float rHat,
    float ess,
    float costCV,
    float swapAcceptance,
    float strictness,
    int iteration,
    ConvergenceMetrics* outMetrics
) {
    if (threadIdx.x != 0 || blockIdx.x != 0) return;

    // Convergence thresholds based on strictness [0,1]
    float rHatThreshold = 1.2f - strictness * 0.19f;    // Range [1.01, 1.2]
    float cvThreshold = 0.1f - strictness * 0.09f;      // Range [0.01, 0.1]

    bool swapQuality = (swapAcceptance >= 0.2f && swapAcceptance <= 0.4f);

    bool hasConverged =
        (rHat < rHatThreshold) &&
        (costCV < cvThreshold) &&
        (ess > 50.0f) &&
        swapQuality;

    outMetrics->gelmanRubin = rHat;
    outMetrics->effectiveSampleSize = ess;
    outMetrics->costStabilization = costCV;
    outMetrics->avgSwapAcceptance = swapAcceptance;
    outMetrics->iteration = iteration;
    outMetrics->hasConverged = hasConverged;
}

extern "C" {
    // Stream-parallel convergence check to overlap with optimization
    __declspec(dllexport) void check_convergence_async(
        const float* costHistory,      // [numChains * historyLength]
        const bool* swapHistory,       // [swapHistoryLength]
        int numChains,
        int historyLength,
        int swapHistoryLength,
        float strictness,
        int iteration,
        ConvergenceMetrics* outMetrics,
        cudaStream_t stream
    ) {
        if (numChains < 2 || historyLength < 10) {
            // Not enough data for convergence check
            ConvergenceMetrics defaultMetrics = {
                .gelmanRubin = 2.0f,
                .effectiveSampleSize = 1.0f,
                .costStabilization = 1.0f,
                .avgSwapAcceptance = 0.0f,
                .iteration = iteration,
                .hasConverged = false
            };
            cudaMemcpyAsync(outMetrics, &defaultMetrics, sizeof(ConvergenceMetrics),
                            cudaMemcpyHostToDevice, stream);
            return;
        }

        // Allocate temporary device memory for intermediate results
        float *d_chainMeans, *d_chainVars, *d_rHat;
        float *d_autocorr, *d_ess, *d_costCV, *d_swapAvg;

        cudaMallocAsync(&d_chainMeans, numChains * sizeof(float), stream);
        cudaMallocAsync(&d_chainVars, numChains * sizeof(float), stream);
        cudaMallocAsync(&d_rHat, sizeof(float), stream);
        cudaMallocAsync(&d_autocorr, 50 * sizeof(float), stream);  // maxLag=50
        cudaMallocAsync(&d_ess, sizeof(float), stream);
        cudaMallocAsync(&d_costCV, sizeof(float), stream);
        cudaMallocAsync(&d_swapAvg, sizeof(float), stream);

        // 1. Compute chain statistics (mean, variance) for each chain
        compute_chain_statistics<<<numChains, 256, 0, stream>>>(
            costHistory, d_chainMeans, d_chainVars, historyLength, numChains
        );

        // 2. Compute Gelman-Rubin R̂
        compute_gelman_rubin<<<1, 256, 0, stream>>>(
            d_chainMeans, d_chainVars, numChains, historyLength, d_rHat
        );

        // 3. Compute autocorrelation for coldest chain (chain 0)
        int maxLag = min(historyLength / 3, 50);
        compute_autocorrelation<<<maxLag, 256, 0, stream>>>(
            costHistory, historyLength, maxLag, d_autocorr
        );

        // 4. Compute ESS from autocorrelations
        compute_ess<<<1, 256, 0, stream>>>(
            d_autocorr, maxLag, historyLength, d_ess
        );

        // 5. Compute coefficient of variation from recent costs
        int recentCount = min(historyLength / 5, 100);
        compute_cost_cv<<<1, 256, 0, stream>>>(
            costHistory, recentCount * numChains, d_costCV
        );

        // 6. Compute swap acceptance average (simple kernel)
        auto computeSwapAvg = [=] __device__ () {
            if (blockIdx.x > 0) return;

            float sum = 0.0f;
            for (int i = threadIdx.x; i < swapHistoryLength; i += blockDim.x) {
                sum += swapHistory[i] ? 1.0f : 0.0f;
            }
            sum = BlockReduce<256>::sum(sum);

            if (threadIdx.x == 0) {
                *d_swapAvg = sum / float(swapHistoryLength);
            }
        };

        // Launch swap average computation
        void (*swapKernel)() = computeSwapAvg;
        cudaLaunchKernel((void*)swapKernel, dim3(1), dim3(256), nullptr, 0, stream);

        // 7. Aggregate all metrics and check convergence
        // Copy results to host first (unavoidable but batched)
        float h_rHat, h_ess, h_costCV, h_swapAvg;
        cudaMemcpyAsync(&h_rHat, d_rHat, sizeof(float), cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(&h_ess, d_ess, sizeof(float), cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(&h_costCV, d_costCV, sizeof(float), cudaMemcpyDeviceToHost, stream);
        cudaMemcpyAsync(&h_swapAvg, d_swapAvg, sizeof(float), cudaMemcpyDeviceToHost, stream);

        cudaStreamSynchronize(stream);

        check_convergence_kernel<<<1, 1, 0, stream>>>(
            h_rHat, h_ess, h_costCV, h_swapAvg, strictness, iteration, outMetrics
        );

        // Cleanup (async free)
        cudaFreeAsync(d_chainMeans, stream);
        cudaFreeAsync(d_chainVars, stream);
        cudaFreeAsync(d_rHat, stream);
        cudaFreeAsync(d_autocorr, stream);
        cudaFreeAsync(d_ess, stream);
        cudaFreeAsync(d_costCV, stream);
        cudaFreeAsync(d_swapAvg, stream);
    }
}
