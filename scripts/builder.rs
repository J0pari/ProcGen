use std::process::{Command, Stdio};
use std::path::{Path, PathBuf};
use std::fs;
use std::time::{Instant, SystemTime, UNIX_EPOCH};
use std::collections::HashMap;
use serde_json;

struct BuildMetrics {
    events: Vec<(String, u128)>,
    durations: HashMap<String, u128>,
    artifacts: HashMap<String, String>,
}

impl BuildMetrics {
    fn new() -> Self {
        BuildMetrics {
            events: Vec::new(),
            durations: HashMap::new(),
            artifacts: HashMap::new(),
        }
    }

    fn record_duration(&mut self, phase: &str, duration_ms: u128) {
        self.durations.insert(phase.to_string(), duration_ms);
    }

    fn record_artifact(&mut self, name: &str, hash: &str) {
        self.artifacts.insert(name.to_string(), hash.to_string());
    }

    fn save(&self, path: &Path) -> std::io::Result<()> {
        let json = serde_json::json!({
            "durations": self.durations,
            "artifacts": self.artifacts,
            "total_phases": self.durations.len(),
        });
        fs::write(path, serde_json::to_string_pretty(&json)?)?;
        Ok(())
    }
}

struct BuildContext {
    root: PathBuf,
    metrics: BuildMetrics,
    start_time: Instant,
}

impl BuildContext {
    fn new() -> Self {
        let mut root = std::env::current_dir().unwrap();
        // If we're in scripts/, move up to parent
        if root.ends_with("scripts") {
            root = root.parent().unwrap().to_path_buf();
        }
        BuildContext {
            root,
            metrics: BuildMetrics::new(),
            start_time: Instant::now(),
        }
    }

    fn run_command(&mut self, name: &str, cmd: &mut Command) -> Result<(), String> {
        eprintln!("[{}] {}", Self::timestamp(), name);
        let start = Instant::now();

        let status = cmd.status().map_err(|e| format!("Failed to execute: {}", e))?;

        let duration = start.elapsed().as_millis();
        self.metrics.record_duration(name, duration);

        if status.success() {
            eprintln!("[{}] {} OK ({}ms)", Self::timestamp(), name, duration);
            Ok(())
        } else {
            eprintln!("[{}] {} FAILED", Self::timestamp(), name);
            Err(format!("{} failed with exit code {:?}", name, status.code()))
        }
    }

    fn timestamp() -> String {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let dt = chrono::DateTime::<chrono::Utc>::from_timestamp(now as i64, 0).unwrap();
        dt.format("%H:%M:%S").to_string()
    }

    fn strip_line_numbers(text: &str) -> String {
        use regex::Regex;
        let re = Regex::new(r"(\.fs)\((\d+),(\d+)\):").unwrap();
        re.replace_all(text, "$1:").to_string()
    }

    fn cuda_needs_rebuild(&self) -> bool {
        let lib = self.root.join("src/libgpu_compute.so");
        if !lib.exists() {
            return true;
        }

        let lib_time = fs::metadata(&lib).unwrap().modified().unwrap();
        let gpu_dir = self.root.join("src/gpu");

        if !gpu_dir.exists() {
            return false;
        }

        for entry in fs::read_dir(gpu_dir).unwrap() {
            let entry = entry.unwrap();
            if entry.path().extension().and_then(|s| s.to_str()) == Some("cu") {
                let cu_time = entry.metadata().unwrap().modified().unwrap();
                if cu_time > lib_time {
                    return true;
                }
            }
        }
        false
    }

    fn compute_sha256(&self, path: &Path) -> Result<String, String> {
        use sha2::{Sha256, Digest};
        let bytes = fs::read(path).map_err(|e| e.to_string())?;
        let hash = Sha256::digest(&bytes);
        Ok(format!("{:x}", hash))
    }

    fn phase_cuda(&mut self) -> Result<(), String> {
        if !self.root.join("src/gpu").exists() {
            eprintln!("[{}]   CUDA kernels not present (skipping)", Self::timestamp());
            return Ok(());
        }

        if !Command::new("nvcc").arg("--version").output().is_ok() {
            eprintln!("[{}]   nvcc not found (skipping CUDA)", Self::timestamp());
            return Ok(());
        }

        if !self.cuda_needs_rebuild() {
            eprintln!("[{}]   CUDA kernels up-to-date (skipping)", Self::timestamp());
            return Ok(());
        }

        eprintln!("[{}]   Compiling 8 CUDA kernels", Self::timestamp());

        let mut cmd = Command::new("nvcc");
        cmd.current_dir(self.root.join("src"))
            .arg("-shared")
            .arg("-o").arg("libgpu_compute.so")
            .arg("gpu/parallel_tempering.cu")
            .arg("gpu/hydraulic_erosion.cu")
            .arg("gpu/marching_cubes.cu")
            .arg("gpu/verlet_integration.cu")
            .arg("gpu/collision_detection.cu")
            .arg("gpu/noise_field.cu")
            .arg("gpu/spring_forces.cu")
            .arg("gpu/mc_tables.cu")
            .arg("-arch=sm_75")
            .arg("-O3")
            .arg("--use_fast_math")
            .arg("-std=c++17")
            .arg("-I").arg("gpu");

        #[cfg(not(target_os = "windows"))]
        cmd.arg("-Xcompiler").arg("-fPIC");

        self.run_command("cuda_build", &mut cmd)
    }

    fn phase_gpu_compute(&mut self) -> Result<(), String> {
        let src_dir = self.root.join("src");

        self.run_command("build_gpu_compute",
            Command::new("dotnet")
                .current_dir(&src_dir)
                .arg("build")
                .arg("GPU.Compute.fsproj"))?;

        let dll = src_dir.join("bin/GPU.Compute/Debug/net8.0/GPU.Compute.dll");
        if !dll.exists() {
            return Err("GPU.Compute.dll not found after build".to_string());
        }

        let hash = self.compute_sha256(&dll)?;
        self.metrics.record_artifact("gpu_compute", &hash);
        Ok(())
    }

    fn phase_server(&mut self) -> Result<(), String> {
        let src_dir = self.root.join("src");

        self.run_command("build_server",
            Command::new("dotnet")
                .current_dir(&src_dir)
                .arg("build")
                .arg("Server.fsproj"))?;

        let dll = src_dir.join("bin/Server/Debug/net8.0/Server.dll");
        if !dll.exists() {
            return Err("Server.dll not found after build".to_string());
        }

        let hash = self.compute_sha256(&dll)?;
        self.metrics.record_artifact("server", &hash);
        Ok(())
    }

    fn phase_cli(&mut self) -> Result<(), String> {
        let src_dir = self.root.join("src");

        self.run_command("build_cli",
            Command::new("dotnet")
                .current_dir(&src_dir)
                .arg("build")
                .arg("CLI.fsproj"))?;

        let dll = src_dir.join("bin/CLI/Debug/net8.0/CLI.dll");
        if !dll.exists() {
            return Err("CLI.dll not found after build".to_string());
        }

        let hash = self.compute_sha256(&dll)?;
        self.metrics.record_artifact("cli", &hash);
        Ok(())
    }

    fn phase_tests(&mut self, run_tests: bool) -> Result<(), String> {
        let tests_dir = self.root.join("tests");

        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S").to_string();
        let output_file = self.root.join(format!("docs/build_tests_{}.txt", timestamp));

        let output = Command::new("dotnet")
            .current_dir(&tests_dir)
            .arg("build")
            .arg("Tests.fsproj")
            .output()
            .map_err(|e| format!("Failed to execute build: {}", e))?;

        let stdout_str = String::from_utf8_lossy(&output.stdout);
        let stderr_str = String::from_utf8_lossy(&output.stderr);

        fs::write(&output_file, stdout_str.as_bytes()).map_err(|e| e.to_string())?;
        fs::write(output_file.with_extension("stderr"), stderr_str.as_bytes()).map_err(|e| e.to_string())?;

        if !output.status.success() {
            eprintln!("[{}] build_tests FAILED", Self::timestamp());
            eprintln!("[{}]   Error log: {}", Self::timestamp(), output_file.display());
            return Err("Tests build failed".to_string());
        }

        eprintln!("[{}] build_tests OK", Self::timestamp());

        if run_tests {
            self.run_command("run_tests",
                Command::new("dotnet")
                    .current_dir(&tests_dir)
                    .arg("test")
                    .arg("Tests.fsproj")
                    .arg("--no-build")
                    .arg("--verbosity").arg("normal"))?;
        }

        Ok(())
    }

    fn finalize(&self) {
        let total = self.start_time.elapsed().as_millis();
        let log_dir = self.root.join("logs");
        let _ = fs::create_dir_all(&log_dir);

        let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S").to_string();
        let telemetry_path = log_dir.join(format!("telemetry_{}.json", timestamp));

        if let Err(e) = self.metrics.save(&telemetry_path) {
            eprintln!("Warning: Failed to save telemetry: {}", e);
        }

        eprintln!("\n========== BUILD SUMMARY ==========");
        eprintln!("Total build time: {}ms", total);
        eprintln!("Telemetry: {}", telemetry_path.display());
        eprintln!("===================================\n");
    }
}

fn main() {
    eprintln!("[{}] ========== BUILD STARTING ==========", BuildContext::timestamp());

    let mut ctx = BuildContext::new();
    let run_tests = std::env::args().nth(1).as_deref() == Some("tests");

    let result = ctx.phase_cuda()
        .and_then(|_| ctx.phase_gpu_compute())
        .and_then(|_| ctx.phase_server())
        .and_then(|_| ctx.phase_cli())
        .and_then(|_| ctx.phase_tests(run_tests));

    ctx.finalize();

    if let Err(e) = result {
        eprintln!("Build failed: {}", e);
        std::process::exit(1);
    }

    eprintln!("[{}] ========== BUILD COMPLETE ==========", BuildContext::timestamp());
}
