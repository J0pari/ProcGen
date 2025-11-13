use std::process::{Command, Stdio};
use std::path::{Path, PathBuf};
use std::fs::{self, OpenOptions};
use std::io::Write;
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

    fn record_phase(&mut self, phase: &str, start_time: u128, duration: u128) {
        self.events.push((format!("{}_start", phase), start_time));
        self.events.push((format!("{}_end", phase), start_time + duration));
        self.durations.insert(phase.to_string(), duration);
    }

    fn record_artifact(&mut self, name: &str, hash: &str) {
        self.artifacts.insert(name.to_string(), hash.to_string());
    }

    fn save(&self, path: &Path) -> std::io::Result<()> {
        let json = serde_json::json!({
            "events": self.events,
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
    build_log: PathBuf,
    build_timestamp: String,
}

impl BuildContext {
    fn new() -> Self {
        let mut root = std::env::current_dir().unwrap();
        if root.ends_with("scripts") {
            root = root.parent().unwrap().to_path_buf();
        }

        let build_timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S").to_string();
        let docs_dir = root.join("docs");
        let _ = fs::create_dir_all(&docs_dir);

        let build_log = docs_dir.join(format!("build_{}.txt", build_timestamp));

        BuildContext {
            root,
            metrics: BuildMetrics::new(),
            start_time: Instant::now(),
            build_log,
            build_timestamp,
        }
    }

    fn run_command(&mut self, name: &str, cmd: &mut Command) -> Result<(), String> {
        use std::io::{BufRead, BufReader};
        use std::sync::{Arc, Mutex};
        use std::thread;

        let phase_marker = format!("\n========== {} ==========\n", name.to_uppercase());
        let mut log_file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.build_log)
            .map_err(|e| format!("Failed to open build log: {}", e))?;
        log_file.write_all(phase_marker.as_bytes()).map_err(|e| e.to_string())?;

        eprintln!("[{}] {} →", Self::timestamp(), name);
        let phase_start = Instant::now();
        let start_time = self.start_time.elapsed().as_millis();

        let mut child = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("Failed to spawn: {}", e))?;

        let stdout_lines = Arc::new(Mutex::new(Vec::new()));
        let stderr_lines = Arc::new(Mutex::new(Vec::new()));

        let stdout_thread = {
            let lines = Arc::clone(&stdout_lines);
            child.stdout.take().map(|stdout| {
                thread::spawn(move || {
                    let reader = BufReader::new(stdout);
                    for line in reader.lines().flatten() {
                        eprintln!("[{}]   {}", BuildContext::timestamp(), line);
                        lines.lock().unwrap().push(line);
                    }
                })
            })
        };

        let stderr_thread = {
            let lines = Arc::clone(&stderr_lines);
            child.stderr.take().map(|stderr| {
                thread::spawn(move || {
                    let reader = BufReader::new(stderr);
                    for line in reader.lines().flatten() {
                        eprintln!("[{}]   {}", BuildContext::timestamp(), line);
                        lines.lock().unwrap().push(line);
                    }
                })
            })
        };

        if let Some(t) = stdout_thread {
            let _ = t.join();
        }
        if let Some(t) = stderr_thread {
            let _ = t.join();
        }

        let status = child.wait().map_err(|e| format!("Failed to wait: {}", e))?;

        let duration = phase_start.elapsed().as_millis();
        self.metrics.record_phase(name, start_time, duration);

        let stdout_content = stdout_lines.lock().unwrap().join("\n");
        let stderr_content = stderr_lines.lock().unwrap().join("\n");

        log_file.write_all(stdout_content.as_bytes()).map_err(|e| e.to_string())?;
        if !stderr_content.is_empty() {
            log_file.write_all(b"\n--- STDERR ---\n").map_err(|e| e.to_string())?;
            log_file.write_all(stderr_content.as_bytes()).map_err(|e| e.to_string())?;
        }

        let ts = Self::timestamp();
        if !status.success() {
            eprintln!("[{}] {} ✗ ({}ms)", ts, name, duration);
            eprintln!("[{}]   Error log: {}", ts, self.build_log.display());
            return Err(format!("{} failed", name));
        }

        eprintln!("[{}] {} ✓ ({}ms)", ts, name, duration);
        Ok(())
    }

    fn timestamp() -> String {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        let dt = chrono::DateTime::<chrono::Utc>::from_timestamp(now as i64, 0).unwrap();
        dt.format("%H:%M:%S").to_string()
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

    fn build_and_record(&mut self, phase_name: &str, project_file: &str, dll_path: &Path, artifact_name: &str) -> Result<(), String> {
        let src_dir = self.root.join("src");

        self.run_command(phase_name,
            Command::new("dotnet")
                .current_dir(&src_dir)
                .arg("build")
                .arg(project_file))?;

        if !dll_path.exists() {
            return Err(format!("{} not found after build", dll_path.display()));
        }

        let hash = self.compute_sha256(dll_path)?;
        self.metrics.record_artifact(artifact_name, &hash);
        Ok(())
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

        eprintln!("[{}]   Compiling 9 CUDA kernels", Self::timestamp());

        #[cfg(target_os = "windows")]
        {
            let src_dir = self.root.join("src");
            let batch_path = src_dir.join("cuda_build_temp.bat");

            let batch_content = r#"@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" x64
nvcc -shared -o libgpu_compute.so gpu/parallel_tempering.cu gpu/convergence.cu gpu/hydraulic_erosion.cu gpu/marching_cubes.cu gpu/verlet_integration.cu gpu/collision_detection.cu gpu/noise_field.cu gpu/spring_forces.cu gpu/mc_tables.cu -arch=sm_75 -O3 --use_fast_math -std=c++17 -I gpu
"#;

            fs::write(&batch_path, batch_content).map_err(|e| e.to_string())?;

            let mut cmd = Command::new(&batch_path);
            cmd.current_dir(&src_dir);

            let result = self.run_command("cuda_build", &mut cmd);

            let _ = fs::remove_file(&batch_path);

            result
        }

        #[cfg(not(target_os = "windows"))]
        {
            let mut cmd = Command::new("nvcc");
            cmd.current_dir(self.root.join("src"))
                .arg("-shared")
                .arg("-o").arg("libgpu_compute.so")
                .arg("gpu/parallel_tempering.cu")
                .arg("gpu/convergence.cu")
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
                .arg("-I").arg("gpu")
                .arg("-Xcompiler").arg("-fPIC");

            self.run_command("cuda_build", &mut cmd)
        }
    }

    fn phase_core(&mut self) -> Result<(), String> {
        let dll = self.root.join("src/bin/Debug/net8.0/Core.dll");
        self.build_and_record("build_core", "Core.fsproj", &dll, "core")
    }

    fn phase_execution(&mut self) -> Result<(), String> {
        let dll = self.root.join("src/bin/Debug/net8.0/Execution.dll");
        self.build_and_record("build_execution", "Execution.fsproj", &dll, "execution")
    }

    fn phase_gpu_interop(&mut self) -> Result<(), String> {
        let dll = self.root.join("src/bin/Debug/net8.0/GPU.Interop.dll");
        self.build_and_record("build_gpu_interop", "GPU.Interop.fsproj", &dll, "gpu_interop")
    }

    fn phase_physics(&mut self) -> Result<(), String> {
        let dll = self.root.join("src/bin/Debug/net8.0/Physics.dll");
        self.build_and_record("build_physics", "Physics.fsproj", &dll, "physics")
    }

    fn phase_terrain(&mut self) -> Result<(), String> {
        let dll = self.root.join("src/bin/Debug/net8.0/Terrain.dll");
        self.build_and_record("build_terrain", "Terrain.fsproj", &dll, "terrain")
    }

    fn phase_server(&mut self) -> Result<(), String> {
        let dll = self.root.join("src/bin/Debug/net8.0/Server.dll");
        self.build_and_record("build_server", "Server.fsproj", &dll, "server")
    }

    fn phase_cli(&mut self) -> Result<(), String> {
        let dll = self.root.join("src/bin/Debug/net8.0/CLI.dll");
        self.build_and_record("build_cli", "CLI.fsproj", &dll, "cli")
    }

    fn phase_tests(&mut self, run_tests: bool) -> Result<(), String> {
        let tests_dir = self.root.join("tests");

        self.run_command("build_tests",
            Command::new("dotnet")
                .current_dir(&tests_dir)
                .arg("build")
                .arg("Tests.fsproj"))?;

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

        let telemetry_path = log_dir.join(format!("telemetry_{}.json", self.build_timestamp));

        if let Err(e) = self.metrics.save(&telemetry_path) {
            eprintln!("Warning: Failed to save telemetry: {}", e);
        }

        eprintln!("\n========== BUILD SUMMARY ==========");
        eprintln!("Total build time: {}ms", total);
        eprintln!("Build log: {}", self.build_log.display());
        eprintln!("Telemetry: {}", telemetry_path.display());
        eprintln!("===================================\n");
    }
}

fn main() {
    eprintln!("[{}] ========== BUILD STARTING ==========", BuildContext::timestamp());

    let mut ctx = BuildContext::new();
    let run_tests = std::env::args().nth(1).as_deref() == Some("tests");

    let result = ctx.phase_cuda()
        .and_then(|_| ctx.phase_core())
        .and_then(|_| ctx.phase_execution())
        .and_then(|_| ctx.phase_gpu_interop())
        .and_then(|_| ctx.phase_physics())
        .and_then(|_| ctx.phase_terrain())
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
