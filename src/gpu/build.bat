@echo off
setlocal

REM Set up MSVC environment
set "MSVC_PATH=C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Tools\MSVC\14.44.35207"
set "SDK_PATH=C:\Program Files (x86)\Windows Kits\10"
set "PATH=%MSVC_PATH%\bin\Hostx64\x64;%PATH%"
set "INCLUDE=%MSVC_PATH%\include;%SDK_PATH%\Include\10.0.22621.0\ucrt;%SDK_PATH%\Include\10.0.22621.0\um;%SDK_PATH%\Include\10.0.22621.0\shared"
set "LIB=%MSVC_PATH%\lib\x64;%SDK_PATH%\Lib\10.0.22621.0\ucrt\x64;%SDK_PATH%\Lib\10.0.22621.0\um\x64"

echo Compiling CUDA kernels...

REM Compile each kernel to object file
nvcc -c -o parallel_tempering.obj parallel_tempering.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

nvcc -c -o collision_detection.obj collision_detection.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

nvcc -c -o verlet_integration.obj verlet_integration.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

nvcc -c -o marching_cubes.obj marching_cubes.cu mc_tables.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

nvcc -c -o hydraulic_erosion.obj hydraulic_erosion.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

nvcc -c -o convergence.obj convergence.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

nvcc -c -o mc_tables_texture.obj mc_tables_texture.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

nvcc -c -o noise_field.obj noise_field.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

nvcc -c -o spring_forces.obj spring_forces.cu -arch=sm_86
if %errorlevel% neq 0 exit /b %errorlevel%

echo Linking shared library...

REM Link into shared DLL
nvcc -shared -o libgpu_tempering.dll ^
    parallel_tempering.obj ^
    collision_detection.obj ^
    verlet_integration.obj ^
    marching_cubes.obj ^
    hydraulic_erosion.obj ^
    convergence.obj ^
    mc_tables_texture.obj ^
    noise_field.obj ^
    spring_forces.obj ^
    -lcudart

if %errorlevel% neq 0 exit /b %errorlevel%

echo Build successful!
echo Output: libgpu_tempering.dll

REM Clean up object files
del *.obj

endlocal
