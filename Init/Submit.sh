#!/bin/bash
#SBATCH -o /work/bm1164/m300832/Derwael/Init/Logs/SLURM_job.%j.%N.out
#SBATCH -e /work/bm1164/m300832/Derwael/Init/Logs/SLURM_job.%j.%N.err
#SBATCH -D /work/bm1164/m300832/Derwael/Init/
#SBATCH -J Init
#SBATCH --get-user-env
#SBATCH --account=bm1164
#SBATCH --mail-user=clara.henry@mpimet.mpg.de
#SBATCH --ntasks=80
#SBATCH --time=00:07:00
#SBATCH --partition=compute
#=================================================================================================================
spack load intel-oneapi-mpi@2021.5.0%intel@2021.5.0
#spack load intel-mpi@2019.10.317
spack load intel-oneapi-compilers@2022.0.1
spack load intel-oneapi-mkl@2022.0.1%gcc@11.2.0
spack load metis@5.1.0%intel@2021.5.0

export OMPI_MCA_pml="ucx"
export OMPI_MCA_btl=self
export OMPI_MCA_osc="pt2pt"
export UCX_IB_ADDR_TYPE=ib_global
# for most runs one may or may not want to disable HCOLL
export OMPI_MCA_coll="^ml,hcoll"
export OMPI_MCA_coll_hcoll_enable="0"
export HCOLL_ENABLE_MCAST_ALL="0"
export HCOLL_MAIN_IB=mlx5_0:1
export UCX_NET_DEVICES=mlx5_0:1
export UCX_TLS=mm,knem,cma,dc_mlx5,dc_x,self
export UCX_UNIFIED_MODE=y
export HDF5_USE_FILE_LOCKING=FALSE
export OMPI_MCA_io="romio321"
export UCX_HANDLE_ERRORS=bt

ulimit -s 102400
ulimit -c 0

set -e
echo Here comes the Nodelist:
echo $SLURM_JOB_NODELIST

echo Here comes the partition the job runs in:
echo $SLURM_JOB_PARTITION
cd $SLURM_SUBMIT_DIR

source ModulesPlusPaths2LoadIntelMPI.sh
export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi2.so
#export I_MPI_OFI_PROVIDER=mlx
#export I_MPI_FABRICS=shm:ofi

#make compile
#make ini
make grid
#mpirun -n 80 ElmerSolver_mpi
srun -l --mpi=pmi2 --export=ALL --cpu_bind=cores --distribution=block:cyclic -n 80 ElmerSolver_mpi Init.sif
