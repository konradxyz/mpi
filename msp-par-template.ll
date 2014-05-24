# @ job_name = MSP_kp306410_4000x4000___PROCESSES_____MACHINES__
# @ account_no = G52-5
# @ class = kdm-dev
# @ error = MSP_kp306410_4000x4000___PROCESSES_____MACHINES__.err
# @ output = MSP_kp306410_4000x4000___PROCESSES_____MACHINES__.out
# @ environment = COPY_ALL
# @ wall_clock_limit = 00:__MINUTES__:00
# @ notification = error
# @ notify_user = kp306410@students.mimuw.edu.pl
# @ job_type = bluegene
# @ bg_size = __MACHINES__
# @ queue
date 1>&2
echo "mpirun -np __PROCESSES__ -mode __MODE__ ./msp-par.exe 4000 4000 1012" 1>&2
mpirun -np __PROCESSES__ -mode __MODE__ ./msp-par.exe 4000 4000 1012
