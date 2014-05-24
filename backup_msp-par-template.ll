# @ job_name = MSP_kp306410_4000x4000
# @ account_no = G52-5
# @ class = kdm-dev
# @ error = MSP_kp306410_4000x4000.err
# @ output = MSP_kp306410_4000x4000.out
# @ environment = COPY_ALL
# @ wall_clock_limit = 00:02:00
# @ notification = error
# @ notify_user = kp306410@students.mimuw.edu.pl
# @ job_type = bluegene
# @ bg_size = 8
# @ queue
date >&2
echo "mpirun -np 32 -mode VN ./msp-par.exe 4000 4000 1012" 1>&2
mpirun -np 32 -mode VN ./msp-par.exe 4000 4000 1012
