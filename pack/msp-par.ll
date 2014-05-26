# @ job_name = MSP_kp306410_1000x1000
# @ account_no = G52-5
# @ class = kdm-dev
# @ error = MSP_kp306410_1000x1000.err
# @ output = MSP_kp306410_1000x1000.out
# @ environment = COPY_ALL
# @ wall_clock_limit = 00:02:00
# @ notification = error
# @ notify_user = kp306410@students.mimuw.edu.pl
# @ job_type = bluegene
# @ bg_size = 8
# @ queue
mpirun -np 32 -mode VN ./msp-par.exe 1000 1000 1012
