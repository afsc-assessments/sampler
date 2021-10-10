awk -F, -v y=$1 -f '{print $0>age$1 poll_age.csv >age$1.csv
echo done ages for $1
awk -F, -v  -f '{y=$1;print $0 >>y"catch.csv"}  akfincat.csv 
>catch$1.csv
echo done catch for $1
awk -F, -v y=$1 -f a1.awk poll_len.csv >len$1.csv
echo done lengths for $1
