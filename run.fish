for i in (seq 10 1000)
    echo $i > out.txt
    git add .
    git commit -a -m $i
end