for i in (seq 1 10)
    echo $i > out.txt
    git add .
    git commit -a -m $i
end